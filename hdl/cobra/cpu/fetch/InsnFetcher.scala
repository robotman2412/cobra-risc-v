package cobra.cpu.fetch

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra.cpu._
import cobra.cpu.mem._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._

/**
 * Instruction fetching pipeline.
 * `dout(1)` is never valid if `dout(2)` is not valid, and `dout(1)` must not be ready if `dout(2)` is not ready but is valid.
 * 
 * The fetch unit works in two stages:
 * - A 128-bit ringbuffer containing fetch packets.
 * - Two multiplexer groups that read instruction bits from the ring buffer.
 * 
 * The ringbuffer's address is aligned to 64 bits, so it contains up to two packets including their trap responses.
 * A 3-bit read index controls where the multiplexers start reading,
 * and a single write index bit controls which half is refilled with the next fetch packet.
 * The logic controlling the multiplexers is also responsible for determining how many halfwords were used,
 * and the ringbuffer logic uses that to update the indices and fetch new bytes.
 */
case class InsnFetcher(cfg: CobraCfg) extends Component {
    val io = new Bundle {
        /** Program memory interface. */
        val ibus    = master port IntMemBus(cfg, false)
        /** Fetched instructions. */
        val dout    = Vec.fill(2)(master port Stream(FetchedInsn(cfg)))
    }
    
    /** How much to consume this cycle. */
    val consume     = UInt(3 bits)
    /** How much is available this cycle. */
    val available   = UInt(3 bits)
    
    /* ==== FIRST STAGE: FETCH RINGBUFFER ==== */
    
    /** Which half of the ringbuffer is to be refilled by the current memory response. */
    val writeIndex      = RegInit(U(0, 1 bits))
    /** A fetch cycle is in progress. */
    val writeActive     = RegInit(False)
    /** Ringbuffer read index in 16-bit increments. */
    val readIndex       = RegInit(U((cfg.entrypoint >> 1) & 3, 3 bits))
    /** Trigger a new fetch cycle. */
    val fetchTrigger    = Bool()
    /** Next address to fetch from. */
    val pc              = RegInit(U(cfg.entrypoint & ~7l, cfg.XLEN bits))
    
    /** Fetch packet ringbuffer with forwarding. */
    val fetchPacket     = Vec.fill(2)(FetchPacket(cfg))
    /** Buffer valid state with forwarding. */
    val fetchValid      = Bits(2 bits)
    /** Fetch packet ringbuffer. */
    val fetchPacketBuf  = RegNext(fetchPacket)
    /** Buffer valid state. */
    val fetchValidBuf   = Reg(Bits(2 bits), B(0), fetchValid)
    
    fetchValid      := fetchValidBuf
    fetchPacket     := fetchPacketBuf
    io.ibus.enable  := False
    io.ibus.exec    := True
    io.ibus.priv    := U"11" // TODO.
    io.ibus.pgEn    := False // TODO.
    io.ibus.addr    := pc
    io.ibus.asize   := U(3)
    
    // Memory response receiver.
    when (writeActive) {
        fetchValid(~writeIndex) := io.ibus.ready
        when (io.ibus.ready) {
            writeActive := False
        }
        fetchPacket(~writeIndex).data   := io.ibus.rdata
        fetchPacket(~writeIndex).trap   := io.ibus.trap
        fetchPacket(~writeIndex).cause  := io.ibus.cause
    }
    
    // Memory request driver.
    when (fetchTrigger) {
        // (Re-)trigger fetch cycle.
        io.ibus.enable  := True
        when (io.ibus.ready) {
            writeActive     := True
            writeIndex      := ~writeIndex
            pc              := pc + U(8)
            fetchPacketBuf(writeIndex).addr := pc
        }
    }
    
    // FIFO consumption logic.
    val readNext     = readIndex + consume
    readIndex       := readNext
    fetchTrigger    := fetchValid =/= B"11" || readNext(2) =/= readIndex(2)
    when (readNext(2) =/= readIndex(2)) {
        // Cancel validity for next cycle when a packet will be fully consumed this cycle.
        fetchValidBuf(readIndex >> 2) := False
    }
    
    // FIFO availability logic.
    val hasCur   = fetchValid(readIndex >> 2)
    val hasNext  = fetchValid(~readIndex >> 2)
    when (hasCur && hasNext) {
        available := 4
    } elsewhen (hasCur) {
        available := U(4) - readIndex(1 downto 0)
    } otherwise {
        available := U(0)
    }
    
    /* ==== SECOND STAGE: OUTPUT MULTIPLEXERS ==== */
    
    /** Instruction data laid out as a single ring buffer. */
    val flatRing    = Vec.fill(8)(Bits(16 bits))
    /** Start index of the instructions within `flatRing`. */
    val insnStart   = Vec.fill(2)(UInt(3 bits))
    /** Whether the instructions are 32-bit. */
    val isLong      = Vec.fill(2)(Bool())   // Bits(2 bits) would generate a combinatorial loop error.
    
    for (i <- 0 until 4) {
        flatRing(i)   := fetchPacket(0).data(i*16+15 downto i*16)
        flatRing(i+4) := fetchPacket(1).data(i*16+15 downto i*16)
    }
    
    // Available / consume handshaking and validity logic.
    io.dout(0).valid := U(1, 3 bits) + isLong(0).asUInt <= available
    io.dout(1).valid := U(2, 3 bits) + isLong(0).asUInt + isLong(1).asUInt <= available
    consume := U(0)
    when (io.dout(1).fire) {
        consume := U(2, 3 bits) + isLong(0).asUInt + isLong(1).asUInt
    } elsewhen (io.dout(0).fire) {
        consume := U(1, 3 bits) + isLong(0).asUInt
    }
    
    // Instuction extraction.
    insnStart(0)    := readIndex;
    insnStart(1)    := readIndex + isLong(0).asUInt.resized + U(1)
    for (i <- 0 until 2) {
        isLong(i)   := flatRing(insnStart(i))(1 downto 0) === B"11"
        io.dout(i).payload.raw(15 downto 0)  := flatRing(insnStart(i))
        io.dout(i).payload.raw(31 downto 16) := flatRing(insnStart(i) + 1)
        when (!isLong(i)) {
            io.dout(i).payload.raw(31 downto 16) := B(0)
        }
    }
    
    // Instruction address/trap logic.
    for (i <- 0 until 2) {
        val spansPackets = isLong(i) && insnStart(i) === M"-11"
        val firstPacket = insnStart(i)(2).asUInt
        io.dout(i).payload.addr             := fetchPacket(firstPacket).addr
        io.dout(i).payload.addr(2 downto 1) := insnStart(i)(1 downto 0)
        io.dout(i).payload.trap             := fetchPacket(firstPacket).trap
        io.dout(i).payload.cause            := fetchPacket(firstPacket).cause
        when (spansPackets && !fetchPacket(firstPacket).trap && fetchPacket(~firstPacket).trap) {
            // Traps on second half of instruction.
            io.dout(i).payload.trap     := True
            io.dout(i).payload.cause    := fetchPacket(~firstPacket).cause
            io.dout(i).payload.addr     := fetchPacket(~firstPacket).addr
        }
        io.dout(i).payload.addr(0) := False
    }
}
