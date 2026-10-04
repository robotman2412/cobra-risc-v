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
 * `dout1` is never valid if `dout2` is not valid, and `dout1` must not be ready if `dout2` is not ready but is valid.
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
        val ibus        = master port IntMemBus(cfg, false)
        /** First fetched instruction. */
        val dout0       = master port Stream(FetchedInsn(cfg))
        /** Second fetched instruction. */
        val dout1       = master port Stream(FetchedInsn(cfg))
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
    val readIndex       = RegInit(U(0, 3 bits))
    /** Trigger a new fetch cycle. */
    val fetchTrigger    = Bool()
    /** Next address to fetch from. */
    val pc              = RegInit(U(cfg.entrypoint, cfg.XLEN bits))
    
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
    fetchTrigger    := fetchValid =/= B"11" || (readNext >> 2) =/= (readIndex >> 2)
    readIndex       := readNext
    
    // FIFO availability logic.
    val hasCur   = fetchValid(readIndex >> 2)
    val hasNext  = fetchValid(~readIndex >> 2)
    when (hasCur && hasNext) {
        available := 4
    } elsewhen (hasCur) {
        available := ~readIndex & U"011"
    } otherwise {
        available := U(0)
    }
    
    /* ==== SECOND STAGE: OUTPUT MULTIPLEXERS ==== */
    consume := U(0)
    io.dout0.valid := False
    io.dout0.payload.assignDontCare
    io.dout1.valid := False
    io.dout1.payload.assignDontCare
}
