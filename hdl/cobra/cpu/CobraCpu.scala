package cobra.cpu

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra.cpu.decode._
import cobra.cpu.execute._
import cobra.cpu.fetch._
import cobra.cpu.misc._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import spinal.core.sim.SimBitVectorPimper
import spinal.core.sim.SimBoolPimper
import spinal.core.sim.SimDataPimper



/**
 * Cobra RISC-V CPU.
 */
case class CobraCpu(
    // CPU features and other parameters.
    cfg:    CobraCfg,
) extends Component {
    val io = new Bundle {
        /** Data bus. */
        // val dbus = master port AhbLite3Master(AhbLite3Config(cfg.paddrWidth, cfg.XLEN))
        /** Instruction bus. */
        val ibus = master port AhbLite3Master(AhbLite3Config(cfg.paddrWidth, 64))
    }
    
    val fetch   = InsnFetcher(cfg)
    val decd    = DecodeStage(cfg)
    val alu0    = AluStage(cfg)
    val alu1    = AluStage(cfg)
    val delay0  = DelayStage(cfg, 4, false, false, false, false)
    val delay1  = DelayStage(cfg, 4, false, false, false, false)
    
    // Pipeline connections.
    decd.io.din   <> fetch.io.dout
    alu0.io.din   << decd.io.dout(0)
    alu1.io.din   << decd.io.dout(1)
    delay0.io.din << alu0.io.dout
    delay1.io.din << alu1.io.dout
    
    // Misc connections.
    fetch.io.branchTrigger := False
    fetch.io.branchTarget := S(0, cfg.badVaddrWidth bits)
    
    val ahb = fetch.io.ibus.toAhb3Master
    io.ibus.HADDR       := ahb.HADDR.resized    // Temporary connection until the L1 caches exist.
    io.ibus.HWRITE      := ahb.HWRITE
    io.ibus.HSIZE       := ahb.HSIZE
    io.ibus.HBURST      := ahb.HBURST
    io.ibus.HPROT       := ahb.HPROT
    io.ibus.HTRANS      := ahb.HTRANS
    io.ibus.HMASTLOCK   := ahb.HMASTLOCK
    io.ibus.HWDATA      := ahb.HWDATA
    ahb.HREADY          := io.ibus.HREADY
    ahb.HRESP           := io.ibus.HRESP
    ahb.HRDATA          := io.ibus.HRDATA
    
    for ((write, result) <- decd.io.regWrite.zip(Seq(delay0.io.dout, delay1.io.dout))) {
        write := result.payload.toRegWrite()
        when (!result.valid) {
            write.we := False
        }
    }
    
    // TODO: Stalling logic.
    decd  .io.stall := False
    alu0  .io.stall := False
    alu1  .io.stall := False
    delay0.io.stall := False
    delay1.io.stall := False
    
    // Simulation helpers.
    /** Read committed register value. */
    def readRegister(regno: Int) = decd.regfile.read(regno)
    /** Read committed register value or value to be committed next cycle. */
    def readRegisterForwarded(regno: Int) = decd.regfile.readForwarded(regno)
    /** Write register file (does not apply to in-flight instructions). */
    def writeRegister(regno: Int, value: BigInt) = decd.regfile.write(regno, value)
    /** Get the program counter of the next instruction that would be fetched. */
    def getFetchPC() = fetch.io.dout(0).payload.addr.toLong
    /** Get the program counter of the instructions being committed this cycle. */
    def getCommitPC(): Seq[Long] = {
        if (delay0.io.dout.valid.toBoolean && delay1.io.dout.valid.toBoolean) {
            Seq(
                delay0.io.dout.payload.pc.toLong,
                delay1.io.dout.payload.pc.toLong,
            )
        } else if (delay1.io.dout.valid.toBoolean) {
            Seq(delay1.io.dout.payload.pc.toLong)
        } else if (delay0.io.dout.valid.toBoolean) {
            Seq(delay0.io.dout.payload.pc.toLong)
        } else {
            Seq()
        }
    }
    
    delay0.io.dout.simPublic()
    delay1.io.dout.simPublic()
}
