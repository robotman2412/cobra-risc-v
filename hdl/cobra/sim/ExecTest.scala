package cobra.sim

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra._
import cobra.cpu._
import cobra.cpu.frontend.InsnDecoder
import cobra.cpu.frontend.Regfile
import spinal.core._
import spinal.core.sim._
import spinal.lib._

case class ExecTestData(cfg: CobraCfg) extends Bundle {
    val insn = Bits(32 bits)
    val lhs  = Bits(cfg.XLEN bits)
    val rhs  = Bits(cfg.XLEN bits)
}

case class ExecTB(cfg: CobraCfg) extends Component {
    val io = new Bundle {
        // Testbench input data.
        val din     = slave  Stream(ExecTestData(cfg))
        // // Regfile write port.
        // val commit  = out port Bool()
        // val waddr   = out port UInt(5 bits)
        // val dout    = out port Bits(cfg.XLEN bits)
        // // Which instruction is about to be committed.
        // val next    = out port UInt(cfg.orderBits bits)
    }
    val decoder = InsnDecoder(cfg)
    decoder.io.decomp := io.din.payload.insn
    io.din.ready := True
}

object ExecTest extends App {
    Config.sim.compile(ExecTB(CobraCfg())).doSim(this.getClass.getSimpleName) { dut =>
        // Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)
        
        // Instruction stream.
        val istream = Seq(
            0xffd30293l, // addi x5, x6, -3
            0x023100b3l, // mul  x1, x2, x3
            0x0020c0b3l, // xor  x1, x1, x2
            0x007454b3l, // srl  x9, x8, x7
        )
        
        // Send it all to the DUT.
        for (data <- istream) {
            dut.clockDomain.waitSampling()
            dut.io.din.valid #= true
            dut.io.din.payload.insn #= data
            while (!dut.io.din.ready.toBoolean) {
                dut.clockDomain.waitSampling()
            }
        }
        
        // Wait another couple cycles.
        for (_ <- 0 to 9) dut.clockDomain.waitSampling()
    }
}