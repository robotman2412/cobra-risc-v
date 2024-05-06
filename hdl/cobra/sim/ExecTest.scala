package cobra.sim

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra._
import cobra.cpu._
import cobra.cpu.decode.InsnDecoder
import cobra.cpu.decode.Regfile
import cobra.cpu.execution._
import cobra.cpu.execution.unit._
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
    val fireIssue = io.din.ready && io.din.valid
    val fireCommit = Bool()
    val order   = RegInit(U(0, cfg.orderBits bits))
    val decoder = InsnDecoder(cfg)
    decoder.io.decomp := io.din.payload.insn
    io.din.ready := True
    val regfile = Regfile(cfg, cfg.XLEN)
    val complex = SingleIssueExec(cfg, List[CobraCfg => ExecUnit](
        cfg => ALU(cfg),
        cfg => MUL.Simple(cfg)
    ))
    complex.io.dout.ready       := True
    regfile.io.write(0).we      := complex.io.dout.valid && complex.io.dout.payload.we
    regfile.io.write(0).regno   := complex.io.dout.payload.regno
    regfile.io.write(0).data    := complex.io.dout.payload.data
    complex.io.reg <> regfile.io.read
    complex.io.decd.valid         := io.din.valid
    complex.io.decd.payload.order := order
    complex.io.decd.payload.insn  := decoder.io.decd
    complex.io.decd.payload.pc.assignDontCare()
    when (fireIssue) {
        order := order + U(1, cfg.orderBits bits)
    }
}

object ExecTest extends App {
    Config.sim.compile(ExecTB(CobraCfg(ISA"RV32IM"))).doSim(this.getClass.getSimpleName) { dut =>
        // Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)
        
        // Instruction stream.
        val istream = Seq(
            0xffd30293l, // addi x5, x6, -3
            0x023100b3l, // mul  x1, x2, x3
            0x0020c0b3l, // xor  x1, x1, x2
            0x007454b3l, // srl  x9, x8, x7
            0x00128293l, // addr x5, x5,  1
        )
        
        // Init regfile.
        dut.regfile.write(6, 0x10000003l)
        dut.regfile.write(2, 0x00001010l)
        dut.regfile.write(3, 0x0000003fl)
        dut.regfile.write(8, 0xffff0000l)
        dut.regfile.write(7, 0x00000005l)
        
        // Send it all to the DUT.
        for (data <- istream) {
            dut.clockDomain.waitSampling()
            dut.io.din.valid #= true
            dut.io.din.payload.insn #= data
            while (!dut.io.din.ready.toBoolean) {
                dut.clockDomain.waitSampling()
            }
        }
        dut.clockDomain.waitSampling()
        dut.io.din.valid #= false
        
        // Wait another couple cycles.
        for (_ <- 0 to 9) dut.clockDomain.waitSampling()
    }
}