package cobra.sim

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra._
import cobra.cpu._
import cobra.cpu.backend.Result
import cobra.cpu.backend.ResultBuffer
import cobra.cpu.backend.WriteBack
import spinal.core._
import spinal.core.sim._
import spinal.lib._

case class WBBench[T <: Data](cfg: CobraCfg, dtype: HardType[T]) extends Component {
    val io = new Bundle {
        // Unordered instruction streams.
        val din_a   = slave Stream(Result(cfg, dtype()))
        val din_b   = slave Stream(Result(cfg, dtype()))
        // Regfile write port.
        val commit  = out port Bool()
        val waddr   = out port UInt(5 bits)
        val dout    = out port dtype()
        // Which instruction is about to be committed.
        val next    = out port UInt(cfg.orderBits bits)
    }
    val wb  = new WriteBack(cfg, 2, dtype())
    val buf = new ResultBuffer(cfg, dtype())
    buf.io.din   << io.din_a
    wb.io.din(0) << buf.io.dout
    wb.io.din(1) << io.din_b
    io.commit := wb.io.commit
    io.waddr  := wb.io.waddr
    io.dout   := wb.io.dout
    io.next   := wb.io.next
}

object WBTest extends App {
    Config.sim.compile(WBBench(CobraCfg(), SInt(32 bits))).doSim(this.getClass.getSimpleName) { dut =>
        // Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)

        // Drive the dut inputs with random values
        dut.io.din_a.valid #= false
        dut.io.din_b.valid #= false

        // Wait a rising edge on the clock
        dut.clockDomain.waitSampling()
        
        // Produce some OoO results.
        dut.io.din_a.valid         #= true
        dut.io.din_a.payload.order #= 1
        dut.io.din_a.payload.regno #= 3
        dut.io.din_a.payload.data  #= 0xdeadbeef
        dut.clockDomain.waitSampling()
        
        dut.io.din_b.valid         #= true
        dut.io.din_b.payload.order #= 0
        dut.io.din_b.payload.regno #= 9
        dut.io.din_b.payload.data  #= 0xcafebabe
        dut.io.din_a.payload.order #= 2
        dut.io.din_a.payload.regno #= 3
        dut.io.din_a.payload.data  #= 0xf00dbabe
        dut.clockDomain.waitSampling()
        
        dut.io.din_b.valid         #= false
        dut.clockDomain.waitSampling()
        
        dut.io.din_a.valid         #= false
        dut.clockDomain.waitSampling()
        dut.clockDomain.waitSampling()
        dut.clockDomain.waitSampling()
    }
}