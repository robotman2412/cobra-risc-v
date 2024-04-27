package cobra.sim

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra._
import cobra.cpu._
import cobra.cpu.id._
import spinal.core._
import spinal.core.sim._

object WBTest extends App {
    Config.sim.compile(WriteBack(CobraCfg(), 4, SInt(32 bits))).doSim(this.getClass.getSimpleName) { dut =>
        // Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)

        // Drive the dut inputs with random values
        dut.io.din(0).valid #= false
        dut.io.din(1).valid #= false
        dut.io.din(2).valid #= false
        dut.io.din(3).valid #= false

        // Wait a rising edge on the clock
        dut.clockDomain.waitSampling()
        
        // Produce some OoO results.
        dut.io.din(2).valid         #= true
        dut.io.din(2).payload.order #= 1
        dut.io.din(2).payload.regno #= 3
        dut.io.din(2).payload.data  #= 0xdeadbeef
        dut.clockDomain.waitSampling()
        
        dut.io.din(1).valid         #= true
        dut.io.din(1).payload.order #= 0
        dut.io.din(1).payload.regno #= 9
        dut.io.din(1).payload.data  #= 0xcafebabe
        dut.clockDomain.waitSampling()
        
        dut.io.din(1).valid         #= false
        dut.clockDomain.waitSampling()
        
        dut.io.din(2).valid         #= false
        dut.clockDomain.waitSampling()
    }
}