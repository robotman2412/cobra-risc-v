package cobra.sim

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra._
import cobra.cpu._
import cobra.cpu.id._
import spinal.core._
import spinal.core.sim._

object RegTest extends App {
    Config.sim.compile(Regfile(CobraCfg())).doSim(this.getClass.getSimpleName) { dut =>
        // Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)

        for (idx <- 0 to 99) {
            // Drive the dut inputs with random values
            dut.io.waddr.randomize()
            dut.io.we.randomize()
            dut.io.raddr_a #= 0

            // Wait a rising edge on the clock
            dut.clockDomain.waitSampling()
        }
    }
}