package cobra.sim

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra._
import cobra.cpu._
import spinal.core._
import spinal.core.sim._
import cobra.cpu.misc.Regfile

object RegTest extends App {
    Config.sim.compile(Regfile(CobraCfg(), 32)).doSim(this.getClass.getSimpleName) { dut =>
        // Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)

        for (idx <- 0 to 99) {
            // Drive the dut inputs with random values
            dut.io.write(0).randomize()
            dut.io.read(0).randomize()
            dut.io.read(1).randomize()

            // Wait a rising edge on the clock
            dut.clockDomain.waitSampling()
        }
    }
}