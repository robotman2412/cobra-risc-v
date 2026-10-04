package cobra.sim

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra._
import cobra.cpu._
import cobra.cpu.execute._
import spinal.core._
import spinal.core.sim._
import spinal.lib._



object AluTest extends App {
    Config.sim.compile(Alu(CobraCfg(ISA"RV32IM"))).doSim(this.getClass.getSimpleName) { dut =>
        // Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)
        
        // Wait another couple cycles.
        dut.clockDomain.waitSampling()
    }
}
