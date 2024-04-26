package cobra.sim

import spinal.core._
import spinal.core.sim._
import cobra._

object RegTest extends App {
  Config.sim.compile(Regfile(32)).doSim(this.getClass.getSimpleName) { dut =>
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