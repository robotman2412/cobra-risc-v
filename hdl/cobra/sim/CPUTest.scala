package cobra.sim

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra._
import cobra.cpu._
import cobra.cpu.backend._
import cobra.cpu.execute._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._

import java.io.File
import java.nio.file.Files
import scala.io._

case class CPUBench(cfg: CobraCfg) extends Component {
    val io = new Bundle {
    }
    
    val cpu  = CobraCPU(cfg)
    val irom = new AhbLite3OnChipRom(AhbLite3Config(cfg.vaddrWidth, 32), readRom())
    cpu.io.ibus.toAhbLite3 <> irom.io.ahb
    
    def readRom(): Seq[Bits] = {
        var raw = Files.readAllBytes(new File("./prog/test/build/rom.bin").toPath)
        var tmp = Seq[Bits]()
        for (i <- 0 until raw.length / 4) {
            val packed  = ((raw(i*4+0) & 255)
                        | ((raw(i*4+1) & 255) << 8)
                        | ((raw(i*4+2) & 255) << 16)
                        | ((raw(i*4+3) & 255) << 24))
            println(f"$packed%08x\n")
            tmp = tmp :+ B(packed & 0xffffffffl, 32 bits)
        }
        return tmp
    }
}

object CPUTest extends App {
    Config.sim.compile(
        CPUBench(
            CobraCfg(ISA"RV32I", entrypoint=0x40000000)
        )
    ).doSim(this.getClass.getSimpleName) { dut =>
        // Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)

        // Wait a rising edge on the clock
        for (i <- 0 until 100) {
            dut.clockDomain.waitSampling()
        }
    }
}