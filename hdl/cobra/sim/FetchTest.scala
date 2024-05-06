package cobra.sim

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra._
import cobra.cpu._
import cobra.cpu.fetch._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._


case class FetchTB(cfg: CobraCfg) extends Component {
    val io = new Bundle {
        val done = out port Bool()
    }
    
    /** Expected instruction stream. */
    val expected = Seq(
        0xaaaa0003,
        0xbbb0,
        0xccc0,
        0xddd0,
        0xeeee0003,
        0xfff0,
    )
    /** Pack a sequence instructions into a list of bits constants. */
    private def packBits(raw: Seq[Int]): Seq[Bits] = {
        var tmp    = List[Long]()
        for (item <- raw) {
            if ((item & 3) == 3) {
                tmp = tmp :+ (item & 0xffff).toLong
                tmp = tmp :+ ((item >> 16) & 0xffff).toLong
            } else {
                tmp = tmp :+ item.toLong
            }
        }
        var packed = List[Bits]()
        for (i <- 0 until tmp.length / 2) {
            packed = packed :+ B(tmp(i*2+1) << 16 | tmp(i*2), 32 bits)
        }
        if ((tmp.length & 1) != 0) {
            packed = packed :+ B(tmp(tmp.length-1), 32 bits)
        }
        return packed
    }
    
    /** Instruction ROM. */
    val irom  = new AhbLite3OnChipRom(
        AhbLite3Config(cfg.vaddrWidth, 32),
        packBits(expected)
    )
    /** Instruction fetcher. */
    val fetch = InsnFetcher(cfg)
    
    // Testbench logic.
    fetch.io.ibus.toAhbLite3 <> irom.io.ahb
    fetch.io.itlb.ppn.assignDontCare()
    fetch.io.itlb.trap.assignDontCare()
    fetch.io.itlb.cause.assignDontCare()
    fetch.io.dout.ready := !io.done
    val counter = RegInit(U(0, 32 bits))
    when (fetch.io.dout.valid && !io.done) {
        counter := counter + 1
    }
    io.done := counter >= expected.length
}

object FetchTest extends App {
    Config.sim.compile(FetchTB(CobraCfg(ISA"RV32IM", entrypoint=0))).doSim(this.getClass.getSimpleName) { dut =>
        // Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)
        
        // Wait another couple cycles.
        dut.clockDomain.waitSampling()
        var i = 0
        while (!dut.io.done.toBoolean && i < 100) {
            dut.clockDomain.waitSampling()
            i += 1
        }
        dut.clockDomain.waitSampling()
    }
}