package cobra.sim

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra._
import cobra.cpu._
import cobra.cpu.fetch._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.regif.AhbLite3BusInterface

case class FetchTB(cfg: CobraCfg) extends Component {
    val io = new Bundle {
        val stall = in  port Bool()
        val done  = out port Bool()
    }
    
    /** Expected instruction stream. */
    val expected = Seq(
        //              PC      Packet  Fetch   Channel
        0x11110003, //  00      0       0       0
        0x22220003, //  04      0       0       1
        0x33330003, //  08      1       1       0
        0x44440003, //  0c      1       1       1
        0x5550,     //  10      2       2       0
        0x6660,     //  12      2       2       1
        0x7770,     //  14      2       3       0
        0x8880,     //  16      2       3       1
        0x99990003, //  18      3       4       0
        0xaaa0,     //  1c      3       4       1
        0xbbbb0003, //  1e      3-4     5       0
        0xccc0,     //  22      4       5       1
        0xdddd0003, //  24      4       6       0
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
        while ((tmp.length & 3) != 0) {
            tmp = tmp :+ 0l
        }
        var packed = List[Bits]()
        for (i <- 0 until tmp.length / 4) {
            packed = packed :+ B(
                BigInt(tmp(i*4+3)) << 48 |
                BigInt(tmp(i*4+2)) << 32 |
                BigInt(tmp(i*4+1)) << 16 |
                BigInt(tmp(i*4)),
                64 bits
            )
        }
        return packed
    }
    
    /** Instruction ROM. */
    val irom  = new AhbLite3OnChipRom(
        AhbLite3Config(cfg.vaddrWidth, 64),
        packBits(expected)
    )
    /** Instruction fetcher. */
    val fetch = InsnFetcher(cfg)
    
    // Testbench logic.
    fetch.io.ibus.toAhb3Master.toAhbLite3 <> irom.io.ahb
    fetch.io.ibus.trap.allowOverride()
    fetch.io.ibus.cause.allowOverride()
    fetch.io.ibus.rdata.allowOverride()
    fetch.io.ibus.ready.allowOverride()
    when (io.stall) {
        fetch.io.ibus.trap.assignDontCare()
        fetch.io.ibus.cause.assignDontCare()
        fetch.io.ibus.rdata.assignDontCare()
        fetch.io.ibus.ready := False
    }
    fetch.io.dout(0).ready := !io.done
    fetch.io.dout(1).ready := !io.done
    val counter = RegInit(U(0, 32 bits))
    when (fetch.io.dout(1).fire && !io.done) {
        counter := counter + 2
    } elsewhen (fetch.io.dout(0).fire && !io.done) {
        counter := counter + 1
    }
    io.done := counter >= expected.length
}

object FetchTest extends App {
    Config.sim.compile(FetchTB(CobraCfg(ISA"RV32I", 2, entrypoint=0))).doSim(this.getClass.getSimpleName) { dut =>
        dut.io.stall #= false
        
        // Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)
        
        // Wait another couple cycles.
        var i = 0
        do {
            dut.io.stall #= (0 < i && i <= 3) || i == 5
            dut.clockDomain.waitSampling()
            i += 1
        } while (!dut.io.done.toBoolean && i < 100)
        dut.clockDomain.waitSampling()
    }
}