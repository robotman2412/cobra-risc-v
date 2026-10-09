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
        val stall      = in  port Bool()
        val done       = out port Bool()
    }
    
    /** ROM instructions in address order (branches replay or skip entries). */
    val expected = Seq(
        //              PC      Packet
        0x11110003, //  00      0
        0x22220003, //  04      0
        0x33330003, //  08      1
        0x44440003, //  0c      1
        0x5550,     //  10      2
        0x6660,     //  12      2
        0x7770,     //  14      2
        0x8880,     //  16      2
        0x99990003, //  18      3
        0xaaa0,     //  1c      3
        0xbbbb0003, //  1e      3-4
        0xccc0,     //  22      4
        0xdddd0003, //  24      4
        0x1010,     //  28      5
        0x1020,     //  2a      5
        0x1030,     //  2c      5
        0x1040,     //  2e      5
        0x1050,     //  30      6
        0x1060,     //  32      6
        0x1070,     //  34      6
        0x1080,     //  36      6
        0x1090,     //  38      7
        0x10a0,     //  3a      7
        0x10b0,     //  3c      7
        0x10c0,     //  3e      7
        0xeeee0003, //  40      8
        0xffff0003, //  44      8
        0x12340003, //  48      9
        0x23450003, //  4c      9
        0x34560003, //  50      10
        0x45670003, //  54      10
        0x56780003, //  58      11
        0x67890003, //  5c      11
        // Four more packets of compressed instructions and four of long
        // instructions leave room for the delayed-response branch tests.
        0x2010,     //  60      12
        0x2020,     //  62      12
        0x2030,     //  64      12
        0x2040,     //  66      12
        0x2050,     //  68      13
        0x2060,     //  6a      13
        0x2070,     //  6c      13
        0x2080,     //  6e      13
        0x2090,     //  70      14
        0x20a0,     //  72      14
        0x20b0,     //  74      14
        0x20c0,     //  76      14
        0x20d0,     //  78      15
        0x20e0,     //  7a      15
        0x20f0,     //  7c      15
        0x2100,     //  7e      15
        0x789a0003, //  80      16
        0x89ab0003, //  84      16
        0x9abc0003, //  88      17
        0xabcd0003, //  8c      17
        0xbcde0003, //  90      18
        0xcdef0003, //  94      18
        0xdef00003, //  98      19
        0xef010003, //  9c      19
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
    for (output <- fetch.io.dout) {
        output.valid.simPublic()
        output.payload.addr.simPublic()
        output.payload.raw.simPublic()
    }
    
    fetch.io.ibus.toAhb3Master.toAhbLite3 <> irom.io.ahb
    fetch.io.branchTrigger  := False
    fetch.io.branchTarget.assignDontCare()

    // One cycle per simulation loop iteration, starting after reset.
    // Branch edges are t=270, 320, 360 and 400 with the 10-unit clock.
    // The same-packet branch follows the 0x24/0x28 outputs at cycle 15;
    // the far branch follows packet 1 at cycle 23. Current-cycle outputs
    // are consumed normally before any redirect takes effect.
    val cycle = RegInit(U(0, 8 bits))
    cycle := cycle + 1
    when (cycle === 10) {
        // Reverse branch without a wait state: packet 3/4 -> packet 2.
        fetch.io.branchTrigger := True
        fetch.io.branchTarget := S(0x10, cfg.badVaddrWidth bits)
    }
    when (cycle === 19) {
        // Reverse branch during a wait state: return to packet 1.
        fetch.io.branchTrigger := True
        fetch.io.branchTarget := S(0x08, cfg.badVaddrWidth bits)
    }
    when (cycle === 15) {
        // Current outputs 0x24/0x28 still retire; skip 0x2a/0x2c within packet 5.
        fetch.io.branchTrigger := True
        fetch.io.branchTarget := S(0x2e, cfg.badVaddrWidth bits)
    }
    when (cycle === 23) {
        // Skip ahead to packet 10, beyond the two-packet ringbuffer.
        fetch.io.branchTrigger := True
        fetch.io.branchTarget := S(0x50, cfg.badVaddrWidth bits)
    }
    // Repeat the four branch semantics with delayed target responses.
    // 25: retire 0x58/0x5c, request packet 2; response stalls at 26-28.
    // 33: retire 0x24/0x28, skip 0x2a/0x2c; response stalls at 34-36.
    // 40: retire buffered 0x38/0x3a during the first stall (40-42).
    //     Request packet 1 at 43, then stall its response at 44-46.
    // 50: retire 0x18/0x1c, request packet 18; response stalls at 51-53.
    when (cycle === 25) {
        fetch.io.branchTrigger := True
        fetch.io.branchTarget := S(0x10, cfg.badVaddrWidth bits)
    }
    when (cycle === 33) {
        fetch.io.branchTrigger := True
        fetch.io.branchTarget := S(0x2e, cfg.badVaddrWidth bits)
    }
    when (cycle === 40) {
        fetch.io.branchTrigger := True
        fetch.io.branchTarget := S(0x08, cfg.badVaddrWidth bits)
    }
    when (cycle === 50) {
        fetch.io.branchTrigger := True
        fetch.io.branchTarget := S(0x90, cfg.badVaddrWidth bits)
    }
    fetch.io.branchTrigger.simPublic()
    fetch.io.branchTarget.simPublic()
    fetch.io.ibus.enable.simPublic()
    fetch.io.ibus.addr.simPublic()
    fetch.io.ibus.ready.simPublic()
    fetch.fetchActive.simPublic()

    // Testbench logic.
    irom.io.ahb.HADDR.allowOverride()
    fetch.io.ibus.trap.allowOverride()
    fetch.io.ibus.cause.allowOverride()
    fetch.io.ibus.rdata.allowOverride()
    fetch.io.ibus.ready.allowOverride()
    val prevAddr = RegNext(irom.io.ahb.HADDR)
    when (io.stall) {
        irom.io.ahb.HADDR := prevAddr
        fetch.io.ibus.trap.assignDontCare()
        fetch.io.ibus.cause.assignDontCare()
        fetch.io.ibus.rdata.assignDontCare()
        fetch.io.ibus.ready := False
    }
    fetch.io.dout(0).ready := !io.done
    fetch.io.dout(1).ready := !io.done
    // Finish after the last branch reaches the ROM end. An overshoot also
    // stops the run; the stream checker reports it as a fetch failure.
    val finished = RegInit(False)
    for (output <- fetch.io.dout) {
        when (cycle > 50 && output.fire && output.payload.addr >= S(0x9c, cfg.badVaddrWidth bits)) {
            finished := True
        }
    }
    io.done := finished
}

object FetchTest extends App {
    Config.sim.compile(FetchTB(CobraCfg(ISA"RV64I", entrypoint=0))).doSim(this.getClass.getSimpleName) { dut =>
        dut.io.stall #= false
        dut.clockDomain.forkStimulus(period = 10)

        val branches = Map(
            10 -> ("reverse", 0x10),
            15 -> ("forward-packet", 0x2e),
            19 -> ("reverse-stalled", 0x08),
            23 -> ("forward-far", 0x50),
            25 -> ("reverse-delayed", 0x10),
            33 -> ("forward-packet-delayed", 0x2e),
            40 -> ("reverse-stalled-delayed", 0x08),
            50 -> ("forward-far-delayed", 0x90)
        )
        // Target address phases must complete before these response stalls.
        val targetRequests = Map(25 -> 0x10, 33 -> 0x28, 43 -> 0x08, 50 -> 0x90)
        val responseStalls = Seq(26 to 28, 34 to 36, 44 to 46, 51 to 53)
        var address = 0L
        val rom = dut.expected.map { raw =>
            val entry = address -> (raw.toLong & 0xffffffffL)
            address += (if ((raw & 3) == 3) 4 else 2)
            entry
        }.toMap
        val endAddress = address
        val failures = scala.collection.mutable.ArrayBuffer[String]()
        var nextPc = 0L
        var segment = "linear"
        var segmentFailed = false
        var branchCount = 0
        var targetRequestCount = 0
        var stalledResponseCount = 0
        var i = 0
        do {
            val delayingResponse = responseStalls.exists(_.contains(i))
            dut.io.stall #= (0 < i && i <= 3) || i == 5 || (19 <= i && i <= 21) ||
                (40 <= i && i <= 42) || delayingResponse
            sleep(1) // Settle ROM/ready forwarding before checking this cycle's outputs.
            if (targetRequests.contains(i)) {
                assert(dut.fetch.io.ibus.enable.toBoolean && dut.fetch.io.ibus.ready.toBoolean,
                    s"Target request was not accepted at cycle $i")
                assert(dut.fetch.io.ibus.addr.toLong == targetRequests(i),
                    s"Wrong target packet requested at cycle $i")
                targetRequestCount += 1
            }
            if (delayingResponse) {
                assert(dut.fetch.fetchActive.toBoolean && !dut.fetch.io.ibus.ready.toBoolean,
                    s"No pending response stalled at cycle $i")
                assert(!dut.fetch.io.dout(0).valid.toBoolean && !dut.fetch.io.dout(1).valid.toBoolean,
                    s"Instructions were forwarded before the target response at cycle $i")
                stalledResponseCount += 1
            }
            for (ch <- 0 until 2 if !dut.io.done.toBoolean && dut.fetch.io.dout(ch).valid.toBoolean) {
                val output = dut.fetch.io.dout(ch).payload
                val pc = output.addr.toLong
                val raw = output.raw.toLong
                val expectedRaw = rom.get(nextPc)
                if ((pc != nextPc || !expectedRaw.contains(raw)) && !segmentFailed) {
                    failures += f"$segment at cycle $i channel $ch: expected PC 0x$nextPc%x " +
                        expectedRaw.map(value => f"instruction 0x$value%08x").getOrElse("past ROM end") +
                        f", got PC 0x$pc%x instruction 0x$raw%08x"
                    // Keep running to exercise every branch; report the first error
                    // in each segment rather than cascading errors from that redirect.
                    segmentFailed = true
                }
                expectedRaw.foreach { value =>
                    nextPc += (if ((value & 3) == 3) 4 else 2)
                }
            }
            val branching = dut.fetch.io.branchTrigger.toBoolean
            assert(branching == branches.contains(i), s"Unexpected branch timing at cycle $i")
            if (branching) {
                val (name, target) = branches(i)
                assert(dut.fetch.io.branchTarget.toLong == target)
                assert(dut.io.stall.toBoolean == (name == "reverse-stalled" || name == "reverse-stalled-delayed"))
                branchCount += 1
            }
            dut.clockDomain.waitSampling()
            // Both outputs of the branch cycle belong to the old instruction
            // stream. Only after their sampling edge does the expected PC change.
            if (branching) {
                val (name, target) = branches(i)
                nextPc = target
                segment = name
                segmentFailed = false
            }
            i += 1
        } while (!dut.io.done.toBoolean && i < 100)
        dut.clockDomain.waitSampling()
        assert(branchCount == branches.size, "Not all branches were exercised")
        assert(targetRequestCount == targetRequests.size, "Not all delayed target requests were exercised")
        assert(stalledResponseCount == responseStalls.map(_.size).sum, "Not all target response stalls were exercised")
        assert(failures.isEmpty, failures.mkString("Branch fetch failures:\n", "\n", ""))
        assert(dut.io.done.toBoolean, "Fetch simulation timed out")
        assert(nextPc == endAddress, "Stopped before the ROM end")
    }
}
