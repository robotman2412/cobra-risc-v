package cobra.sim

// Copyright (c) 2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra.Config
import cobra.cpu._
import cobra.cpu.decode._
import spinal.core.sim._

object DecodeTest extends App {
    for (rv64 <- Seq(false, true)) {
        val cfg = CobraCfg(isa = CobraISA(RV64 = rv64), pagingLevels = if (rv64) 3 else 2)
        Config.sim.compile(DecodeStage(cfg)).doSim { dut =>
            val mask = (BigInt(1) << cfg.XLEN) - 1
            val source = (BigInt(1) << (cfg.XLEN - 1)) | 0x1234
            dut.io.stall #= false
            for (input <- dut.io.din) {
                input.valid #= false
                input.payload.raw #= 0x13
                input.payload.addr #= 0
                input.payload.trap #= false
                input.payload.cause #= 0
            }
            for (write <- dut.io.regWrite) {
                write.we #= false
                write.regno #= 0
                write.data #= 0
            }
            dut.clockDomain.forkStimulus(10)
            dut.clockDomain.waitSampling(2)
            sleep(1)
            assert(dut.io.dout.forall(output => !output.valid.toBoolean))

            // Populate distinct sources through both write-back ports.
            dut.io.regWrite(0).we #= true
            dut.io.regWrite(0).regno #= 30
            dut.io.regWrite(0).data #= source
            dut.io.regWrite(1).we #= true
            dut.io.regWrite(1).regno #= 31
            dut.io.regWrite(1).data #= 7
            dut.clockDomain.waitSampling()
            sleep(1)
            for (write <- dut.io.regWrite) write.we #= false

            // Issue ADDI x1, x30, -9 and ADD x2, x31, x30 together.
            dut.io.din(0).valid #= true
            dut.io.din(0).payload.addr #= 0x1000
            dut.io.din(0).payload.raw #= (0xff7L << 20) | (30 << 15) | (1 << 7) | 0x13
            dut.io.din(1).valid #= true
            dut.io.din(1).payload.addr #= 0x1004
            dut.io.din(1).payload.raw #= (30 << 20) | (31 << 15) | (2 << 7) | 0x33
            sleep(1)
            assert(dut.io.dout.forall(output => !output.valid.toBoolean),
                "Fetched instructions must wait for the input pipeline register")
            dut.clockDomain.waitSampling()
            sleep(1)
            for (i <- 0 until 2) {
                assert(dut.io.din(i).ready.toBoolean)
                assert(dut.io.dout(i).valid.toBoolean)
                assert(dut.io.dout(i).payload.pc.toLong == 0x1000 + i * 4)
                assert(dut.io.dout(i).payload.insn.rd.toInt == i + 1)
                assert(dut.io.dout(i).payload.insn.usesRd.toBoolean)
            }
            assert(dut.io.dout(0).payload.data1.toBigInt == source)
            assert(dut.io.dout(0).payload.data2.toBigInt == ((-BigInt(9)) & mask))
            assert(dut.io.dout(1).payload.data1.toBigInt == 7)
            assert(dut.io.dout(1).payload.data2.toBigInt == source)

            // A stalled Flow output must not issue the held inputs again.
            dut.io.stall #= true
            sleep(1)
            for (i <- 0 until 2) {
                assert(!dut.io.din(i).ready.toBoolean)
                assert(!dut.io.dout(i).valid.toBoolean)
                // Upstream changes must not overwrite the stalled instructions.
                dut.io.din(i).payload.raw #= 0x00300093
                dut.io.din(i).payload.addr #= 0x2000 + i * 4
            }
            dut.clockDomain.waitSampling(2)
            dut.io.stall #= false
            sleep(1)
            assert(dut.io.dout.forall(_.valid.toBoolean))
            for (i <- 0 until 2) {
                assert(dut.io.dout(i).payload.pc.toLong == 0x1000 + i * 4)
                assert(dut.io.dout(i).payload.insn.rd.toInt == i + 1)
            }
            assert(dut.io.dout(0).payload.data2.toBigInt == ((-BigInt(9)) & mask))
            assert(dut.io.dout(1).payload.data2.toBigInt == source)
            dut.clockDomain.waitSampling()
            sleep(1)
            for (i <- 0 until 2) {
                assert(dut.io.dout(i).valid.toBoolean)
                assert(dut.io.dout(i).payload.pc.toLong == 0x2000 + i * 4)
                assert(dut.io.dout(i).payload.data2.toBigInt == 3)
            }

            // Legal stale bits in a bubble must not become an instruction.
            dut.io.din(0).valid #= false
            dut.io.din(1).valid #= false
            sleep(1)
            assert(dut.io.dout.forall(_.valid.toBoolean),
                "Upstream bubbles must not clear the registered instructions before the clock edge")
            dut.clockDomain.waitSampling()
            sleep(1)
            assert(dut.io.dout.forall(output => !output.valid.toBoolean))

            // Trapping or illegal inputs cannot execute through the normal ALU path.
            dut.io.din(0).valid #= true
            dut.io.din(0).payload.trap #= true
            dut.io.din(1).valid #= true
            dut.io.din(1).payload.raw #= 0xffffffffL
            dut.clockDomain.waitSampling()
            sleep(1)
            assert(dut.io.dout.forall(output => !output.valid.toBoolean))

            // Writes to x0 must not change the source selected for ADDI x1, x0, 3.
            dut.io.din(0).payload.trap #= false
            dut.io.din(0).payload.raw #= 0x00300093
            dut.io.din(1).valid #= false
            dut.io.regWrite(0).we #= true
            dut.io.regWrite(0).regno #= 0
            dut.io.regWrite(0).data #= mask
            dut.clockDomain.waitSampling()
            sleep(1)
            assert(dut.io.dout(0).valid.toBoolean)
            assert(dut.io.dout(0).payload.data1.toBigInt == 0)
            assert(dut.io.dout(0).payload.data2.toBigInt == 3)
        }
    }
}
