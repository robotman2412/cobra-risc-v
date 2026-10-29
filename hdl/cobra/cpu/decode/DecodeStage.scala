package cobra.cpu.decode

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import spinal.core._
import spinal.lib._
import cobra.cpu._
import cobra.cpu.decode._
import cobra.cpu.fetch._
import cobra.cpu.execute._
import cobra.cpu.misc._

/** The decode and issue stage. */
case class DecodeStage(cfg: CobraCfg) extends Component {
    val io = new Bundle {
        /** Fetched instructions. */
        val din         = Vec.fill(2)(slave port Stream(FetchedInsn(cfg)))
        /** Decoded instructions (temporary). */
        val dout        = Vec.fill(2)(master port Flow(InflightInsn(cfg)))
        /** Write-back data. */
        val regWrite    = Vec.fill(2)(slave port RegWrite(cfg.XLEN))
        /** Stall this unit. */
        val stall       = in port Bool()
    }
    
    val decoder0 = Decoder(cfg)
    val decoder1 = Decoder(cfg)
    val regfile  = Regfile(cfg, false)
    
    regfile.io.read(0).regno := decoder0.io.decd.rs1
    regfile.io.read(1).regno := decoder0.io.decd.rs2
    regfile.io.read(2).regno := decoder1.io.decd.rs1
    regfile.io.read(3).regno := decoder1.io.decd.rs2
    regfile.io.write <> io.regWrite
    
    for ((decoder, i) <- Seq(decoder0, decoder1).zipWithIndex) {
        // Capture the fetched instruction before the combinational decoder.
        val input = Reg(Flow(FetchedInsn(cfg)))
        input.valid.init(False)
        io.din(i).ready := !io.stall
        when (!io.stall) {
            input.valid := io.din(i).valid
            input.payload := io.din(i).payload
        }
        val output = io.dout(i)
        val insn = decoder.io.decd

        decoder.io.decomp := input.payload.raw
        output.valid := input.valid && !io.stall && decoder.io.legal && !input.payload.trap
        output.payload.insn := insn
        output.payload.pc := input.payload.addr.resized
        output.payload.data1 := regfile.io.read(i * 2).data
        output.payload.data2 := insn.imm.resize(cfg.XLEN).asBits
        when (insn.usesRs2) {
            output.payload.data2 := regfile.io.read(i * 2 + 1).data
        }
    }
}
