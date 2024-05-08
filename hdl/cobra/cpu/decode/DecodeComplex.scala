package cobra.cpu.decode

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import cobra.cpu.fetch._
import spinal.core._
import spinal.lib._



/**
 * Single-issue decode/issue complex.
 */
case class DecodeComplex(cfg: CobraCfg) extends Component {
    val io = new Bundle {
        /** Fetched instruction stream. */
        val din         = slave  port Stream(FetchedInsn(cfg))
        /** Issued instruction stream. */
        val dout        = master port Stream(IssuedInsn(cfg))
        /** Base address for JALR instructions. */
        val jalrBase    = in     port UInt(cfg.XLEN bits)
    }
    
    /** Fetch packet is valid. */
    val valid = RegInit(False)
    /** Pipeline register. */
    val plr   = Reg(FetchedInsn(cfg))
    /** Instruction order number. */
    val order = RegInit(U(0, cfg.orderBits bits))
    when (io.dout.ready && io.dout.valid) {
        order := order + 1
    }
    
    // Decoding of decompressed instructions.
    val decoder = InsnDecoder(cfg)
    decoder.io.decomp   := plr.raw
    
    // TODO: Branch target address generation.
    
    // Stream logic.
    when (!valid || io.dout.ready) {
        valid := io.din.valid
        plr   := io.din.payload
    }
    io.din.ready            := !valid || io.dout.ready
    io.dout.valid           := valid
    io.dout.payload.pc      := plr.addr
    io.dout.payload.insn    := decoder.io.decd
    io.dout.payload.order   := order
}
