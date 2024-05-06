package cobra.cpu.execution.unit

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu.CobraCfg
import cobra.cpu.execution._
import cobra.cpu.decode.DecodedInsn._
import spinal.core._
import spinal.lib._



object MUL {
    /**
     * Simple multiplier implementation.
     */
    case class Simple(cfg: CobraCfg) extends ExecUnit(cfg.XLEN, 2, Seq(ExeType.MUL)) {
        // Sign logic.
        val lhs = SInt(cfg.XLEN*2 bits)
        val rhs = SInt(cfg.XLEN*2 bits)
        when (insn.mul.unsignedL) {
            lhs(cfg.XLEN-1 downto 0)            := data(0).asSInt
            lhs(cfg.XLEN*2-1 downto cfg.XLEN)   := S(0, cfg.XLEN bits)
        } otherwise {
            lhs := data(0).asSInt.resize(cfg.XLEN*2 bits)
        }
        when (insn.mul.unsignedR) {
            rhs(cfg.XLEN-1 downto 0)            := data(1).asSInt
            rhs(cfg.XLEN*2-1 downto cfg.XLEN)   := S(0, cfg.XLEN bits)
        } otherwise {
            rhs := data(1).asSInt.resize(cfg.XLEN*2 bits)
        }
        
        // Output mux.
        val multiplier = lhs * rhs
        when (insn.mul.upper) {
            io.dout.payload.data := multiplier(cfg.XLEN*2-1 downto cfg.XLEN).asBits
        } otherwise {
            io.dout.payload.data := multiplier(cfg.XLEN-1 downto 0).asBits
        }
        
        // Stream logic.
        io.din.ready  := io.dout.ready || !valid
        io.dout.valid := valid
    }
}
