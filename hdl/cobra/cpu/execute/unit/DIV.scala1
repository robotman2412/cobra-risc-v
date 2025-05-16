package cobra.cpu.execute.unit

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu.CobraCfg
import cobra.cpu.decode.DecodedInsn._
import cobra.cpu.execute._
import spinal.core._
import spinal.lib._



object DIV {
    object Simple {
        def factory(cfg: CobraCfg): ExecUnit = DIV.Simple(cfg)
    }
    
    /**
     * Simple divider implementation.
     */
    case class Simple(cfg: CobraCfg) extends ExecUnit(cfg.XLEN, 2, Seq(ExeType.DIV)) {
        // Sign logic.
        val lhs = SInt(cfg.XLEN*2 bits)
        val rhs = SInt(cfg.XLEN*2 bits)
        when (insn.div.unsigned) {
            lhs(cfg.XLEN-1 downto 0)            := data(0).asSInt
            lhs(cfg.XLEN*2-1 downto cfg.XLEN)   := S(0, cfg.XLEN bits)
        } otherwise {
            lhs := data(0).asSInt.resize(cfg.XLEN*2 bits)
        }
        when (insn.div.unsigned) {
            rhs(cfg.XLEN-1 downto 0)            := data(1).asSInt
            rhs(cfg.XLEN*2-1 downto cfg.XLEN)   := S(0, cfg.XLEN bits)
        } otherwise {
            rhs := data(1).asSInt.resize(cfg.XLEN*2 bits)
        }
        
        // Output mux.
        val divider   = lhs / rhs
        val remainder = lhs % rhs
        when (insn.div.remainder) {
            io.dout.payload.data := remainder.asBits
        } otherwise {
            io.dout.payload.data := divider.asBits
        }
        
        // Stream logic.
        io.din.ready  := io.dout.ready || !valid
        io.dout.valid := valid
    }
}
