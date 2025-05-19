package cobra.cpu.execute

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import spinal.core._
import spinal.lib._
import cobra.cpu._
import cobra.cpu.decode._



object Mul {
    def apply(cfg: CobraCfg): MulDiv = MulDiv(cfg, false)
}

object Div {
    def apply(cfg: CobraCfg): MulDiv = MulDiv(cfg, true)
}

case class MulDiv(cfg: CobraCfg, isDiv: Boolean) extends Component {
    /** How many cycles this unit takes to compute. */
    val latency = if (isDiv) { cfg.divLatency } else { cfg.mulLatency }
    
    val io = new Bundle {
        /** Instruction to execute. */
        val din     = slave  port Flow(IssuedInsn(cfg, false, false, !isDiv, isDiv))
        /** Pending results. */
        val pending = latency > 1 generate Vec.fill(latency-1)(out port PendData(cfg))
        /** Result data. */
        val dout    = master port Flow(WbData(cfg))
        /** Stall this unit. */
        val stall   = in     port Bool()
    }
    
    // Initial pipeline register.
    val valid = RegNextWhen(io.din.valid,         !io.stall)
    val insn  = RegNextWhen(io.din.payload.insn,  !io.stall)
    val data1 = RegNextWhen(io.din.payload.data1, !io.stall)
    val data2 = RegNextWhen(io.din.payload.data2, !io.stall)
    
    val tmp = if (isDiv) {
        // Divider implementation.
        val lhs = SInt(cfg.XLEN + 1 bits)
        val rhs = SInt(cfg.XLEN + 1 bits)
        // Sign logic.
        lhs(cfg.XLEN-1 downto 0) := data1.asSInt
        lhs(cfg.XLEN-1)          := data1(cfg.XLEN-1) && insn.div.unsigned
        rhs(cfg.XLEN-1 downto 0) := data2.asSInt
        rhs(cfg.XLEN-1)          := data1(cfg.XLEN-1) && insn.div.unsigned
        // Divide/remainder and truncate the extra bits.
        Cat((lhs / rhs)(cfg.XLEN-1 downto 0), (lhs % rhs)(cfg.XLEN-1 downto 0))
    } else {
        // Multiplier implementation.
        val lhs = SInt(cfg.XLEN + 1 bits)
        val rhs = SInt(cfg.XLEN + 1 bits)
        // Sign logic.
        lhs(cfg.XLEN-1 downto 0) := data1.asSInt
        lhs(cfg.XLEN-1)          := data1(cfg.XLEN-1) && insn.mul.unsignedL
        rhs(cfg.XLEN-1 downto 0) := data2.asSInt
        rhs(cfg.XLEN-1)          := data1(cfg.XLEN-1) && insn.mul.unsignedR
        // Multiply and truncate the extra bits.
        (lhs * rhs)(2*cfg.XLEN-1 downto 0).asBits
    }
    
    // Internal pipeline registers.
    val selTmp  = if (isDiv) { insn.div.remainder } else { insn.mul.upper }
    val pendTmp = Flow(PendData(cfg))
    pendTmp.rd    := io.din.insn.rd
    pendTmp.valid := io.din.valid
    val tmpPlr  = pipelineDelay(tmp, latency-1, io.stall)
    val selPlr  = pipelineDelay(selTmp, latency-1, io.stall)
    val pendPlr = registerChain(pendTmp, latency, io.stall)
    
    for (i <- 0 until latency-1) {
        io.pending(i) := pendPlr(i)
    }
    
    // Output multiplexer logic.
    io.dout.valid := pendPlr(latency-1).valid
    io.dout.rd    := pendPlr(latency-1).rd
    when (selTmp) {
        io.dout.res   := tmpPlr(cfg.XLEN*2-1 downto cfg.XLEN)
    } otherwise {
        io.dout.res   := tmpPlr(cfg.XLEN-1 downto 0)
    }
}
