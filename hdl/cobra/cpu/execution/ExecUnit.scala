package cobra.cpu.execution

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu.CobraCfg
import cobra.cpu.frontend.DecodedInsn
import spinal.core._
import spinal.lib._



/**
 * Execution unit input signals.
 */
case class ExecInput(cfg: CobraCfg, width: Int, nparam: Int) extends Bundle {
    val order   = UInt(cfg.orderBits bits)
    val data    = Vec.fill(nparam)(Bits(width bits))
    val insn    = DecodedInsn()
}

/**
 * Execution unit output signals.
 * FP instructions that do not write to the regfile must have `we === false`.
 * Integer instruction writes to `regno === U"0"` are ingored.
 */
case class ExecResult(cfg: CobraCfg, width: Int) extends Bundle {
    val order = UInt(cfg.orderBits bits)
    val we    = Bool()
    val regno = UInt(5 bits)
    val data  = Bits(width bits)
}

/**
 * Abstract execution unit.
 * Implementations are responsible for `din.ready`, `dout.payload.data` and `dout.valid`.
 */
abstract class ExecUnit(
    val width:    Int,
    val nparam:   Int,
    val supports: Seq[SpinalEnumElement[DecodedInsn.ExeType.type]]
) extends Component {
    def cfg: CobraCfg
    
    val io = new Bundle {
        val din     = slave  Stream(ExecInput(cfg, width, nparam))
        val dout    = master Stream(ExecResult(cfg, width))
    }
    
    /** Ordering. */
    val order   = Reg(UInt(cfg.orderBits bits))
    /** Instruction is valid. */
    val valid   = RegInit(False)
    /** Operands / input data. */
    val data    = Reg(Vec.fill(nparam)(Bits(width bits)))
    /** Instruction buffer. */
    val insn    = Reg(DecodedInsn())
    
    // Output logic.
    io.dout.payload.regno   := insn.rd
    io.dout.payload.we      := insn.usesRd
    io.dout.payload.order   := order
    
    // Buffer logic.
    when (io.din.ready && io.din.valid) {
        insn  := io.din.payload.insn
        order := io.din.payload.order
        data  := io.din.payload.data
        valid := True
    } elsewhen (io.din.ready) {
        valid := False
    }
}
