package cobra.cpu.execute

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import cobra.cpu.decode._
import spinal.core._
import spinal.lib._



/**
 * Execution unit input signals.
 */
case class ExecInput(
    val cfg:      CobraCfg,
    val width:    Int,
    val nParam:   Int,
    val supports: Seq[SpinalEnumElement[DecodedInsn.ExeType.type]] = null
) extends Bundle {
    val order   = UInt(cfg.orderBits bits)
    val data    = Vec.fill(nParam)(Bits(width bits))
    val insn    = if (supports != null) DecodedInsn(supports) else DecodedInsn()
    val pc      = UInt(cfg.paddrWidth bits)
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
    val pc      = UInt(cfg.paddrWidth bits)
}

/**
 * Creates execution units.
 */
object ExecUnit {
    type Factory = CobraCfg => ExecUnit
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
    
    /** Generates logic that detects whether this unit supports an operation. */
    def supportsLogic(thing: SpinalEnumCraft[DecodedInsn.ExeType.type]): Bool = {
        val supportsTmp = Bool()
        supportsTmp := False
        for (mode <- supports) {
            when (thing === mode) {
                supportsTmp := True
            }
        }
        return supportsTmp
    }
    
    /** Instruction is valid. */
    val valid   = RegInit(False)
    /** Ordering. */
    val order   = Reg(UInt(cfg.orderBits bits))
    /** Operands / input data. */
    val data    = Reg(Vec.fill(nparam)(Bits(width bits)))
    /** Instruction buffer. */
    val insn    = Reg(DecodedInsn(supports))
    /** Current program counter. */
    val pc      = Reg(UInt(cfg.paddrWidth bits))
    
    // Output logic.
    io.dout.payload.regno   := insn.rd
    io.dout.payload.we      := insn.usesRd
    io.dout.payload.order   := order
    io.dout.payload.pc      := pc
    
    // Buffer logic.
    when (io.din.ready && io.din.valid) {
        order := io.din.payload.order
        data  := io.din.payload.data
        insn  := io.din.payload.insn
        pc    := io.din.payload.pc
        valid := True
    } elsewhen (io.din.ready) {
        valid := False
    }
}
