package cobra.cpu.frontend

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.Riscv
import cobra.cpu._
import spinal.core._
import spinal.lib._



/**
 * Issued instruction.
 */
case class IssuedInsn(cfg: CobraCfg) extends Bundle {
    val order   = UInt(cfg.orderBits bits)
    val insn    = DecodedInsn()
    val pc      = UInt(cfg.paddrWidth bits)
}
