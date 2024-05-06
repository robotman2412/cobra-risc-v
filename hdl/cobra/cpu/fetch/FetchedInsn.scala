package cobra.cpu.fetch

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._



case class FetchedInsn(cfg: CobraCfg) extends Bundle {
    /** Instruction base address / trapping part address. */
    val addr    = UInt(cfg.vaddrWidth bits)
    /** Raw instruction bits. */
    val raw     = Bits(32 bits)
    /** Trap raised. */
    val trap    = Bool()
    /** Trap cause. */
    val cause   = UInt(4 bits)
}
