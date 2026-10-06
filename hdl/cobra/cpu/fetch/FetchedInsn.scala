package cobra.cpu.fetch

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra.cpu._
import spinal.core._



case class FetchedInsn(cfg: CobraCfg) extends Bundle {
    /** Instruction base address / trapping part address. */
    val addr    = SInt(cfg.badVaddrWidth bits)
    /** Raw instruction bits. */
    val raw     = Bits(32 bits)
    /** Trap raised. */
    val trap    = Bool()
    /** Trap cause. */
    val cause   = UInt(5 bits)
}
