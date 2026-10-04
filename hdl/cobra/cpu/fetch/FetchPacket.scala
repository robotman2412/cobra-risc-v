package cobra.cpu.fetch

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra.cpu._
import cobra.cpu.mem._
import spinal.core._
import spinal.lib._

case class FetchPacket(cfg: CobraCfg) extends Bundle {
    val addr    = UInt(cfg.XLEN bits)
    val data    = Bits(64 bits)
    val trap    = Bool()
    val cause   = UInt(4 bits)
}
