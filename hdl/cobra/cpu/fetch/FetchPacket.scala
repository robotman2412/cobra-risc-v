package cobra.cpu.fetch

// Copyright © 2026, Julian Scheffers, see LICENSE for info

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
