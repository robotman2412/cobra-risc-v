package cobra.cpu.wb

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._

case class Result(cfg: CobraCfg = CobraCfg()) extends Bundle {
    val order = UInt(cfg.orderBits bits)
    val regno = UInt(5 bits)
    val data  = UInt(cfg.XLEN bits)
}
