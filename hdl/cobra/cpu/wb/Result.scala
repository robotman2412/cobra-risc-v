package cobra.cpu.wb

import cobra.cpu._
import spinal.core._

case class Result(cfg: CobraCfg = CobraCfg()) extends Bundle {
    val order = UInt(cfg.orderBits bits)
    val regno = UInt(5 bits)
    val data  = UInt(cfg.XLEN bits)
}
