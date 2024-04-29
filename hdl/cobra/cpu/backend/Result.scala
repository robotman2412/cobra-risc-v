package cobra.cpu.backend

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._



/**
 * Integer or floating-point result.
 * FP instructions that do not write to the regfile must have `we === false`.
 * Integer instruction writes to `regno === U"0"` are ingored.
 */
case class Result[T <: Data](cfg: CobraCfg, dtype: HardType[T]) extends Bundle {
    val order = UInt(cfg.orderBits bits)
    val we    = Bool()
    val regno = UInt(5 bits)
    val data  = dtype()
}
