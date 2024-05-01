package cobra.cpu.frontend

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.Riscv
import cobra.cpu._
import cobra.cpu.frontend.DecodedInsn.ExeType
import spinal.core._
import spinal.lib._



/**
 * Simple instruction fetch implementation.
 */
case class SimpleFetch(cfg: CobraCfg) extends Component {
    val io = new Bundle {
    }
}
