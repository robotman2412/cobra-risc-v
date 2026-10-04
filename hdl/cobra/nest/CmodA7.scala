package cobra.nest

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra._
import spinal.core._

case class CmodA7() extends Component {
    val io = new Bundle {
    }
}

object CmodA7Verilog extends App {
    Config.spinal.generateVerilog(CmodA7())
}
