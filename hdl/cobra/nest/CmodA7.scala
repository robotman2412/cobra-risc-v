package cobra.nest

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra._
import spinal.core._

case class CmodA7() extends Component {
    val io = new Bundle {
    }
}

object CmodA7Verilog extends App {
    Config.spinal.generateVerilog(CmodA7())
}
