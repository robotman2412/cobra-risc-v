package cobranest

import spinal.core._
import cobra._

case class CmodA7() extends Component {
    val io = new Bundle {
    }
}

object CmodA7Verilog extends App {
    Config.spinal.generateVerilog(CmodA7())
}
