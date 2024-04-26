package cobra

import spinal.core._

case class Regfile(XLEN: Int) extends Component {
    val io = new Bundle {
        val we      = in  Bool()
        val waddr   = in  UInt(5 bits)
        val wdata   = in  UInt(XLEN bits)
        
        val raddr_a = in  UInt(5 bits)
        val rdata_a = out UInt(XLEN bits)
        
        val raddr_b = in  UInt(5 bits)
        val rdata_b = out UInt(XLEN bits)
    }
    
    val storage = Mem(UInt(XLEN bits), 32)
    storage.init(List.fill(32)(U"0"))
    when (io.waddr =/= U"0") {
        storage.write(io.waddr, io.wdata, io.we)
    }
    io.rdata_a := storage.readAsync(io.raddr_a)
    io.rdata_b := storage.readAsync(io.raddr_b)
}
