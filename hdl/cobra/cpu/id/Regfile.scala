package cobra.cpu.id

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._

/**
 * Dual-read, single-write integer register file.
 */
case class Regfile(cfg: CobraCfg) extends Component {
    val io = new Bundle {
        val we      = in  Bool()
        val waddr   = in  UInt(5 bits)
        val wdata   = in  UInt(cfg.XLEN bits)
        
        val raddr_a = in  UInt(5 bits)
        val rdata_a = out UInt(cfg.XLEN bits)
        
        val raddr_b = in  UInt(5 bits)
        val rdata_b = out UInt(cfg.XLEN bits)
    }
    
    val storage = Mem(UInt(cfg.XLEN bits), 32)
    storage.init(List.fill(32)(U"0"))
    when (io.we && io.waddr =/= U"0") {
        storage.write(io.waddr, io.wdata)
    }
    io.rdata_a := storage.readAsync(io.raddr_a)
    io.rdata_b := storage.readAsync(io.raddr_b)
}
