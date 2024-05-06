package cobra.cpu.decode

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

/**
 * Regfile read port.
 */
case class RegRead(width: Int) extends Bundle with IMasterSlave {
    val regno   = UInt(5 bits)
    val data    = Bits(width bits)
    
    def asMaster() = {
        out(regno)
        in (data)
    }
}

/**
 * Regfile write port.
 */
case class RegWrite(width: Int) extends Bundle with IMasterSlave {
    val we      = Bool()
    val regno   = UInt(5 bits)
    val data    = Bits(width bits)
    
    def asMaster() = {
        out(we)
        out(regno)
        out(data)
    }
}

/**
 * Parametric register file.
 */
case class Regfile(cfg: CobraCfg, width: Int, wports: Int = 1, rports: Int = 2) extends Component {
    val io = new Bundle {
        val write = Vec.fill(wports)(slave(RegWrite(width)))
        val read  = Vec.fill(rports)(slave(RegRead(width)))
    }
    
    val storage = Mem(Bits(width bits), 32)
    storage.simPublic()
    storage.init(List.fill(32)(B(0, width bits)))
    for (port <- io.write) {
        when (port.we && port.regno =/= U"00000") {
            storage.write(port.regno, port.data)
        }
    }
    for (port <- io.read) {
        port.data := storage.readAsync(port.regno)
    }
    
    def read(regno: Int) = {
        storage.getBigInt(regno)
    }
    def write(regno: Int, value: BigInt) = {
        storage.setBigInt(regno, value)
    }
}
