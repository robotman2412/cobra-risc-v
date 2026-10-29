package cobra.cpu.misc

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

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
case class Regfile(cfg: CobraCfg, writeR0: Boolean, wports: Int = 2, rports: Int = 4) extends Component {
    val io = new Bundle {
        val write = Vec.fill(wports)(slave(RegWrite(cfg.XLEN)))
        val read  = Vec.fill(rports)(slave(RegRead(cfg.XLEN)))
    }
    
    val storage = Vec.fill(32)(RegInit(B(0, cfg.XLEN bits)))
    storage.simPublic()
    io.simPublic()
    for (port <- io.write) {
        when (port.we && (Bool(writeR0) || port.regno =/= U"00000")) {
            storage(port.regno) := port.data
        }
    }
    for (port <- io.read) {
        port.data := storage(port.regno)
    }
    
    def read(regno: Int) = {
        storage(regno).toBigInt
    }
    def readForwarded(regno: Int): BigInt = {
        for (port <- io.write.reverse) {
            if (port.regno.toInt == regno && port.we.toBoolean) {
                return port.data.toBigInt
            }
        }
        read(regno)
    }
    def write(regno: Int, value: BigInt) = {
        storage(regno) #= value
    }
}
