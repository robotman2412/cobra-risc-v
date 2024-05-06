package cobra.cpu.vmem

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._
import spinal.lib._



/**
 * Stub implementation of virtual VMBus for systems without vmem.
 */
case class VMStub(cfg: CobraCfg, isData: Boolean) extends Component {
    val io = new Bundle {
        val bus = VMBus(cfg, isData)
    }
    val ppnReg = Reg(UInt(cfg.ppnWidth bits))
    ppnReg := io.bus.vpn
    io.bus.ppn := ppnReg
    io.bus.trap := False
    io.bus.cause.assignDontCare()
}

/**
 * Virtual memory bus for TLBs and page table walkers.
 * Access latency: 1
 */
case class VMBus(cfg: CobraCfg, isData: Boolean) extends Bundle with IMasterSlave {
    /** Virtual page number to access. */
    val vpn     = UInt(cfg.vpnWidth bits)
    /** Read access. */
    val r       = isData generate Bool()
    /** Write access. */
    val w       = isData generate Bool()
    /** Execute access. */
    val x       = !isData generate Bool()
    
    /** Physical page number of the access. */
    val ppn     = UInt(cfg.ppnWidth bits)
    /** Page trap raised. */
    val trap    = Bool()
    /** Trap cause. */
    val cause   = UInt(4 bits)
    
    /** Signals from master (CPU) perspective. */
    def asMaster() = { out(vpn, r, w, x); in(ppn, trap, cause) }
}
