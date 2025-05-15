package cobra.cpu.vmem

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._
import spinal.lib._



/**
 * Page translation and protection value.
 */
case class VMPageData(cfg: CobraCfg, isCode: Boolean, isData: Boolean) extends Bundle {
    /** PMP read permission. */
    val pmpR = isData generate Bool()
    /** PMP write permission. */
    val pmpW = isData generate Bool()
    /** PMP execute permission. */
    val pmpX = isCode generate Bool()
    
    /** PTE read permission. */
    val pteR = isData generate Bool()
    /** PTE write permission. */
    val pteW = isData generate Bool()
    /** PTE execute permission. */
    val pteX = isCode generate Bool()
    
    /** Physical page number. */
    val ppn   = UInt(cfg.ppnWidth bits)
}

/**
 * Virtual memory bus for TLBs and page table walkers.
 * Access latency: 1
 */
case class VMBus(cfg: CobraCfg, isCode: Boolean, isData: Boolean) extends Bundle with IMasterSlave {
    /** Query enable. */
    val query   = Bool()
    /** Virtual page number to access. */
    val vpn     = UInt(cfg.vpnWidth bits)
    /** Response available. */
    val ready   = Bool()
    /** Translation response. */
    val resp    = VMPageData(cfg, isCode, isData)
    
    /** Signals from master (CPU) perspective. */
    def asMaster() = { out(query, vpn); in(ready, resp) }
}
