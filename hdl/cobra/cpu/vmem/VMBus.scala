package cobra.cpu.vmem

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._
import spinal.lib._



/**
 * Page translation and protection value.
 */
case class VMPageData(cfg: CobraCfg, r: Boolean, w: Boolean, x: Boolean) extends Bundle {
    /** Whether paging is enabled for this entry. */
    val paged = Bool()
    
    /** PMP read permission. */
    val pmpR = r generate Bool()
    /** PMP write permission. */
    val pmpW = w generate Bool()
    /** PMP execute permission. */
    val pmpX = x generate Bool()
    
    /** PTE read permission. */
    val pteR = r generate Bool()
    /** PTE write permission. */
    val pteW = w generate Bool()
    /** PTE execute permission. */
    val pteX = x generate Bool()
    
    /** Physical page number. */
    val ppn   = UInt(cfg.ppnWidth bits)
}

/**
 * Virtual memory bus for TLBs and page table walkers.
 * The TLB or page walker must respond with the initial request before fetching data for the next request.
 * As such, the CPU or lower-level TLB must internally keep track of the previous request to match, and stall the next while `ready` is 0.
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
    val resp    = VMPageData(cfg, isData, isData, isCode)
    
    /** Signals from master (CPU) perspective. */
    def asMaster() = { out(query, vpn); in(ready, resp) }
}
