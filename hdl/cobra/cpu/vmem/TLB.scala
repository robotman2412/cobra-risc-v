package cobra.cpu.vmem

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._
import spinal.lib._



case class TLBConfig(lines: Int, ways: Int) {}

/**
 * Translation lookaside buffer; stores translation, protection and PMP.
 */
case class TLB(cfg: CobraCfg, tlbCfg: TLBConfig, isCode: Boolean, isData: Boolean) extends Component {
    val io = new Bundle {
        /** Side that responds to translation requests. */
        val out = slave  port VMBus(cfg, isCode, isData)
        /** Side that fetches translation requests. */
        val in  = master port VMBus(cfg, isCode, isData)
    }
    
    /** Present bits of all entries in the TLB. */
    val present = Vec.fill(tlbCfg.lines)(RegInit(B(0, tlbCfg.ways bits)))
    /** Translation values. */
    val dataMem = Mem(Vec.fill(tlbCfg.ways)(VMPageData(cfg, isCode, isData)))
}
