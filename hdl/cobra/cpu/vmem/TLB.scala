package cobra.cpu.vmem

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._
import spinal.lib._



case class TLBConfig(lines: Int, ways: Int) {}

case class TLBEntry(cfg: CobraCfg, tlbCfg: TLBConfig, isCode: Boolean, isData: Boolean) extends Bundle {
    /** Cache line tag. */
    val tag  = UInt(cfg.vpnWidth - log2Up(tlbCfg.lines) bit)
    /** Stored translation entry. */
    val data = VMPageData(cfg, isData, isData, isCode)
}

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
    /** Combined tag an data memory. */
    val dataMem = Mem(Vec.fill(tlbCfg.ways)(TLBEntry(cfg, tlbCfg, isCode, isData)), tlbCfg.lines)
    
    /** Next tag that will be requested. */
    val nextTag     = io.out.vpn(cfg.vpnWidth - 1 downto log2Up(tlbCfg.lines))
    /** Next line that will be requested. */
    val nextLine    = io.out.vpn(log2Up(tlbCfg.lines) - 1 downto 0)
    
    /** Current tag corresponding to the read data. */
    val curTag      = RegNext(nextTag)
    /** Current line corresponding to the read data. */
    val curLine     = RegNext(nextLine)
    /** Current line read data. */
    val lineData    = dataMem.readSync(nextLine, io.out.query, writeFirst)
    /** Present bits for the current line data. */
    val linePresent = RegNext(present(nextLine))
    
    val wayMask     = Bits(tlbCfg.ways bits)
    for (i <- 0 until tlbCfg.ways) {
        wayMask(i) := linePresent(i) && lineData(i).tag === curTag
    }
    
}
