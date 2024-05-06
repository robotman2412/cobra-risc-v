package cobra.cpu.vmem

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import cobra.cpu.vmem.L1Cache._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._



case class L1$Cfg(cfg: CobraCfg, vdWidth: Int, pdWidth: Int, ways: Int = 2, lineWords: Int = 4) {
    val dWidth      = if (vdWidth > pdWidth) vdWidth else pdWidth
    val paddrWidth  = cfg.paddrWidth
    val vaddrWidth  = cfg.vaddrWidth
    val ppnWidth    = cfg.ppnWidth
    val vpnWidth    = cfg.vpnWidth
    val lines       = 4096 / lineWords * 8 / dWidth
}

/**
 * AHB-lite-3 Virtually Indexed, Physically Tagged L1 cache.
 */
case class L1Cache(cfg: L1$Cfg) extends Component {
    val io = new Bundle {
        /** Memory bus coming from CPU. */
        val bus_in  = slave (AhbLite3(AhbLite3Config(cfg.vaddrWidth, cfg.vdWidth)))
        /** Memory bus going to L2 cache. */
        val bus_out = master(AhbLite3(AhbLite3Config(cfg.paddrWidth, cfg.pdWidth)))
        /** PPN from the L1 TLB. */
        val ppn     = in  port UInt(cfg.ppnWidth bits)
    }
    
    /** Data memory. */
    val dmem   = Mem(Vec.fill(cfg.ways)(Line(cfg)), cfg.lines*cfg.lineWords)
    /** Data memory read address. */
    val draddr = UInt(dmem.addressWidth bits)
    /** Data memory read data. */
    val drdata = dmem.readSync(draddr)
    /** Data memory write address. */
    val dwaddr = UInt(dmem.addressWidth bits)
    /** Data memory write data. */
    val dwdata = Line(cfg)
    /** Data memory byte strobe. */
    val dwbs   = Bits(cfg.dWidth / 8 bits)
    /** Data memory write way. */
    val dwway  = UInt(log2Up(cfg.ways) bits)
    /** Data memory write access logic.*/
    val dwlogic = new Area {
        val tmpdata = Vec.fill(cfg.ways)(Line(cfg))
        val bitmask = Bits(dmem.wordType.getBitsWidth bits)
        for (x <- 0 until cfg.ways) {
            tmpdata(x) := dwdata
            for (y <- 0 until cfg.dWidth) {
                when (dwway === U(x, dwway.getBitsWidth bits)) {
                    bitmask(x*cfg.dWidth+y) := dwbs(y/8)
                } otherwise {
                    bitmask(x*cfg.dWidth+y) := False
                }
            }
            dmem.write(dwaddr, tmpdata, mask=bitmask)
        }
    }
    
    /** Tag memory. */
    val tmem = Mem(TagSet(cfg), cfg.lines)
    /** Data memory read address. */
    val traddr = UInt(dmem.addressWidth bits)
    /** Data memory read data. */
    val trdata = dmem.readSync(traddr)
    /** Data memory write address. */
    val twaddr = UInt(dmem.addressWidth bits)
    /** Data memory write data. */
    val twdata = Tag(cfg)
    /** Data memory write way. */
    val twway  = UInt(log2Up(cfg.ways) bits)
}

object L1Cache {
    def Line(cfg: L1$Cfg) = Bits(cfg.dWidth bits)
    case class TagSet(cfg: L1$Cfg) extends Bundle {
        /** Next tag to be overwritten. */
        val next = UInt(log2Up(cfg.ways) bits)
        /** List of tags in this line. */
        val tags = Vec.fill(cfg.ways)(Tag(cfg))
        /** Alias for tags(i). */
        def apply(index: Int) = tags(index)
        /** Alias for tags(i). */
        def apply(address: UInt) = tags(address)
    }
    case class Tag(cfg: L1$Cfg) extends Bundle {
        /** Associated line contains valid data. */
        val valid   = Bool()
        /** Physical page number. */
        val ppn     = UInt(cfg.ppnWidth bits)
    }
}
