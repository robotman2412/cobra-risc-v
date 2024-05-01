package cobra.cpu.execution

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra._
import cobra.cpu._
import cobra.cpu.execution._
import cobra.cpu.frontend._
import spinal.core._
import spinal.lib._



/**
 * Register stale checking port.
 */
case class RegStaleCheck() extends Bundle with IMasterSlave {
    /** Register number to check. */
    val regno = UInt(5 bits)
    /** Register is stale. */
    val stale = Bool()
    
    def asMaster() = {
        out(regno)
        in (stale)
    }
}

/**
 * Counter-based register stale detector.
 */
case class RegStaleCounter(cfg: CobraCfg, checkPorts: Int, lockR0: Boolean) extends Component {
    val io = new Bundle {
        /** Register being marked stale. */
        val mark        = in  port Bool()
        /** Register number being marked. */
        val markNo      = in  port UInt(5 bits)
        /** Register being unmarked stale. */
        val unmark      = in  port Bool()
        /** Register number being unmarked. */
        val unmarkNo    = in  port UInt(5 bits)
        /** Register stale checking ports. */
        val check       = Vec.fill(checkPorts)(slave(RegStaleCheck()))
    }
    
    /** Counters table. */
    val counters = Vec.fill(32)(RegInit(U(0, cfg.orderBits bits)))
    
    // Counting logic.
    if (lockR0) {
        counters(0) := U(0, cfg.orderBits bits)
    }
    for (i <- lockR0.toInt to 31) {
        val inc = io.mark   && io.markNo   === U(i, 5 bits)
        val dec = io.unmark && io.unmarkNo === U(i, 5 bits)
        when (inc && !dec) {
            counters(i) := counters(i) + U(1, cfg.orderBits bits)
        } elsewhen (dec && !inc) {
            counters(i) := counters(i) - U(1, cfg.orderBits bits)
        }
    }
    
    // Checking logic.
    for (port <- io.check) {
        port.stale := counters(port.regno) =/= U(0, cfg.orderBits bits)
    }
}
