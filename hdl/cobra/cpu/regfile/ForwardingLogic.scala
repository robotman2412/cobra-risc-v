package cobra.cpu.regfile

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import cobra.cpu.backend._
import cobra.cpu.execute._
import spinal.core._
import spinal.lib._



/**
 * Single register's worth of out-of-order forwarding logic.
 */
case class ForwardingSlice(cfg: CobraCfg, nUnits: Int, width: Int) extends Component {
    val io = new Bundle {
        /** Register to forward to. */
        val regno       = in  port UInt(5 bits)
        /** Order of next instruction to be retired. */
        val order       = in  port UInt(cfg.orderBits bits)
        /** Finished instruction results. */
        val done        = Vec.fill(nUnits)(slave Flow(ExecResult(cfg, width)))
        /** Register can be forwarded. */
        val forwardable = out port Bool()
        /** Execution unit source bitmask. */
        val sourceMask  = out port Bits(nUnits bits)
    }
    
    // Filter for matching register numbers.
    val matches = Bits(nUnits bits)
    for (i <- 0 until nUnits) {
        matches(i) := io.done(i).valid && io.done(i).payload.we && io.done(i).payload.regno === io.regno
    }
    
    // Offset order numbers to be linear.
    val order = Vec.fill(nUnits)(UInt(cfg.orderBits bits))
    for (i <- 0 until nUnits) {
        order(i) := io.done(i).payload.order - io.order
    }
    
    // Sorting logic.
    val allOrder = {
        var tmp = B(0, 1 << cfg.orderBits bits)
        for (i <- 0 until nUnits) {
            tmp = tmp | (matches(i).asBits << order(i))
        }
        tmp
    }
    val firstOrder = Bits(1 << cfg.orderBits bits)
    firstOrder(0) := allOrder(0)
    for (i <- 1 until 1 << cfg.orderBits) {
        firstOrder(i) := allOrder(i) && allOrder(i-1 downto 0) === B(0, i bits)
    }
    for (i <- 0 until nUnits) {
        io.sourceMask(i) := firstOrder(order(i))
    }
    io.forwardable := io.sourceMask =/= B(0, nUnits bits)
}

/**
 * Generic forwarding logic for scalar and superscalar CPUs.
 */
case class ForwardingLogic(cfg: CobraCfg, nRegs: Int, nUnits: Int, width: Int) extends Component {
    val io = new Bundle {
        /** Read ports with forwarding. */
        val fwread      = Vec.fill(nRegs)(slave  port RegRead(width))
        /** Register file read ports. */
        val regread     = Vec.fill(nRegs)(master port RegRead(width))
        /** Register stale checking ports. */
        val regstale    = Vec.fill(nRegs)(master port RegStaleCheck())
        /** Order of next instruction to be issued. */
        val order       = in  port UInt(cfg.orderBits bits)
        /** Finished instruction results. */
        val done        = Vec.fill(nUnits)(slave Flow(ExecResult(cfg, width)))
    }
    
    for (i <- 0 until nRegs) {
        val fwHelper    = ForwardingSlice(cfg, nUnits, width)
        fwHelper.io.order       := io.order
        fwHelper.io.done        := io.done
        fwHelper.io.regno       := io.fwread(i).regno
        io.regstale(i).regno    := io.fwread(i).regno
        io.regread(i).regno     := io.fwread(i).regno
        when (!io.regstale(i).stale) {
            // Register isn't stale.
            io.fwread(i).data   := io.regread(i).data
            io.fwread(i).stale  := False
        } otherwise {
            // Register is stale.
            io.fwread(i).data   := io.done.oneHotAccess(fwHelper.io.sourceMask).payload.data
            io.fwread(i).stale  := !fwHelper.io.forwardable
        }
    }
}
