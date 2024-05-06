package cobra.cpu.execution

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import cobra.cpu.backend._
import cobra.cpu.decode.IssuedInsn
import spinal.core._
import spinal.lib._



/**
 * Single register out-of-order forwarding logic.
 */
case class SingleForwarding(cfg: CobraCfg, nUnits: Int, nParam: Int, width: Int) extends Component {
    val io = new Bundle {
        /** Register to forward to. */
        val regno       = in  port UInt(5 bits)
        /** Decoded instruction stream. */
        val din         = in  port IssuedInsn(cfg)
        /** Finished instruction results. */
        val done        = Vec.fill(nUnits)(in(Stream(ExecResult(cfg, width))))
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
        order(i) := io.done(i).payload.order - io.din.order
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