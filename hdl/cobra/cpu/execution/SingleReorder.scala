package cobra.cpu.execution

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._
import spinal.lib._



/**
 * Single-issue reorder component.
 * Re-orders a continuous stream of instruction results into program order.
 */
case class SingleReorder(cfg: CobraCfg, ports: Int, width: Int) extends Component {
    val io = new Bundle {
        // Unordered instruction streams.
        val din     = Vec.fill(ports)(slave Stream(ExecResult(cfg, width)))
        // Ordered instruction stream.
        val dout    = master Stream(ExecResult(cfg, width))
    }
    
    // Next instruction to retire.
    val next     = RegInit(U(0, cfg.orderBits bits))
    // Matching instruction bitmask.
    val nextMask = Bits(ports bits)
    
    for (i <- 0 until ports) {
        // Compare available results.
        nextMask(i)     := io.din(i).valid && io.din(i).payload.order === next
        // Consume the matching instruction.
        io.din(i).ready := nextMask(i) && io.dout.ready
    }
    
    when (io.dout.ready && io.dout.valid) {
        next := next + 1
    }
    
    io.dout.valid   := nextMask =/= B(0, ports bits)
    io.dout.payload := io.din.oneHotAccess(nextMask).payload
}