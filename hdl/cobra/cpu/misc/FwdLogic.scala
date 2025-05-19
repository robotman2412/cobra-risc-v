package cobra.cpu.misc

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra._
import cobra.cpu._
import spinal.core._
import spinal.lib._
import cobra.cpu.execute._



/**
 * Forwarding and stalling logic.
 */
case class FwdLogic(
    val cfg:    CobraCfg,
    val fwd1:   Boolean = true,
    val fwd2:   Boolean = true,
    val hasMem: Boolean = true,
    val hasAlu: Boolean = true,
    val hasMul: Boolean = true,
    val hasDiv: Boolean = true
) extends Component {
    val io = new Bundle {
        /** Incoming issued instruction. */
        val din      = slave  port Flow(IssuedInsn(cfg, hasMem, hasAlu, hasMul, hasDiv))
        /** Outgoing issued instruction. */
        val dout     = master port Flow(IssuedInsn(cfg, hasMem, hasAlu, hasMul, hasDiv))
        /** Stall request; when asserted, this and stages before are stalled. */
        val stallReq = out    port Bool()
    }
    
    io.dout.insn := io.din.insn
}