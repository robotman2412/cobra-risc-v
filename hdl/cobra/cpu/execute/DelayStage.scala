package cobra.cpu.execute

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra.cpu._
import cobra.cpu.execute._
import spinal.core._
import spinal.lib._

case class DelayStage(
    cfg: CobraCfg,
    depth: Int,
    hasMem: Boolean = true,
    hasAlu: Boolean = true,
    hasMul: Boolean = true,
    hasDiv: Boolean = true
) extends Component {
    val io = new Bundle {
        val din   = slave  port Flow(InflightInsn(cfg, hasMem, hasAlu, hasMul, hasDiv))
        val dout  = master port Flow(InflightInsn(cfg, hasMem, hasAlu, hasMul, hasDiv))
        val stall = in     port Bool()
    }
    assert(depth >= 1)
    
    val chain = Vec.fill(depth)(Reg(Flow(InflightInsn(cfg, hasMem, hasAlu, hasMul, hasDiv))))
    for (i <- 0 until depth) {
        chain(i).valid.init(False)
    }
    when (!io.stall) {
        chain(0) := io.din
        for (i <- 1 until depth) {
            chain(i) := chain(i-1)
        }
    }
    io.dout := chain(depth-1)
}
