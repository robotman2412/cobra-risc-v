package cobra.cpu.backend

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import cobra.cpu.execute.ExecResult
import spinal.core._
import spinal.lib._

/**
 * Instruction result buffer stage.
 * Can store a single instruction result when the WriteBack is not ready to receive it.
 */
case class ResultBuffer(cfg: CobraCfg, width: Int) extends Component {
    val io = new Bundle {
        val din  = slave  Stream(ExecResult(cfg, width))
        val dout = master Stream(ExecResult(cfg, width))
    }
    val hasdat = RegInit(False)
    val buf    = Reg(ExecResult(cfg, width))
    
    io.dout.valid := io.din.valid || hasdat
    when (hasdat) {
        io.dout.payload := buf
    } otherwise {
        io.dout.payload := io.din.payload
    }
    when (hasdat && io.din.valid) {
        io.din.ready := io.dout.ready
        when (io.dout.ready) {
            hasdat := io.din.valid
            buf    := io.din.payload
        }
    } elsewhen (io.din.valid) {
        io.din.ready := True
        when (!io.dout.ready) {
            hasdat := True
            buf    := io.din.payload
        } otherwise {
            buf assignDontCare
        }
    } otherwise {
        io.din.ready assignDontCare
    }
}
