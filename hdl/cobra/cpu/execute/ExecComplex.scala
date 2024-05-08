package cobra.cpu.execute

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import cobra.cpu.execute._
import cobra.cpu.execute.unit._
import spinal.core._



/**
 * Generates the appropriate execution complex for the config.
 */
object ExecComplex {
    def apply(cfg: CobraCfg, nStale: Int) = {
        ScalarExecComplex(cfg, nStale, {
            var list = Seq[ExecUnit.Factory](ALU.factory)
            if (cfg.isa.M) list = list :+ MUL.Simple.factory _
            if (cfg.isa.M) list = list :+ DIV.Simple.factory _
            list
        })
    }
}
