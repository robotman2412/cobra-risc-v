package cobra.cpu

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu.decode._
import cobra.cpu.execute._
import cobra.cpu.fetch._
import cobra.cpu.regfile._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._



/**
 * Cobra RISC-V CPU.
 */
case class CobraCpu(
    // CPU features and other parameters.
    cfg:    CobraCfg,
) extends Component {
    val io = new Bundle {
        /** Data bus. */
        val dbus = master port AhbLite3Master(AhbLite3Config(cfg.paddrWidth, cfg.XLEN))
        /** Instruction bus. */
        val ibus = master port AhbLite3Master(AhbLite3Config(cfg.paddrWidth, 64))
    }
    
    val decd0 = Decoder(cfg)
    val decd1 = Decoder(cfg)
    val alu0  = Alu(cfg)
    val alu1  = Alu(cfg)
    val mul   = Mul(cfg)
    val div   = Div(cfg)
}