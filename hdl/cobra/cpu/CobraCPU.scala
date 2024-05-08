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
case class CobraCPU(
    // CPU features and other parameters.
    cfg:    CobraCfg,
    // I/O region list; all accesses within are in program order.
    mmio:   Seq[MemRange] = Seq[MemRange]()
) extends Component {
    val io = new Bundle {
        /** Data bus. */
        val dbus = master port AhbLite3Master(AhbLite3Config(cfg.paddrWidth, cfg.XLEN))
        /** Data bus. */
        val ibus = master port AhbLite3Master(AhbLite3Config(cfg.paddrWidth, 32))
    }
    
    val fetch   = InsnFetcher(cfg)
    val decode  = DecodeComplex(cfg)
    val execute = ExecComplex(cfg, 2)
    val forward = ForwardingLogic(cfg, 2, execute.nUnits, cfg.XLEN)
    val regfile = Regfile(cfg, cfg.XLEN, 1, 2)
    
    // Forwarding logic.
    forward.io.done             := execute.io.done
    forward.io.order            := execute.io.next
    forward.io.regread          <> regfile.io.read
    forward.io.regstale         <> execute.io.stale
    execute.io.regread          <> forward.io.fwread
    
    // Pipeline connections.
    fetch.io.ibus               <> io.ibus
    fetch.io.dout               >> decode.io.din
    decode.io.dout              >> execute.io.din
    decode.io.jalrBase          := regfile.io.read(0).data.asUInt
    execute.io.dout.ready       := True
    regfile.io.write(0).we      := execute.io.dout.valid && execute.io.dout.payload.we
    regfile.io.write(0).regno   := execute.io.dout.payload.regno
    regfile.io.write(0).data    := execute.io.dout.payload.data
}