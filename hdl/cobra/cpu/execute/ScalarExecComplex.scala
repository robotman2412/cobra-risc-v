package cobra.cpu.execute

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import cobra.cpu.decode._
import cobra.cpu.regfile._
import spinal.core._
import spinal.lib._



/**
 * Single-issue execution complex.
 */
case class ScalarExecComplex(cfg: CobraCfg, nStale: Int, factories: Seq[ExecUnit.Factory]) extends Component {
    /** Number of units. */
    val nUnits      = factories.length
    
    val io = new Bundle {
        /** Issued instruction stream. */
        val din     = slave  Stream(IssuedInsn(cfg))
        /** Instruction results. */
        val dout    = master Stream(ExecResult(cfg, cfg.XLEN))
        /** Order of next instruction to be retired. */
        val next    = out port UInt(cfg.orderBits bits)
        /** Regfile read ports. */
        val regread = Vec.fill(2)(master(RegRead(cfg.XLEN)))
        /** Stale checking ports. */
        val stale   = Vec.fill(nStale)(slave(RegStaleCheck()))
        /** Forwarding sources. */
        val done    = Vec.fill(nUnits)(master Flow(ExecResult(cfg, cfg.XLEN)))
    }
    
    /** Stale counter. */
    val stale       = RegStaleCounter(cfg, nStale, true)
    stale.io.check <> io.stale
    
    /** Selected for current operation mask. */
    val selectMask  = Bits(nUnits bits)
    /** Units in this complex. */
    val units = {
        var tmp = List[ExecUnit]()
        for (factory <- factories) {
            tmp = tmp :+ factory(cfg)
        }
        tmp
    }
    
    // Forwarding logic.
    var forwardStall = Bool()
    forwardStall := False
    var regVal = Vec.fill(2)(Bits(cfg.XLEN bits))
    for (i <- 0 until 2) {
        io.regread(i).regno := io.din.payload.insn.rs(i)
        when (!io.din.payload.insn.usesRs(i)) {
            regVal(i).assignDontCare()
        } elsewhen (io.regread(i).stale) {
            regVal(i).assignDontCare()
            forwardStall := True
        } otherwise {
            regVal(i) := io.regread(i).data
        }
    }
    
    // Register staleness logic.
    stale.io.mark     := io.din.valid && io.din.ready && io.din.payload.insn.usesRd
    stale.io.markNo   := io.din.payload.insn.rd
    stale.io.unmark   := io.dout.valid && io.dout.ready && io.dout.payload.we
    stale.io.unmarkNo := io.dout.payload.regno
    
    // Selection logic.
    val selPropMask     = SInt(nUnits bits)
    val select          = Vec.fill(nUnits)(Bool)
    for (i <- 0 until nUnits) {
        selPropMask(i) := !units(i).supportsLogic(io.din.payload.insn.exeType) || !units(i).io.din.ready
        selectMask(i)  := select(i)
    }
    
    select(0) := units(0).supportsLogic(io.din.payload.insn.exeType)
    for (i <- 1 until nUnits) {
        select(i) := selPropMask(i-1 downto 0) === S(-1, i bits)
    }
    
    val selReady    = Bool()
    selReady        := False
    for (i <- 0 until nUnits) {
        when (select(i) && units(i).io.din.ready) {
            selReady := True
        }
    }
    when (selectMask === B(0, nUnits bits)) {
        selReady := True
    }
    
    // Input stream logic.
    for (i <- 0 until nUnits) {
        // Stream inputs.
        units(i).io.din.valid           := io.din.valid && select(i) && !forwardStall
        units(i).io.din.payload.order   := io.din.payload.order
        units(i).io.din.payload.insn    := io.din.payload.insn
        units(i).io.din.payload.pc      := io.din.payload.pc
        // RS1 / PC.
        when (io.din.payload.insn.usesRs1) {
            units(i).io.din.payload.data(0) := regVal(0)
        } otherwise {
            units(i).io.din.payload.data(0) := io.din.payload.pc.asBits.resize(cfg.XLEN bits)
        }
        // RS2 / imm.
        when (io.din.payload.insn.usesRs2) {
            units(i).io.din.payload.data(1) := regVal(1)
        } otherwise {
            units(i).io.din.payload.data(1) := io.din.payload.insn.imm.resize(cfg.XLEN bits).asBits
        }
    }
    io.din.ready := selReady && !forwardStall
    
    // Reordering logic.
    val reorder = ScalarReorder(cfg, nUnits, cfg.XLEN)
    for (i <- 0 until nUnits) {
        units(i).io.dout    >> reorder.io.din(i)
        io.done(i).valid    := units(i).io.dout.valid
        io.done(i).payload  := units(i).io.dout.payload
    }
    reorder.io.dout >> io.dout
    io.next := reorder.io.next
}

