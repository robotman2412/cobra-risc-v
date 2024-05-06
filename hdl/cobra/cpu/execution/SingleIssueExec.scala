package cobra.cpu.execution

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._
import spinal.lib._
import cobra.cpu.decode.{IssuedInsn, RegRead}



/**
 * Single-issue execution complex.
 */
case class SingleIssueExec(cfg: CobraCfg, factories: Seq[ExecUnit.Factory]) extends Component {
    val io = new Bundle {
        /** Issued instruction stream. */
        val decd    = slave  Stream(IssuedInsn(cfg))
        /** Regfile read ports. */
        val reg     = Vec.fill(2)(master(RegRead(cfg.XLEN)))
        /** Instruction results. */
        val dout    = master Stream(ExecResult(cfg, cfg.XLEN))
    }
    
    /** Stale counter. */
    val stale       = RegStaleCounter(cfg, 2, true)
    /** Number of units. */
    val nUnits      = factories.length
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
    /** Forwarding helpers. */
    val fwHelpers = {
        var tmp = List[SingleForwarding]()
        for (i <- 0 until 2) {
            tmp = tmp :+ SingleForwarding(cfg, nUnits, 2, cfg.XLEN)
        }
        tmp
    }
    /** Packed outputs from units. */
    var fwTmpVal = Vec.fill(nUnits)(Bits(cfg.XLEN bits))
    for (i <- 0 until nUnits) {
        fwTmpVal(i) := units(i).io.dout.payload.data
    }
    
    // Forwarding logic.
    var regVal       = Vec.fill(2)(Bits(cfg.XLEN bits))
    var forwardStall = Bool()
    forwardStall := False
    io.reg(0).regno := io.decd.payload.insn.rs1
    io.reg(1).regno := io.decd.payload.insn.rs2
    stale.io.check(0).regno := io.decd.payload.insn.rs1
    stale.io.check(1).regno := io.decd.payload.insn.rs2
    fwHelpers(0).io.regno   := io.decd.payload.insn.rs1
    fwHelpers(1).io.regno   := io.decd.payload.insn.rs2
    for (i <- 0 until 2) {
        fwHelpers(i).io.din     := io.decd.payload
        for (j <- 0 until nUnits) {
            fwHelpers(i).io.done(j) := units(j).io.dout
        }
        when (stale.io.check(i).stale && fwHelpers(i).io.forwardable) {
            // Forwardable stale register.
            regVal(i) := fwTmpVal.oneHotAccess(fwHelpers(i).io.sourceMask)
        } elsewhen (stale.io.check(i).stale) {
            // Stall due to stale register.
            forwardStall := True
            regVal(i) assignDontCare
        } otherwise {
            // Up-to-date register.
            regVal(i) := io.reg(i).data
        }
    }
    
    // Register staleness logic.
    stale.io.mark     := io.decd.valid && io.decd.ready && io.decd.payload.insn.usesRd
    stale.io.markNo   := io.decd.payload.insn.rd
    stale.io.unmark   := io.dout.valid && io.dout.ready && io.dout.payload.we
    stale.io.unmarkNo := io.dout.payload.regno
    
    // Selection logic.
    val selPropMask  = SInt(nUnits bits)
    val select       = Vec.fill(nUnits)(Bool)
    for (i <- 0 until nUnits) {
        selPropMask(i) := !units(i).supportsLogic(io.decd.payload.insn.exeType) || !units(i).io.din.ready
        selectMask(i)  := select(i)
    }
    
    select(0) := units(0).supportsLogic(io.decd.payload.insn.exeType)
    for (i <- 1 until nUnits) {
        select(i) := selPropMask(i-1 downto 0) === S(-1, i bits)
    }
    
    // Input stream logic.
    for (i <- 0 until nUnits) {
        // Stream inputs.
        units(i).io.din.valid           := io.decd.valid && select(i) && !forwardStall
        units(i).io.din.payload.order   := io.decd.payload.order
        units(i).io.din.payload.insn    := io.decd.payload.insn
        units(i).io.din.payload.pc      := io.decd.payload.pc
        // RS1 / PC.
        when (io.decd.payload.insn.usesRs1) {
            units(i).io.din.payload.data(0) := regVal(0)
        } otherwise {
            units(i).io.din.payload.data(0) := io.decd.payload.pc.asBits.resize(cfg.XLEN bits)
        }
        // RS2 / imm.
        when (io.decd.payload.insn.usesRs2) {
            units(i).io.din.payload.data(1) := regVal(1)
        } otherwise {
            units(i).io.din.payload.data(1) := io.decd.payload.insn.imm.resize(cfg.XLEN bits).asBits
        }
    }
    io.decd.ready := selectMask =/= B(0, nUnits bits) && !forwardStall
    
    // Reordering logic.
    val reorder = SingleReorder(cfg, nUnits, cfg.XLEN)
    for (i <- 0 until nUnits) {
        units(i).io.dout >> reorder.io.din(i)
    }
    reorder.io.dout >> io.dout
}

