package cobra.cpu.execution

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu.CobraCfg
import cobra.cpu.frontend.DecodedInsn._
import spinal.core._
import spinal.lib._



/**
 * ALU execution unit.
 */
case class ALU(cfg: CobraCfg) extends ExecUnit(cfg.XLEN, 2, Seq(ExeType.ALU)) {
    // Bitwise operations.
    val bitwise = Bits(cfg.XLEN bits)
    switch (insn.alu.bitMux) {
        is (BitMux.AND) { bitwise := data(0) & data(1) }
        is (BitMux.OR ) { bitwise := data(0) | data(1) }
        is (BitMux.XOR) { bitwise := data(0) ^ data(1) }
        is (BitMux.LHS) { bitwise := data(0) }
    }
    
    // Adder logic.
    val lhs = UInt(cfg.XLEN bits)
    val rhs = UInt(cfg.XLEN bits)
    lhs := data(0).asUInt
    rhs := data(1).asUInt
    when (insn.alu.signed) {
        lhs(cfg.XLEN-1) := !data(0)(cfg.XLEN-1)
    }
    when (insn.alu.signed ^ insn.alu.subtract) {
        rhs(cfg.XLEN-1) := !data(1)(cfg.XLEN-1)
    }
    when (insn.alu.subtract) {
        rhs(cfg.XLEN-2 downto 2) := ~data(1)(cfg.XLEN-2 downto 2).asUInt
    }
    val adder = lhs + rhs
    
    // Comparator.
    val zero  = adder === U(0, cfg.XLEN bits)
    val cmp = Bool()
    when (insn.alu.cmpLT) {
        // Less than.
        cmp := insn.alu.cmpInv ^ (!adder(cfg.XLEN) && !zero)
    } otherwise {
        // Equal to.
        cmp := insn.alu.cmpInv ^ zero
    }
    
    // Bit shifer.
    val shifter = UInt(cfg.XLEN bits)
    when (insn.alu.shiftRight && insn.alu.arithShift) {
        // Arithmetic shift right.
        shifter := (data(0).asSInt >> data(1)(log2Up(cfg.XLEN)-1 downto 0).asUInt).asUInt
        
    } elsewhen (insn.alu.shiftRight) {
        // Logical shift right.
        shifter := data(0).asUInt >> data(1)(log2Up(cfg.XLEN)-1 downto 0).asUInt
        
    } otherwise {
        // Logical shift left.
        shifter := data(0).asUInt << data(1)(log2Up(cfg.XLEN)-1 downto 0).asUInt
    }
    
    // Output multiplexer.
    switch (insn.alu.mux) {
        is (ALUMux.ADDER)      { io.dout.payload.data := adder.asBits }
        is (ALUMux.SHIFTER)    { io.dout.payload.data := shifter.asBits }
        is (ALUMux.COMPARATOR) { io.dout.payload.data := cmp.asBits.resize(cfg.XLEN) }
        is (ALUMux.BITWISE)    { io.dout.payload.data := bitwise }
    }
    
    // Stream logic.
    io.din.ready  := io.dout.ready
    io.dout.valid := valid
}
