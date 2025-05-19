package cobra.cpu.execute

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu.CobraCfg
import cobra.cpu.decode.DecdInsn._
import cobra.cpu.execute._
import spinal.core._
import spinal.lib._
import cobra.cpu.decode._



/**
 * ALU that supports additive, branch condition eval, bitwise and bit shift.
 */
case class Alu(cfg: CobraCfg) extends Component {
    val io = new Bundle {
        /** Instruction to execute. */
        val din   = slave  port Flow(IssuedInsn(cfg, true, true, false, false))
        /** Result data and instruction to pass through. */
        val dout  = master port Flow(IssuedInsn(cfg, true, false, false, false))
        /** Stall this unit. */
        val stall = in     port Bool()
    }
    
    // Pipeline register.
    val valid = RegNextWhen(io.din.valid,         !io.stall)
    val insn  = RegNextWhen(io.din.payload.insn,  !io.stall)
    val data1 = RegNextWhen(io.din.payload.data1, !io.stall)
    val data2 = RegNextWhen(io.din.payload.data2, !io.stall)
    
    // Bitwise operations.
    val bitwise = Bits(cfg.XLEN bits)
    switch (insn.alu.bitMux) {
        is (BitMux.AND) { bitwise := data1 & data2 }
        is (BitMux.OR ) { bitwise := data1 | data2 }
        is (BitMux.XOR) { bitwise := data1 ^ data2 }
        is (BitMux.LHS) { bitwise := data1 }
    }
    
    // Adder logic.
    val lhs = UInt(cfg.XLEN bits)
    val rhs = UInt(cfg.XLEN bits)
    lhs := data1.asUInt
    rhs := data2.asUInt
    when (insn.alu.signed) {
        lhs(cfg.XLEN-1) := !data1(cfg.XLEN-1)
    }
    when (insn.alu.signed ^ insn.alu.subtract) {
        rhs(cfg.XLEN-1) := !data2(cfg.XLEN-1)
    }
    when (insn.alu.subtract) {
        rhs(cfg.XLEN-2 downto 0) := ~data2(cfg.XLEN-2 downto 0).asUInt
    }
    val adder = lhs.expand + rhs.expand
    
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
        shifter := (data1.asSInt |>> data2(log2Up(cfg.XLEN)-1 downto 0).asUInt).asUInt
        
    } elsewhen (insn.alu.shiftRight) {
        // Logical shift right.
        shifter := data1.asUInt |>> data2(log2Up(cfg.XLEN)-1 downto 0).asUInt
        
    } otherwise {
        // Logical shift left.
        shifter := data1.asUInt |<< data2(log2Up(cfg.XLEN)-1 downto 0).asUInt
    }
    
    // Output multiplexer.
    switch (insn.alu.mux) {
        is (ALUMux.ADDER)      { io.dout.payload.data1 := adder.asBits.resize(cfg.XLEN) }
        is (ALUMux.SHIFTER)    { io.dout.payload.data1 := shifter.asBits.resize(cfg.XLEN) }
        is (ALUMux.COMPARATOR) { io.dout.payload.data1 := cmp.asBits.resize(cfg.XLEN) }
        is (ALUMux.BITWISE)    { io.dout.payload.data1 := bitwise }
    }
    io.dout.payload.data2 := data2
    
    // Stream logic.
    io.dout.valid := valid
}
