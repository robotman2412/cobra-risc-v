package cobra.cpu.frontend

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.Riscv
import cobra.cpu._
import spinal.core._
import spinal.lib._



/**
 * Decoded instruction data.
 */
case class DecodedInsn() extends Bundle {
    // Register indices.
    val rs1     = UInt(5 bits)
    val rs2     = UInt(5 bits)
    val rs3     = UInt(5 bits)
    val rd      = UInt(5 bits)
    // Register usage.
    val usesRs1 = Bool()
    val usesRs2 = Bool()
    val usesRs3 = Bool()
    val usesRd  = Bool()
    // Float operation size.
    val fpSize  = UInt(2 bits)
    // Memory access control signals.
    val mem     = DecodedInsn.Mem()
    // Perform 32-bit integer operation.
    val op32    = Bool()
    // ALU control signals.
    val alu     = DecodedInsn.ALU()
    // Multiplier control signals.
    val mul     = DecodedInsn.Mul()
    // Divider control signals.
    val div     = DecodedInsn.Div()
}

object DecodedInsn {
    /** Memory access control signals. */
    case class Mem() extends Bundle {
        val asize       = UInt(2 bits)
        val signed      = Bool()
        val re          = Bool()
        val we          = Bool()
    }
    /** ALU output selection. */
    object ALUMux extends SpinalEnum {
        val BITWISE, SHIFTER, ADDER, COMPARATOR = newElement()
    }
    /** ALU bitwise mode selection. */
    object BitMux extends SpinalEnum {
        val XOR, LHS, OR, AND = newElement()
    }
    /** ALU control signals. */
    case class ALU() extends Bundle {
        val signed      = Bool()
        val subtract    = Bool()
        val cmpLT       = Bool()
        val cmpInv      = Bool()
        val shiftRight  = Bool()
        val arithShift  = Bool()
        val bitMux      = BitMux()
        val mux         = DecodedInsn.ALUMux()
    }
    /** Multiplier control signals. */
    case class Mul() extends Bundle {
        val upper       = Bool()
        val unsignedL   = Bool()
        val unsignedR   = Bool()
    }
    /** Divider control signals. */
    case class Div() extends Bundle {
        val remainder   = Bool()
        val unsigned    = Bool()
    }
}

/**
 * Immediate operand encoding.
 */
object ImmEncoding extends SpinalEnum {
    val IMM12, UIMM20, IMM12_SW, IMM5_CSR = newElement()
}

/**
 * Instruction decoder logic.
 */
case class InsnDecoder(cfg: CobraCfg) extends Component {
    val io = new Bundle {
        val decd = out port DecodedInsn()
    }
    
    // TODO: Decompression.
    val decomp = UInt(32 bits)
    val immEnc = ImmEncoding()
    val hasImm = Bool()
    
    // Defaults.
    io.decd.assignDontCare()
    immEnc.assignDontCare()
    hasImm := False
    
    // Register indices.
    io.decd.rs1 := decomp(19 downto 15)
    io.decd.rs2 := decomp(24 downto 20)
    io.decd.rs3 := decomp(31 downto 27)
    io.decd.rd  := decomp(11 downto  7)
    
    val opcode = decomp(6 downto 2)
    when (opcode === Riscv.ALU_OPS && opcode(5) && decomp(25)) {
        // MUL/DIV operations.
        io.decd.op32    := decomp(3)
        io.decd.usesRd  := True
        io.decd.usesRs1 := True
        io.decd.usesRs2 := True
        // Multiplier bit patterns.
        switch (decomp(14 downto 12)) {
            is(U"00") {
                io.decd.mul.upper       := False
            }
            is(U"01") {
                io.decd.mul.upper       := True
                io.decd.mul.unsignedL   := False
                io.decd.mul.unsignedR   := False
            }
            is(U"10") {
                io.decd.mul.upper       := True
                io.decd.mul.unsignedL   := False
                io.decd.mul.unsignedR   := True
            }
            is(U"11") {
                io.decd.mul.upper       := True
                io.decd.mul.unsignedL   := True
                io.decd.mul.unsignedR   := True
            }
        }
        // Divider bit patterns.
        io.decd.div.remainder := decomp(13)
        io.decd.div.unsigned  := decomp(12)
        
    } elsewhen (opcode === Riscv.ALU_OPS) {
        // ALU operations.
        hasImm          := !decomp(5)
        immEnc          := ImmEncoding.IMM12
        io.decd.op32    := decomp(3)
        io.decd.usesRd  := True
        io.decd.usesRs1 := True
        io.decd.usesRs2 := !hasImm
        switch (decomp(14 downto 12)) {
            is(Riscv.ALU_ADD)  {
                io.decd.alu.mux         := DecodedInsn.ALUMux.ADDER
                io.decd.alu.subtract    := decomp(30) && !hasImm
            }
            is(Riscv.ALU_SLL)  {
                io.decd.alu.mux         := DecodedInsn.ALUMux.SHIFTER
                io.decd.alu.shiftRight  := False
            }
            is(Riscv.ALU_SLT)  {
                io.decd.alu.mux         := DecodedInsn.ALUMux.COMPARATOR
                io.decd.alu.signed      := True
            }
            is(Riscv.ALU_SLTU) {
                io.decd.alu.mux         := DecodedInsn.ALUMux.COMPARATOR
                io.decd.alu.signed      := False
            }
            is(Riscv.ALU_XOR)  {
                io.decd.alu.mux         := DecodedInsn.ALUMux.BITWISE
                io.decd.alu.bitMux      := DecodedInsn.BitMux.XOR
            }
            is(Riscv.ALU_SRL)  {
                io.decd.alu.mux         := DecodedInsn.ALUMux.SHIFTER
                io.decd.alu.shiftRight  := False
                io.decd.alu.arithShift  := decomp(30)
            }
            is(Riscv.ALU_OR)   {
                io.decd.alu.mux         := DecodedInsn.ALUMux.BITWISE
                io.decd.alu.bitMux      := DecodedInsn.BitMux.OR
            }
            is(Riscv.ALU_AND)  {
                io.decd.alu.mux         := DecodedInsn.ALUMux.BITWISE
                io.decd.alu.bitMux      := DecodedInsn.BitMux.AND
            }
        }
        
    } elsewhen (opcode === Riscv.MEM_OPS) {
        // Load/store operations.
        hasImm              :=  True
        when (opcode(5)) {
            immEnc          := ImmEncoding.IMM12_SW
        } otherwise {
            immEnc          := ImmEncoding.IMM12
        }
        io.decd.mem.re      := !opcode(5)
        io.decd.mem.we      :=  opcode(5)
        io.decd.mem.asize   :=  opcode(13 downto 12)
        io.decd.mem.signed  := !opcode(14)
        io.decd.usesRs1     :=  True
        io.decd.usesRs2     :=  opcode(5)
        io.decd.usesRd      := !opcode(5)
        
    }
}
