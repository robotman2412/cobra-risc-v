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
    // Required execution unit type.
    val exeType = DecodedInsn.ExeType()
    // Decoded immediate value.
    val imm     = SInt(32 bits)
    // Decoded branch offset.
    val branch  = SInt(32 bits)
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
    /** Required execution unit type. */
    object ExeType extends SpinalEnum {
        val MEM, ALU, MUL, DIV = newElement()
    }
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
        val branchMode  = Bool()
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
    val IMM12, IMM20, IMM12_SW, UIMM5_CSR = newElement()
}

/**
 * Instruction decoder logic.
 */
case class InsnDecoder(cfg: CobraCfg) extends Component {
    import DecodedInsn._
    val io = new Bundle {
        // Input decompressed instruction.
        val decomp  = in  port Bits(32 bits)
        // Output decoded instruction.
        val decd    = out port DecodedInsn()
    }
    
    val decomp = io.decomp
    val immEnc = ImmEncoding()
    val hasImm = Bool()
    
    // Defaults.
    io.decd.assignDontCare()
    immEnc.assignDontCare()
    hasImm := False
    
    // Register indices.
    io.decd.rs1 := decomp(19 downto 15).asUInt
    io.decd.rs2 := decomp(24 downto 20).asUInt
    io.decd.rs3 := decomp(31 downto 27).asUInt
    io.decd.rd  := decomp(11 downto  7).asUInt
    
    // Immediate value decoding.
    switch(immEnc) {
        is(ImmEncoding.IMM12) {
            io.decd.imm                 := decomp(31 downto 20).asSInt.resize(32 bits)
        }
        is(ImmEncoding.IMM12_SW) {
            io.decd.imm(31 downto  5)   := decomp(31 downto 25).asSInt.resize(27 bits)
            io.decd.imm( 4 downto  0)   := decomp(11 downto  7).asSInt
        }
        is(ImmEncoding.IMM20) {
            io.decd.imm(31 downto 12)   := decomp(31 downto 12).asSInt
            io.decd.imm(11 downto  0)   := S(0, 12 bits)
        }
        is(ImmEncoding.UIMM5_CSR) {
            io.decd.imm(31 downto  5)   := S(0, 27 bits)
            io.decd.imm( 4 downto  0)   := decomp(19 downto 15).asSInt
        }
    }
    
    // Instruction decoding.
    val opcode = decomp(6 downto 2)
    when (opcode === Riscv.ALU_OPS && decomp(5) && decomp(25)) {
        // MUL/DIV operations.
        io.decd.op32                    := decomp(3)
        io.decd.usesRd                  := True
        io.decd.usesRs1                 := True
        io.decd.usesRs2                 := True
        when (decomp(14)) {
            io.decd.exeType             := ExeType.DIV
        } otherwise {
            io.decd.exeType             := ExeType.MUL
        }
        // Multiplier bit patterns.
        switch (decomp(13 downto 12)) {
            is(B"00") {
                io.decd.mul.upper       := False
            }
            is(B"01") {
                io.decd.mul.upper       := True
                io.decd.mul.unsignedL   := False
                io.decd.mul.unsignedR   := False
            }
            is(B"10") {
                io.decd.mul.upper       := True
                io.decd.mul.unsignedL   := False
                io.decd.mul.unsignedR   := True
            }
            is(B"11") {
                io.decd.mul.upper       := True
                io.decd.mul.unsignedL   := True
                io.decd.mul.unsignedR   := True
            }
        }
        // Divider bit patterns.
        io.decd.div.remainder           := decomp(13)
        io.decd.div.unsigned            := decomp(12)
        
    } elsewhen (opcode === Riscv.ALU_OPS) {
        // ALU operations.
        hasImm                          := !decomp(5)
        immEnc                          := ImmEncoding.IMM12
        io.decd.op32                    := decomp(3)
        io.decd.usesRd                  := True
        io.decd.usesRs1                 := True
        io.decd.usesRs2                 := !hasImm
        io.decd.exeType                 := ExeType.ALU
        io.decd.alu.branchMode          := False
        switch (decomp(14 downto 12)) {
            is(Riscv.ALU_ADD)  {
                io.decd.alu.mux         := ALUMux.ADDER
                io.decd.alu.subtract    := decomp(30) && !hasImm
            }
            is(Riscv.ALU_SLL)  {
                io.decd.alu.mux         := ALUMux.SHIFTER
                io.decd.alu.shiftRight  := False
            }
            is(Riscv.ALU_SLT)  {
                io.decd.alu.mux         := ALUMux.COMPARATOR
                io.decd.alu.signed      := True
            }
            is(Riscv.ALU_SLTU) {
                io.decd.alu.mux         := ALUMux.COMPARATOR
                io.decd.alu.signed      := False
            }
            is(Riscv.ALU_XOR)  {
                io.decd.alu.mux         := ALUMux.BITWISE
                io.decd.alu.bitMux      := BitMux.XOR
            }
            is(Riscv.ALU_SRL)  {
                io.decd.alu.mux         := ALUMux.SHIFTER
                io.decd.alu.shiftRight  := False
                io.decd.alu.arithShift  := decomp(30)
            }
            is(Riscv.ALU_OR)   {
                io.decd.alu.mux         := ALUMux.BITWISE
                io.decd.alu.bitMux      := BitMux.OR
            }
            is(Riscv.ALU_AND)  {
                io.decd.alu.mux         := ALUMux.BITWISE
                io.decd.alu.bitMux      := BitMux.AND
            }
        }
        
    } elsewhen (opcode === Riscv.MEM_OPS) {
        // Load/store operations.
        hasImm                          :=  True
        when (decomp(5)) {
            immEnc                      := ImmEncoding.IMM12_SW
        } otherwise {
            immEnc                      := ImmEncoding.IMM12
        }
        io.decd.mem.re                  := !decomp(5)
        io.decd.mem.we                  :=  decomp(5)
        io.decd.mem.asize               :=  decomp(13 downto 12).asUInt
        io.decd.mem.signed              := !decomp(14)
        io.decd.usesRs1                 :=  True
        io.decd.usesRs2                 :=  decomp(5)
        io.decd.usesRd                  := !decomp(5)
        
    } elsewhen (opcode === Riscv.UIMM_OPS) {
        // AUIPC or LUI.
        hasImm                          := True
        immEnc                          := ImmEncoding.IMM20
        io.decd.alu.branchMode          := False
        when (decomp(5)) {
            // TODO: Assign PC to RHS.
            io.decd.alu.mux             := ALUMux.ADDER
            io.decd.alu.signed          := False
            io.decd.alu.subtract        := False
        } otherwise {
            io.decd.alu.mux             := ALUMux.BITWISE
            io.decd.alu.bitMux          := BitMux.LHS
        }
        
    } elsewhen (opcode === Riscv.OP_JAL) {
        // JAL.
        io.decd.alu.branchMode          := False
        io.decd.alu.subtract            := False
        io.decd.alu.signed              := False
        io.decd.alu.mux                 := ALUMux.ADDER
        // TODO: Select ALU inputs.
        // Branch offset.
        io.decd.branch(0)               := False
        io.decd.branch(19 downto 12)    := decomp(19 downto 12).asSInt
        io.decd.branch(11)              := decomp(20)
        io.decd.branch(10 downto  1)    := decomp(30 downto 21).asSInt
        io.decd.branch(31 downto 20)    := decomp(31).asSInt.resize(12 bits)
        
    } elsewhen (opcode === Riscv.OP_JALR) {
        // JALR.
        io.decd.alu.branchMode          := False
        io.decd.alu.subtract            := False
        io.decd.alu.signed              := False
        io.decd.alu.mux                 := ALUMux.ADDER
        // TODO: Select ALU inputs.
        // Branch offset.
        io.decd.branch                  := decomp(31 downto 20).asSInt.resize(32 bits)
        
    } elsewhen (opcode === Riscv.OP_BRANCH) {
        // Branch opcodes.
        io.decd.alu.branchMode          := True
        io.decd.alu.subtract            := True
        io.decd.alu.signed              := !decomp(13)
        io.decd.alu.cmpInv              := decomp(12)
        io.decd.alu.cmpLT               := decomp(14)
        // Branch offset.
        io.decd.branch(31 downto 12)    := decomp(31).asSInt.resize(20 bits)
        io.decd.branch(10 downto  5)    := decomp(30 downto 25).asSInt
        io.decd.branch( 4 downto  1)    := decomp(11 downto  8).asSInt
        io.decd.branch(11)              := decomp(7)
        
    } elsewhen (opcode === Riscv.OP_SYSTEM) {
        // TODO: SYSTEM.
    }
}
