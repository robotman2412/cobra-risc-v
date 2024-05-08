package cobra.cpu.decode

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import cobra.cpu.decode.DecodedInsn.ExeType
import spinal.core._



/**
 * Decoded instruction data.
 */
case class DecodedInsn(
    val hasMem: Boolean = true,
    val hasAlu: Boolean = true,
    val hasMul: Boolean = true,
    val hasDiv: Boolean = true
) extends Bundle {
    // Register indices.
    val rs1     = UInt(5 bits)
    val rs2     = UInt(5 bits)
    val rs3     = UInt(5 bits)
    val rd      = UInt(5 bits)
    // Register source shorthand.
    def rs(index: Int): UInt = index match {
        case 0 => rs1
        case 1 => rs2
        case 2 => rs3
        case _ => U"00000"
    }
    // Register usage.
    val usesRs1 = Bool()
    val usesRs2 = Bool()
    val usesRs3 = Bool()
    val usesRd  = Bool()
    // Register source usage shorthand.
    def usesRs(index: Int): Bool = index match {
        case 0 => usesRs1
        case 1 => usesRs2
        case 2 => usesRs3
        case _ => False
    }
    // Required execution unit type.
    val exeType = DecodedInsn.ExeType()
    // Decoded immediate value.
    val imm     = SInt(32 bits)
    // Decoded branch offset.
    val branch  = SInt(32 bits)
    // Float operation size.
    val fpSize  = UInt(2 bits)
    // Memory access control signals.
    val mem     = hasMem generate DecodedInsn.Mem()
    // Perform 32-bit integer operation.
    val op32    = Bool()
    // ALU control signals.
    val alu     = hasAlu generate DecodedInsn.ALU()
    // Multiplier control signals.
    val mul     = hasMul generate DecodedInsn.Mul()
    // Divider control signals.
    val div     = hasDiv generate DecodedInsn.Div()
}

object DecodedInsn {
    /** Generate DecodedInsn from capabilities list. */
    def apply(capabilities: Seq[SpinalEnumElement[ExeType.type]]): DecodedInsn = {
        return DecodedInsn(
            capabilities.contains(ExeType.MEM),
            capabilities.contains(ExeType.ALU),
            capabilities.contains(ExeType.MUL),
            capabilities.contains(ExeType.DIV)
        )
    }
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