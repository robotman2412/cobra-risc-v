package cobra

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import spinal.core._



object Riscv {
    // 32-bit opcodes
    val OP_LOAD             = U"00000"
    val OP_LOAD_FP          = U"00001"
    val OP_custom_0         = U"00010"
    val OP_MISC_MEM         = U"00011"
    val OP_OP_IMM           = U"00100"
    val OP_AUIPC            = U"00101"
    val OP_OP_IMM_32        = U"00110"
    val OP_STORE            = U"01000"
    val OP_STORE_FP         = U"01001"
    val OP_custom_1         = U"01010"
    val OP_AMO              = U"01011"
    val OP_OP               = U"01100"
    val OP_LUI              = U"01101"
    val OP_OP_32            = U"01110"
    val OP_MADD             = U"10000"
    val OP_MSUB             = U"10001"
    val OP_NMSUB            = U"10010"
    val OP_NMADD            = U"10011"
    val OP_OP_FP            = U"10100"
    val OP_custom_2         = U"10110"
    val OP_BRANCH           = U"11000"
    val OP_JALR             = U"11001"
    val OP_JAL              = U"11011"
    val OP_SYSTEM           = U"11100"
    val OP_custom_3         = U"11110"
    
    // 32-bit opcode groups
    val ALU_OPS             = M"0-1-0"
    val MEM_OPS             = M"0-00-"
    val MADD_OPS            = M"100--"
    
    // 16-bit opcodes
    val OPC_ADDI4SPN        = U"00000"
    val OPC_FLD             = U"00001"
    val OPC_LW              = U"00010"
    val OPC_FLW_LD          = U"00011"
    val OPC_FSD             = U"00101"
    val OPC_SW              = U"00110"
    val OPC_FSW_SD          = U"00111"

    val OPC_ADDI            = U"01000"
    val OPC_JAL_ADDIW       = U"01001"
    val OPC_LI              = U"01010"
    val OPC_LUI_ADDI16SP    = U"01011"
    val OPC_ALU             = U"01100"
    val OPC_J               = U"01101"
    val OPC_BEQZ            = U"01110"
    val OPC_BNEZ            = U"01111"

    val OPC_SLLI            = U"10000"
    val OPC_FLDSP           = U"10001"
    val OPC_LWSP            = U"10010"
    val OPC_FLWSP_LDSP      = U"10011"
    val OPC_JR_MV_ADD       = U"10100"
    val OPC_FSDSP           = U"10101"
    val OPC_SWSP            = U"10110"
    val OPC_FSWSP_SDSP      = U"10111"

    // ALU FUNCT3 values.
    val ALU_ADD             = U"000"
    val ALU_SLL             = U"001"
    val ALU_SLT             = U"010"
    val ALU_SLTU            = U"011"
    val ALU_XOR             = U"100"
    val ALU_SRL             = U"101"
    val ALU_OR              = U"110"
    val ALU_AND             = U"111"
}
