package cobra

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import spinal.core._



object Riscv {
    // 32-bit opcodes
    def OP_LOAD             = B"00000"
    def OP_LOAD_FP          = B"00001"
    def OP_custom_0         = B"00010"
    def OP_MISC_MEM         = B"00011"
    def OP_OP_IMM           = B"00100"
    def OP_AUIPC            = B"00101"
    def OP_OP_IMM_32        = B"00110"
    def OP_STORE            = B"01000"
    def OP_STORE_FP         = B"01001"
    def OP_custom_1         = B"01010"
    def OP_AMO              = B"01011"
    def OP_OP               = B"01100"
    def OP_LUI              = B"01101"
    def OP_OP_32            = B"01110"
    def OP_MADD             = B"10000"
    def OP_MSUB             = B"10001"
    def OP_NMSUB            = B"10010"
    def OP_NMADD            = B"10011"
    def OP_OP_FP            = B"10100"
    def OP_custom_2         = B"10110"
    def OP_BRANCH           = B"11000"
    def OP_JALR             = B"11001"
    def OP_JAL              = B"11011"
    def OP_SYSTEM           = B"11100"
    def OP_custom_3         = B"11110"
    
    // 32-bit opcode groups
    def ALU_OPS             = M"0-1-0"
    def MEM_OPS             = M"0-00-"
    def UIMM_OPS            = M"0-101"
    def MADD_OPS            = M"100--"
    
    // 16-bit opcodes
    def OPC_ADDI4SPN        = B"00000"
    def OPC_FLD             = B"00001"
    def OPC_LW              = B"00010"
    def OPC_FLW_LD          = B"00011"
    def OPC_FSD             = B"00101"
    def OPC_SW              = B"00110"
    def OPC_FSW_SD          = B"00111"

    def OPC_ADDI            = B"01000"
    def OPC_JAL_ADDIW       = B"01001"
    def OPC_LI              = B"01010"
    def OPC_LUI_ADDI16SP    = B"01011"
    def OPC_ALU             = B"01100"
    def OPC_J               = B"01101"
    def OPC_BEQZ            = B"01110"
    def OPC_BNEZ            = B"01111"

    def OPC_SLLI            = B"10000"
    def OPC_FLDSP           = B"10001"
    def OPC_LWSP            = B"10010"
    def OPC_FLWSP_LDSP      = B"10011"
    def OPC_JR_MV_ADD       = B"10100"
    def OPC_FSDSP           = B"10101"
    def OPC_SWSP            = B"10110"
    def OPC_FSWSP_SDSP      = B"10111"

    // ALU FUNCT3 values.
    def ALU_ADD             = B"000"
    def ALU_SLL             = B"001"
    def ALU_SLT             = B"010"
    def ALU_SLTU            = B"011"
    def ALU_XOR             = B"100"
    def ALU_SRL             = B"101"
    def ALU_OR              = B"110"
    def ALU_AND             = B"111"
    
    // Trap cause values.
    def CAUSE_IALIGN        = U(0, 5 bits)
    def CAUSE_IACCESS       = U(1, 5 bits)
    def CAUSE_IILLEGAL      = U(2, 5 bits)
    def CAUSE_BREAK         = U(3, 5 bits)
    def CAUSE_LALIGN        = U(4, 5 bits)
    def CAUSE_LACCESS       = U(5, 5 bits)
    def CAUSE_SALIGN        = U(6, 5 bits)
    def CAUSE_SACCESS       = U(7, 5 bits)
    def CAUSE_ECALL_U       = U(8, 5 bits)
    def CAUSE_ECALL_S       = U(9, 5 bits)
    def CAUSE_ECALL_M       = U(11, 5 bits)
    def CAUSE_IPAGE         = U(12, 5 bits)
    def CAUSE_LPAGE         = U(13, 5 bits)
    def CAUSE_SPAGE         = U(15, 5 bits)
    def CAUSE_DOUBLETRAP    = U(16, 5 bits)
    def CAUSE_SWCHECK       = U(18, 5 bits)
    def CAUSE_HWERR         = U(19, 5 bits)
}
