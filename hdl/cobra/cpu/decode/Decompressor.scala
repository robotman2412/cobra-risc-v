package cobra.cpu.decode

import cobra.cpu._
import spinal.core._
import cobra.Riscv

/**
 * RVC instruction decompressor.
 */
case class Decompressor(cfg: CobraCfg) extends Component {
    val io = new Bundle {
        val rv64    = cfg.isa.RV64 generate in  port Bool()
        val din     = in  port Bits(16 bits)
        
        val valid   = out port Bool()
        val dout    = out port Bits(32 bits)
    }
    
    val rv64    = if (cfg.isa.RV64) io.rv64 else False
    val isa_F   = Bool(cfg.isa.F)
    val isa_D   = Bool(cfg.isa.D)
    val insn    = io.din
    
    val funct3      = insn(15 downto 13)
    val funct4      = insn(15 downto 12)
    val funct6      = insn(15 downto 10)
    val rd_rs1_l    = insn(11 downto 7).asUInt
    val rd_l        = insn(11 downto 7).asUInt
    val rs1_l       = insn(11 downto 7).asUInt
    val rs1_s       = Cat(U"01", insn(9 downto 7)).asUInt
    val rd_rs1_s    = Cat(U"01", insn(9 downto 7)).asUInt
    val rd_s        = Cat(U"01", insn(4 downto 2)).asUInt
    val rs2_s       = Cat(U"01", insn(4 downto 2)).asUInt
    val rs2_l       = insn(6 downto 2).asUInt
    
    val uimm_c_addispn  = Cat(insn(10 downto 7), insn(12 downto 11), insn(5), insn(6), U"00").asUInt.resize(12).asSInt
    val uimm_c_mem64    = Cat(insn(6 downto 5), insn(12 downto 10), U"000").asUInt.resize(12).asSInt
    val uimm_c_mem32    = Cat(insn(5), insn(12 downto 10), insn(6), U"00").asUInt.resize(12).asSInt
    val imm_c_op_imm    = Cat(insn(12), insn(6 downto 2)).asSInt.resize(12)
    val uimm_c_op_imm   = Cat(insn(12), insn(6 downto 2)).asUInt.resize(12).asSInt
    val uimm_c_sr_i     = Cat(S"0", insn(10), S"000", insn(12), insn(6 downto 2)).asSInt
    val imm_c_jal       = Cat(insn(12), insn(8), insn(10 downto 9), insn(6), insn(7), insn(2), insn(11), insn(5 downto 3), S"0").asSInt.resize(999999)
    val imm_c_addi16sp  = Cat(insn(12), insn(4 downto 3), insn(5), insn(2), insn(6), S"0000").asSInt.resize(12)
    val imm_c_b__z      = Cat(insn(12), insn(6 downto 5), insn(2), insn(11 downto 10), insn(4 downto 3), S"0").asSInt.resize(13)
    val uimm_c_s_sp64   = Cat(insn(9 downto 7), insn(12 downto 10), U"0000").asUInt.resize(12).asSInt
    val uimm_c_s_sp32   = Cat(insn(8 downto 7), insn(12 downto 9), U"000").asUInt.resize(12).asSInt
    val uimm_c_l_sp64   = Cat(insn(4 downto 2), insn(12), insn(6 downto 5), U"0000").asUInt.resize(12).asSInt
    val uimm_c_l_sp32   = Cat(insn(3 downto 2), insn(12), insn(6 downto 4), U"000").asUInt.resize(12).asSInt
    
    // Internal re-encoding functions omit the implicit two LSB, as they are always 11.
    private def encWidthAssert(widths: (Data, Int)*) = {
        for ((thing, width) <- widths) {
            assert(thing.getBitsWidth == width, s"${thing.name} must be ${width} bits, but is ${thing.getBitsWidth}")
        }
    }
    private def enc_r(funct7: Bits, rs2: UInt, rs1: UInt, funct3: Bits, rd: UInt, op_maj: Bits): Bits = {
        encWidthAssert((funct7, 7), (rs2, 5), (rs1, 5), (funct3, 3), (rd, 5), (op_maj, 5))
        Cat(funct7, rs2, rs1, funct3, rd, op_maj)
    }
    private def enc_i(imm: SInt, rs1: UInt, funct3: Bits, rd: UInt, op_maj: Bits): Bits = {
        encWidthAssert((imm, 12), (rs1, 5), (funct3, 3), (rd, 5), (op_maj, 5))
        Cat(imm, rs1, funct3, rd, op_maj)
    }
    private def enc_s(imm: SInt, rs2: UInt, rs1: UInt, funct3: Bits, op_maj: Bits): Bits = {
        encWidthAssert((imm, 12), (rs2, 5), (rs1, 5), (funct3, 3), (op_maj, 5))
        Cat(imm(11 downto 5), rs2, rs1, funct3, imm(4 downto 0), op_maj)
    }
    private def enc_b(imm: SInt, rs2: UInt, rs1: UInt, funct3: Bits, op_maj: Bits): Bits = {
        encWidthAssert((imm, 13), (rs2, 5), (rs1, 5), (funct3, 3), (op_maj, 5))
        Cat(imm(12), imm(10 downto 5), rs2, rs1, funct3, imm(4 downto 1), imm(11), op_maj)
    }
    private def enc_u(imm: SInt, rd: UInt, op_maj: Bits): Bits = {
        encWidthAssert((imm, 20), (rd, 5), (op_maj, 5))
        Cat(imm, rd, op_maj)
    }
    private def enc_j(imm: SInt, rd: UInt, op_maj: Bits): Bits = {
        encWidthAssert((imm, 21), (rd, 5), (op_maj, 5))
        Cat(imm(20), imm(10 downto 1), imm(11), imm(19 downto 12), rd, op_maj)
    }
    
    val res = io.dout(31 downto 2)
    io.dout(1 downto 0) := B"11"
    io.valid := True
    when (insn === B(0)) {
        // All zeroes is explicitly invalid.
        res.assignDontCare()
        io.valid := False
        
        /* ======== QUADRANT 0 ======== */
    } elsewhen (insn === M"000_--------_---_00") {
        res := enc_i(uimm_c_addispn, Riscv.REG_RA, Riscv.ALU_ADD, rd_s, Riscv.OP_OP_IMM)            // C.ADDI4SPN
    } elsewhen (insn === M"001_--------_---_00"          && isa_D) {
        res := enc_i(uimm_c_mem64, rs1_s, B(3, 3 bits), rd_s, Riscv.OP_LOAD_FP)                     // C.FLD (RVD)
    } elsewhen (insn === M"010_--------_---_00") {
        res := enc_i(uimm_c_mem32, rs1_s, B(3, 2 bits), rd_s, Riscv.OP_LOAD)                        // C.LW
    } elsewhen (insn === M"011_--------_---_00" && !rv64 && isa_F) {
        res := enc_i(uimm_c_mem32, rs1_s, B(3, 2 bits), rd_s, Riscv.OP_LOAD_FP)                     // C.FLW (RV32F)
    } elsewhen (insn === M"011_--------_---_00" &&  rv64) {
        res := enc_i(uimm_c_mem64, rs1_s, B(3, 3 bits), rd_s, Riscv.OP_LOAD)                        // C.LD (RV64)
    } elsewhen (insn === M"101_--------_---_00"          && isa_D) {
        res := enc_s(uimm_c_mem64, rs2_s, rs1_s, B(3, 3 bits), Riscv.OP_STORE_FP)                   // C.FSD (RVD)
    } elsewhen (insn === M"110_--------_---_00") {
        res := enc_s(uimm_c_mem32, rs2_s, rs1_s, B(3, 2 bits), Riscv.OP_STORE)                      // C.SW
    } elsewhen (insn === M"111_--------_---_00" && !rv64 && isa_F) {
        res := enc_s(uimm_c_mem32, rs2_s, rs1_s, B(3, 2 bits), Riscv.OP_STORE_FP)                   // C.FSW (RV32F)
    } elsewhen (insn === M"111_--------_---_00" &&  rv64) {
        res := enc_s(uimm_c_mem64, rs2_s, rs1_s, B(3, 3 bits), Riscv.OP_STORE)                      // C.SD (RV64)
        
        /* ======== QUADRANT 1 ======== */
    } elsewhen (insn === M"000_-_-----_-----_01") {
        res := enc_i(imm_c_op_imm, rd_rs1_l, Riscv.ALU_ADD, rd_rs1_l, Riscv.OP_OP_IMM)              // C.ADDI
    } elsewhen (insn === M"001___-----------_01" && !rv64) {
        res := enc_j(imm_c_jal, U(1, 5 bits), Riscv.OP_JAL)                                         // C.JAL (RV32)
    } elsewhen (insn === M"001_-_-----_-----_01" &&  rv64         && rd_rs1_l =/= U(0)) {
        res := enc_i(imm_c_op_imm, rd_rs1_l, Riscv.ALU_ADD, rd_rs1_l, Riscv.OP_OP_IMM_32)           // C.ADDIW (RV64)
    } elsewhen (insn === M"010_-_-----_-----_01"                  && rd_l =/= U(0)) {
        res := enc_i(imm_c_op_imm, Riscv.REG_ZERO, Riscv.ALU_ADD, rd_l, Riscv.OP_OP_IMM)            // C.LI
    } elsewhen (insn === M"011_-_00010_-----_01") {
        res := enc_i(imm_c_addi16sp, Riscv.REG_SP, Riscv.ALU_ADD, Riscv.REG_SP, Riscv.OP_OP_IMM)    // C.ADDI16SP
    } elsewhen (insn === M"011_-_-----_-----_01"                  && rd_l =/= U(2)) {
        res := enc_u(imm_c_op_imm.resize(20), rd_l, Riscv.OP_LUI)                                   // C.LUI
    } elsewhen (insn === M"100_-_0----_-----_01") {
        res := enc_i(uimm_c_sr_i, rd_rs1_s, Riscv.ALU_SRL, rd_rs1_l, Riscv.OP_OP_IMM)               // C.SRLI / C.SRAI
    } elsewhen (insn === M"100_-_10---_-----_01") {
        res := enc_i(uimm_c_op_imm, rd_rs1_s, Riscv.ALU_AND, rd_rs1_l, Riscv.OP_OP_IMM)             // C.ANDI
    } elsewhen (insn === M"100_0_11---_00---_01") {
        res := enc_r(B"0100000", rs2_s, rd_rs1_s, Riscv.ALU_ADD, rd_rs1_s, Riscv.OP_OP)             // C.SUB
    } elsewhen (insn === M"100_0_11---_01---_01") {
        res := enc_r(B"0000000", rs2_s, rd_rs1_s, Riscv.ALU_XOR, rd_rs1_s, Riscv.OP_OP)             // C.XOR
    } elsewhen (insn === M"100_0_11---_10---_01") {
        res := enc_r(B"0000000", rs2_s, rd_rs1_s, Riscv.ALU_OR, rd_rs1_s, Riscv.OP_OP)              // C.OR
    } elsewhen (insn === M"100_0_11---_11---_01") {
        res := enc_r(B"0000000", rs2_s, rd_rs1_s, Riscv.ALU_AND, rd_rs1_s, Riscv.OP_OP)             // C.AND
    } elsewhen (insn === M"100_1_11---_00---_01" &&  rv64) {
        res := enc_r(B"0100000", rs2_s, rd_rs1_s, Riscv.ALU_ADD, rd_rs1_s, Riscv.OP_OP_32)          // C.SUBW (RV64)
    } elsewhen (insn === M"100_1_11---_01---_01" &&  rv64) {
        res := enc_r(B"0000000", rs2_s, rd_rs1_s, Riscv.ALU_ADD, rd_rs1_s, Riscv.OP_OP_32)          // C.ADDW (RV64)
    } elsewhen (insn === M"101___-----------_01") {
        res := enc_j(imm_c_jal, Riscv.REG_ZERO, Riscv.OP_JAL)                                       // C.J
    } elsewhen (insn === M"11-_---_---_-----_01") {
        res := enc_b(imm_c_b__z, Riscv.REG_ZERO, rs1_s, Cat(B"00", insn(13)), Riscv.OP_BRANCH)      // C.BEQZ / C.BNEZ
        
        /* ======== QUADRANT 2 ======== */
    } elsewhen (insn === M"000_-_-----_-----_10") {
        res := enc_i(uimm_c_op_imm, rd_rs1_l, Riscv.ALU_SLL, rd_rs1_l, Riscv.OP_OP_IMM)             // C.SLLI
    } elsewhen (insn === M"001_-_-----_-----_10"          && isa_D) {
        res := enc_i(uimm_c_l_sp64, Riscv.REG_SP, B"011", rd_l, Riscv.OP_LOAD_FP)                   // C.FLDSP
    } elsewhen (insn === M"010_-_-----_-----_10") {
        res := enc_i(uimm_c_l_sp32, Riscv.REG_SP, B"010", rd_l, Riscv.OP_LOAD)                      // C.LWSP
    } elsewhen (insn === M"011_-_-----_-----_10" && !rv64 && isa_F) {
        res := enc_i(uimm_c_l_sp32, Riscv.REG_SP, B"010", rd_l, Riscv.OP_LOAD_FP)                   // C.FLWSP (RV32)
    } elsewhen (insn === M"011_-_-----_-----_10" &&  rv64         && rd_l =/= U(0)) {
        res := enc_i(uimm_c_l_sp64, Riscv.REG_SP, B"011", rd_l, Riscv.OP_LOAD)                      // C.LDSP (RV64)
    } elsewhen (insn === M"100_0_-----_00000_10"                  && rs1_l =/= U(0)) {
        res := enc_i(S(0, 12 bits), rs1_l, B"000", Riscv.REG_ZERO, Riscv.OP_JALR)                   // C.JR
    } elsewhen (insn === M"100_0_-----_-----_10") {
        res := enc_i(S(0, 12 bits), rs2_l, Riscv.ALU_ADD, rd_l, Riscv.OP_OP_IMM)                    // C.MV
    } elsewhen (insn === M"100_1_00000_00000_10") {
        res := B"000000000001_00000_000_00000_1110011"                                              // C.EBREAK
    } elsewhen (insn === M"100_1_-----_00000_10") {
        res := enc_i(S(0, 12 bits), rs1_l, B"000", Riscv.REG_RA, Riscv.OP_JALR)                     // C.JALR
    } elsewhen (insn === M"100_1_-----_-----_10") {
        res := enc_r(B"0000000", rs2_l, rd_rs1_l, B"000", rd_rs1_l, Riscv.OP_OP)                    // C.ADD
    } elsewhen (insn === M"101__------_-----_10"          && isa_D) {
        res := enc_s(uimm_c_s_sp64, rs2_l, Riscv.REG_SP, B"011", Riscv.OP_STORE_FP)                 // C.FSDSP
    } elsewhen (insn === M"110__------_-----_10") {
        res := enc_s(uimm_c_s_sp32, rs2_l, Riscv.REG_SP, B"010", Riscv.OP_STORE)                    // C.SWSP
    } elsewhen (insn === M"111__------_-----_10" && !rv64 && isa_F) {
        res := enc_s(uimm_c_s_sp32, rs2_l, Riscv.REG_SP, B"010", Riscv.OP_STORE_FP)                 // C.SDSP (RV32)
    } elsewhen (insn === M"111__------_-----_10" &&  rv64) {
        res := enc_s(uimm_c_s_sp64, rs2_l, Riscv.REG_SP, B"011", Riscv.OP_STORE)                    // C.FSWSP (RV32)
        
    } elsewhen (insn(1 downto 0) === B"11") {
        // Not a compressed instruction.
        res.assignDontCare()
        io.valid.assignDontCare()
    } otherwise {
        // Reserved.
        res.assignDontCare()
        io.valid := False
    }
}
