package cobra.cpu

import spinal.core._

case class CobraISA(
    // CPU supports (and boots in) 64-bit mode.
    RV64:           Boolean     = false,
    // Supported standard instruction sets.
    M:              Boolean     = true,
    A:              Boolean     = true,
    F:              Boolean     = true,
    C:              Boolean     = true
) {
    val D = RV64 && F
    val XLEN = if (RV64) 64 else 32
}

case class CobraPriv(
    // Supported privilege modes.
    S_mode:         Boolean     = true,
    U_mode:         Boolean     = true,
    // Supported privileged hardware.
    pmpCount:       Int         = 0,
    pmpGrain:       Int         = 2,
    hpm:            Boolean     = false
)

case class CobraCfg(
    // Supported RISC-V features.
    isa:            CobraISA    = CobraISA(),
    priv:           CobraPriv   = CobraPriv(),
    // I/O parameters.
    paddrWidth:     Int         = 32,
    irqCount:       Int         = 32,
    // Tuning parameters.
    mulLatency:     Int         = 2,
    mulPipelined:   Boolean     = false,
    divLatency:     Int         = 6,
    divPipelined:   Boolean     = false
) {
    def valitate(): Boolean = {
        if (priv.S_mode && !priv.U_mode) return false
        if (isa.D && !(isa.F && isa.RV64)) return false
        if (priv.pmpCount != 0 && priv.pmpCount != 16 && priv.pmpCount != 64) return false
        if (priv.pmpCount != 0 && (priv.pmpGrain < 2 || priv.pmpGrain >= paddrWidth - 4)) return false
        return true
    }
    val XLEN = isa.XLEN
    val orderBits = 4
}