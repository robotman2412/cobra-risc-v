package cobra.cpu

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._



/**
 * Supported instruction sets configuration.
 */
case class CobraISA(
    // CPU supports (and boots in) 64-bit mode.
    RV64:           Boolean     = false,
    // Supported standard instruction sets.
    M:              Boolean     = true,
    A:              Boolean     = false,
    F:              Boolean     = false,
    D:              Boolean     = false,
    C:              Boolean     = false
) {
    assert(F || !D, "F is required when D is enabled")
    val XLEN = if (RV64) 64 else 32
    val FLEN = if (D) 64 else 32
}



/**
 * Privileged features configuration.
 */
case class CobraPriv(
    // Supported privilege modes.
    S_mode:         Boolean     = true,
    U_mode:         Boolean     = true,
    // Supported privileged hardware.
    pmpCount:       Int         = 0,
    pmpGrain:       Int         = 2,
    hpm:            Boolean     = false
) {
    assert(U_mode || !S_mode, "U-mode is required when S-mode is enabled")
    assert(pmpCount == 0 || pmpCount == 16 || pmpCount == 64, "PMP count must be 0, 16 or 64")
    assert(pmpGrain >= 2, "PMP granularity must be at least 2 bits")
}



/**
 * Memory address range.
 */
case class MemRange(
    // Base address.
    val addr: BigInt,
    // Size in bytes.
    val size: BigInt
) {
    assert(addr % 16 == 0, "Memory range address must be a multiple of 16")
    assert(size % 16 == 0, "Memory range size must be a multiple of 16")
}



/**
 * Cobra RISC-V CPU configuration parameters.
 */
case class CobraCfg(
    /* ==== Supported RISC-V features ==== */
    // Supported instruction sets.
    isa:            CobraISA    = ISA"RV64GC",
    // Supported privileged features.
    priv:           CobraPriv   = CobraPriv(),
    
    /* ==== I/O parameters ==== */
    // Maximum physical address width.
    paddrWidth:     Int         = 32,
    // Number of IRQ channels including the disabled channel 0.
    irqCount:       Int         = 32,
    
    /* ==== Pipeline topology ==== */
    // Merge multiplier and divider into one stage.
    mergeMulDiv:    Boolean     = false,
    // Merge ALU and the memory stages into one stage.
    mergeALUMem:    Boolean     = false,
    
    /* ==== Tuning parameters ==== */
    // Multiplier delay / latency.
    mulLatency:     Int         = 2,
    // Use a pipelined multiplier instead of a cyclic one.
    mulPipelined:   Boolean     = false,
    // Divider delay / latency.
    divLatency:     Int         = 6,
    // Use a pipelined divider instead of a cyclic one.
    divPipelined:   Boolean     = false
) {
    assert(priv.pmpGrain < paddrWidth, "PMP granularity must be less then address width")
    val XLEN = isa.XLEN
    val FLEN = isa.FLEN
    val orderBits = 4
}
