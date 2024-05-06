package cobra.cpu

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._
import spinal.lib.bus.amba3.ahblite.AhbLite3Config



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
    /** Supported instruction sets. */
    isa:            CobraISA    = ISA"RV64GC",
    /** Supported privileged features. */
    priv:           CobraPriv   = CobraPriv(),
    
    /* ==== I/O parameters ==== */
    /** Maximum physical address width. */
    paddrWidth:     Int         = 32,
    /** Number of IRQ channels including the disabled channel 0. */
    irqCount:       Int         = 32,
    
    /* ==== Pipeline topology ==== */
    /** Merge multiplier and divider into one stage. */
    mergeMulDiv:    Boolean     = false,
    /** Merge ALU and the memory stages into one stage. */
    mergeALUMem:    Boolean     = false,
    
    /* ==== Tuning parameters ==== */
    /** Multiplier delay / latency. */
    mulLatency:     Int         = 2,
    /** Use a pipelined multiplier instead of a cyclic one. */
    mulPipelined:   Boolean     = false,
    /** Divider delay / latency. */
    divLatency:     Int         = 6,
    /** Use a pipelined divider instead of a cyclic one. */
    divPipelined:   Boolean     = false,
    
    /* ==== Miscellaneous ==== */
    /** Entrypoint address at reset. */
    entrypoint:     BigInt      = 0x10000000l,
) {
    assert(priv.pmpGrain < paddrWidth, "PMP granularity must be less then address width")
    if (isa.RV64) {
        assert(paddrWidth <= 56, "Maximum supported RV64 physical address width is 56")
    } else if (priv.S_mode) {
        assert(paddrWidth <= 34, "Maximum supported RV32 with S-mode physical address width is 34")
    } else {
        assert(paddrWidth <= 32, "Maximum supported RV32 without S-mode physical address width is 32")
    }
    assert(paddrWidth >= 16, "Minimum supported physical address width is 16")
    /** Width of integer registers and CSRs. */
    val XLEN        = isa.XLEN
    /** Width of floating-point registers. */
    val FLEN        = isa.FLEN
    /** Number of bits required to uniquely represent in-flight instruction ordering. */
    val orderBits   = 4
    /** Derived maximum virtual address width. */
    val vaddrWidth  = if (!priv.S_mode) paddrWidth else if (isa.RV64) 56 else 32
    /** Derived maximum virtual page number width. */
    val vpnWidth    = if (!priv.S_mode) paddrWidth-12 else if (isa.RV64) 45 else 20
    /** Derived maximum physical page number width. */
    val ppnWidth    = if (!priv.S_mode) paddrWidth-12 else if (isa.RV64) 44 else 22
}
