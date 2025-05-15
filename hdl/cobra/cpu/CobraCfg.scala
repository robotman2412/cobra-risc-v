package cobra.cpu

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import spinal.core._
import spinal.lib.bus.amba3.ahblite.AhbLite3Config
import cobra.cpu.vmem.TLBConfig



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
    /** Number of PMP entries. */
    pmpCount:       Int         = 0,
    /** Support HPM counters. */
    hpm:            Boolean     = false,
    /** Number of ASID bits for virtual memory. */
    asidLen:        Int         = 12,
) {
    assert(pmpCount == 0 || pmpCount == 16 || pmpCount == 64, "PMP count must be 0, 16 or 64")
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
    /** Multiplier latency. */
    mulLatency:     Int         = 2,
    /** Divider latency. */
    divLatency:     Int         = 6,
    
    /* ==== Cache parameters ==== */
    /** L1 ITLB configuration. */
    l1ITLB:         TLBConfig   = TLBConfig(32, 4),
    /** L1 DTLB configuration. */
    l1DTLB:         TLBConfig   = TLBConfig(32, 4),
    /** L2 TLB configuration. */
    l2TLB:          TLBConfig   = TLBConfig(32, 16),
    
    /* ==== Miscellaneous ==== */
    /** Entrypoint address at reset. */
    entrypoint:     BigInt      = 0x10000000l,
) {
    if (isa.RV64) {
        assert(paddrWidth <= 56, "Maximum supported RV64 physical address width is 56")
    } else {
        assert(paddrWidth <= 34, "Maximum supported RV32 physical address width is 34")
    }
    assert(paddrWidth >= 16, "Minimum supported physical address width is 16")
    /** Width of integer registers and CSRs. */
    val XLEN        = isa.XLEN
    /** Width of floating-point registers. */
    val FLEN        = isa.FLEN
    /** Derived maximum virtual address width. */
    val vaddrWidth  = if (isa.RV64) 56 else 32
    /** Derived maximum virtual page number width. */
    val vpnWidth    = if (isa.RV64) 45 else 20
    /** Derived maximum physical page number width. */
    val ppnWidth    = if (isa.RV64) 44 else 22
    /** Number vpn bits used per page table level. */
    val pageBits    = if (isa.RV64)  9 else 10
}
