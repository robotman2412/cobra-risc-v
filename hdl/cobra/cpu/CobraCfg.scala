package cobra.cpu

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra.cpu._
import spinal.core._
import spinal.lib.bus.amba3.ahblite.AhbLite3Config
import cobra.cpu.mem.TLBConfig



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
    assert(RV64 || !D, "RVD without RV64 is unsupported")
    assert(F || !D, "F is required when D is enabled")
    val XLEN = if (RV64) 64 else 32
    val FLEN = if (D) 64 else 32
}



/**
 * Cobra RISC-V CPU configuration parameters.
 */
case class CobraCfg(
    /* ==== Supported RISC-V features ==== */
    /** Supported instruction sets. */
    isa:            CobraISA    = ISA"RV64GC",
    /** Number of paging levels. */
    pagingLevels:   Int         = 3,
    /** Number of ASID bits for virtual memory. */
    asidLen:        Int         = 8,
    /** Number of PMP entries. */
    pmpCount:       Int         = 0,
    /** Support HPM counters. */
    hpm:            Boolean     = false,
    
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
    assert(pmpCount == 0 || pmpCount == 16 || pmpCount == 64, "PMP count must be 0, 16 or 64")
    if (isa.RV64) {
        assert(paddrWidth <= 56, "Maximum supported RV64 physical address width is 56")
        assert(asidLen <= 16, "Maximum supported RV64 ASIDLEN is 16")
        assert(pagingLevels >= 3 && pagingLevels <= 5, "RV64 paging levels must be from 3 to 5 inclusive")
    } else {
        assert(paddrWidth <= 32, "Maximum supported RV32 physical address width is 32")
        assert(asidLen <= 9, "Maximum supported RV32 ASIDLEN is 9")
        assert(pagingLevels == 2, "RV32 paging levels must be exactly 2")
    }
    assert(paddrWidth >= 16, "Minimum supported physical address width is 16")
    assert(entrypoint % 2 == 0, "Entrypoint must be aligned to 2 bytes")
    assert(entrypoint < (1 << paddrWidth), "Entrypoint address does not fit in physical address width")
    /** Width of integer registers and CSRs. */
    val XLEN            = isa.XLEN
    /** Width of floating-point registers. */
    val FLEN            = isa.FLEN
    /** Number vpn bits used per page table level. */
    val bitsPerPTLevel  = if (isa.RV64)  9 else 10
    /** Derived maximum virtual address width. */
    val vaddrWidth      = 12 + bitsPerPTLevel * pagingLevels
    /** Derived maximum virtual page number width. */
    val vpnWidth        = vaddrWidth - 12
    /** Derived maximum physical page number width. */
    val ppnWidth        = paddrWidth - 12
}
