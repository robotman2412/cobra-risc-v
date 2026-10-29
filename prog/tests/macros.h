
// Copyright (c) 2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

#pragma once



// Enough no-ops to flush the entire pipeline.
// 10 no-ops; 7-deep pipeline minus the 2 load cycles.
#define FLUSH_PIPELINE  \
    nop; nop;           \
    nop; nop;           \
    nop; nop;           \
    nop; nop;           \
    nop; nop

// Set register value to be pre-loaded before execution begins.
#define REG_PRELOAD(regno, value)   \
    .pushsection "reg_preload";     \
    .byte   regno;                  \
    .byte   0;                      \
    .byte   0;                      \
    .byte   0;                      \
    .byte   0;                      \
    .byte   0;                      \
    .byte   0;                      \
    .byte   0;                      \
    .8byte  value;                  \
    .popsection

// Note expected register value asserted by the harness.
#define REG_EXPECT(regno, value)    \
9:                                  \
    .pushsection "reg_expect";      \
    .4byte  9b;                     \
    .byte   regno;                  \
    .byte   0;                      \
    .byte   0;                      \
    .byte   0;                      \
    .8byte  value;                  \
    .popsection

// Mark the beginning of the test.
#define TEST_BEGIN                      \
    .globl _start;                      \
    .type _start, %function;            \
    .section ".text", "ax", @progbits;  \
_start:

// Mark the end of the test.
#define TEST_END            \
    .size _start, .-_start; \
__test_end:                 \
    j __test_end;           \
    .size __test_end, .-__test_end
