
// Copyright (c) 2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

#include "macros.h"


TEST_BEGIN
    
    addi    x1, x0, 3
    addi    x2, x0, -9
    FLUSH_PIPELINE
    REG_EXPECT(1, 3)
    REG_EXPECT(2, -9)
    
    addi    x1, x1, 3
    addi    x4, x2, -9
    FLUSH_PIPELINE
    REG_EXPECT(1, 6)
    REG_EXPECT(4, -18)
    
    add     x8, x1, x4
    sub     x9, x1, x1
    FLUSH_PIPELINE
    REG_EXPECT(8, -12)
    REG_EXPECT(9, 0)
    
TEST_END
