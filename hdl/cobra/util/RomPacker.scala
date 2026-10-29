package cobra.util

// Copyright (c) 2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra._
import spinal.core._



/** Pack little-endian bytes into a sequence of bits. */
object RomPacker {
    def packBits(data: Seq[Byte], wordBits: Int): Seq[Bits] = packBigInt(data, wordBits).map(x => B(x, wordBits bits))
    
    def packUInt(data: Seq[Byte], wordBits: Int): Seq[UInt] = packBigInt(data, wordBits).map(x => U(x, wordBits bits))
    
    def packSInt(data: Seq[Byte], wordBits: Int): Seq[SInt] = packBigInt(data, wordBits).map(x => S(x, wordBits bits))
    
    def packBigInt(data: Seq[Byte], wordBits: Int): Seq[BigInt] = {
        require(wordBits > 0, "Word width must be positive")
        val wordBytes = (wordBits - 1) / 8 + 1
        val mask = (BigInt(1) << wordBits) - 1
        data.grouped(wordBytes).map { bytes =>
            BigInt(1, bytes.reverse.toArray) & mask
        }.toVector
    }
}
