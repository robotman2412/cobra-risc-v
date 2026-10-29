package cobra.cpu

import spinal.core._
import cobra.cpu._
import cobra.cpu.decode._
import cobra.cpu.misc._

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0



package object execute {
    /** Represents an in-flight instruction. */
    case class InflightInsn(
        val cfg:    CobraCfg,
        val hasMem: Boolean = true,
        val hasAlu: Boolean = true,
        val hasMul: Boolean = true,
        val hasDiv: Boolean = true
    ) extends Bundle {
        /** Instruction to execut and/or pass through. */
        val insn  = DecodedInsn(hasMem, hasAlu, hasMul, hasDiv)
        /** Instruction starting virtual address. */
        val pc    = SInt(cfg.vaddrWidth bits)
        /** Value of `rs1` or `rd`. */
        val data1 = Bits(cfg.XLEN bits)
        /** Value of `rs2` or immediate. */
        val data2 = Bits(cfg.XLEN bits)
        
        def toRegWrite() = {
            val write = RegWrite(cfg.XLEN)
            write.we    := this.insn.usesRd
            write.regno := this.insn.rd
            write.data  := this.data1
            write
        }
    }
    
    /** Represents data that is pending computation. */
    case class PendData(cfg: CobraCfg) extends Bundle {
        /** Write-back is to float register. */
        val freg = cfg.isa.F generate Bool()
        /** Write-back register. */
        val rd   = UInt(5 bits)
    }
    
    /** Represents data ready after computation. */
    case class WbData(cfg: CobraCfg) extends Bundle {
        /** Write-back is to float register. */
        val freg = cfg.isa.F generate Bool()
        /** Write-back register. */
        val rd   = UInt(5 bits)
        /** Write-back result. */
        val res  = Bits(cfg.XLEN bits)
    }
    
    /** Generate a chain of one or more register with optional stall signal. */
    def registerChain[T <: Data](din: T, stages: Int, stall: Bool = False): IndexedSeq[T] = {
        val regs = for (i <- 0 until stages) yield {
            Reg(din)
        }
        when (!stall) {
            regs(0) := din
            for (i <- 1 until stages) {
                regs(i) := regs(i-1)
            }
        }
        return regs;
    }
    
    /** Generate a pipelined delay stage with optional stall signal. */
    def pipelineDelay[T <: Data](din: T, stages: Int, stall: Bool = False): T = {
        if (stages == 0) {
            return din;
        }
        return registerChain(din, stages, stall)(stages-1);
    }
}
