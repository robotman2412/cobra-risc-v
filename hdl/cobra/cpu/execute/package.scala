package cobra.cpu

import spinal.core._
import cobra.cpu._
import cobra.cpu.decode._

// Copyright © 2024, Julian Scheffers, see LICENSE for info



package object execute {
    /** Represents an instruction to be passed into an execution unit. */
    case class IssuedInsn(
        val cfg:    CobraCfg,
        val hasMem: Boolean = true,
        val hasAlu: Boolean = true,
        val hasMul: Boolean = true,
        val hasDiv: Boolean = true
    ) extends Bundle {
        /** Instruction to execut and/or pass through. */
        val insn  = DecdInsn(hasMem, hasAlu, hasMul, hasDiv)
        /** Value of `rs1`; left-hand operand. */
        val data1 = Bits(cfg.XLEN bits)
        /** Value of `rs2`; right-hand operand. */
        val data2 = Bits(cfg.XLEN bits)
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
