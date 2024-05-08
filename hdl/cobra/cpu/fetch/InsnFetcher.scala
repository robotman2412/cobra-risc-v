package cobra.cpu.fetch

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import cobra.cpu._
import cobra.cpu.vmem._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._



/**
 * Instruction fetching pipeline.
 */
case class InsnFetcher(cfg: CobraCfg) extends Component {
    val io = new Bundle {
        /** Program memory interface. */
        val ibus    = master port AhbLite3Master(AhbLite3Config(cfg.vaddrWidth, 32))
        /** Instruction TLB interface. */
        val itlb    = master port VMBus(cfg, false)
        /** Fetched instructions. */
        val dout    = master port Stream(FetchedInsn(cfg))
    }
    
    /** Branch trigger. */
    val branchTrig = Bool()
    /** Branch target address. */
    val branchAddr = UInt(cfg.vaddrWidth bits)
    // TODO.
    branchTrig := False
    branchAddr.assignDontCare()
    
    /** Raw instruction stream. */
    val rawStr = Stream(FetchedInsn(cfg))
    /** Buffered instruction stream. */
    val bufStr = Stream(Vec.fill(2)(FetchedInsn(cfg)))
    
    /** Raw instruction reader. */
    val reader = new Area {
        /** Next aligned word to load. */
        val addr    = RegInit(U(cfg.entrypoint & ~3, cfg.vaddrWidth bits))
        /** Word just loaded from memory. */
        val pAddr   = Reg(UInt(cfg.vaddrWidth bits))
        /** Read was requested last cycle. */
        val reReg   = RegInit(False)
        
        /** Prevents sending invalid data after reset. */
        val validReg = RegInit(False)
        validReg := True
        
        // Request constants.
        io.ibus.HPROT       := B"1010"  /* Cachable, Unbufferable, Privileged, Instruction fetch */
        io.ibus.HMASTLOCK   := False
        io.ibus.HSIZE       := B"010"   /* 4 bytes */
        io.ibus.HWRITE      := False
        io.ibus.HBURST      := B"001"   /* Single burst */
        io.ibus.HWDATA.assignDontCare()
        
        // Memory request logic.
        io.ibus.HADDR   := addr
        when (branchTrig) {
            // Branch correction; change address.
            io.ibus.HTRANS  := AhbLite3.IDLE
            reReg           := False
            addr            := branchAddr
            pAddr.assignDontCare()
        } elsewhen (reReg && !rawStr.ready) {
            // Stream is blocked; enter wait state.
            io.ibus.HTRANS  := AhbLite3.IDLE
            reReg           := False
            io.ibus.HADDR   := pAddr
        } elsewhen (io.ibus.HREADY || !reReg) {
            // Memory is ready; issue read access.
            io.ibus.HTRANS  := AhbLite3.NONSEQ
            reReg           := True
            pAddr           := addr
            addr            := addr + 4
        } otherwise {
            // Memory is not ready.
            io.ibus.HTRANS  := AhbLite3.NONSEQ
        }
        
        // Output stream logic.
        rawStr.valid        := validReg && (!reReg || io.ibus.HREADY)
        rawStr.payload.addr := pAddr
        rawStr.payload.trap := False
        rawStr.payload.cause.assignDontCare()
        when (io.ibus.HRESP) {
            rawStr.payload.raw  := B(0, 32 bits)
        } otherwise {
            rawStr.payload.raw  := io.ibus.HRDATA
        }
    }
    
    /** Instruction buffer. */
    val buffer = if (cfg.isa.C) new Area {
        // The connector.
        rawStr.ready        := bufStr.ready
        bufStr.valid        := rawStr.valid
        bufStr.payload(0).assignDontCare()
        bufStr.payload(1)   := rawStr.payload
        
    } else new Area {
        // The buffer.
        val validReg = RegInit(False)
        val bufReg   = Reg(FetchedInsn(cfg))
        
        // Incoming stream logic.
        when (branchTrig) {
            bufReg.assignDontCare()
            validReg        := False
        } elsewhen (rawStr.valid && rawStr.ready) {
            bufReg          := rawStr.payload
            validReg        := True
        }
        rawStr.ready        := !validReg || bufStr.ready
        
        // Outgoing stream logic.
        bufStr.valid        := validReg && rawStr.valid
        bufStr.payload(0)   := bufReg
        bufStr.payload(1)   := rawStr.payload
    }
    
    /** Instruction recombination. */
    val recomb = new Area {
        /** Bit 1 of PC register. */
        val pc1  = if (cfg.isa.C) RegInit(Bool((cfg.entrypoint & 1) != 0)) else False
        
        /** Incoming instruction is 32-bit. */
        val is32 = if (cfg.isa.C) Bool() else True
        if (cfg.isa.C) {
            when (pc1) {
                is32 := bufStr.payload(0).raw(17 downto 16) === M"11"
            } otherwise {
                is32 := bufStr.payload(0).raw( 1 downto  0) === M"11"
            }
        }
        
        // Recombination logic.
        when (!is32) {
            // 16-bit instruction.
            io.dout.payload.addr                := bufStr.payload(0).addr
            io.dout.payload.addr(1)             := pc1
            io.dout.payload.raw(15 downto  0)   := pc1.mux(
                False -> bufStr.payload(0).raw(15 downto  0),
                True  -> bufStr.payload(0).raw(31 downto 16),
            )
            io.dout.payload.raw(31 downto 16)   := B(0, 16 bits)
            io.dout.payload.trap                := bufStr.payload(0).trap
            io.dout.payload.cause               := bufStr.payload(0).cause
        } elsewhen (!pc1) {
            // Aligned instruction.
            io.dout.payload := bufStr.payload(0)
        } elsewhen (bufStr.payload(0).trap) {
            // Misaligned 32-bit instruction traps on lower half.
            io.dout.payload.addr                := bufStr.payload(0).addr | 2
            io.dout.payload.raw.assignDontCare()
            io.dout.payload.trap                := True
            io.dout.payload.cause               := bufStr.payload(0).cause
        } elsewhen (bufStr.payload(1).trap) {
            // Misaligned 32-bit instruction traps on upper half.
            io.dout.payload.addr                := bufStr.payload(1).addr
            io.dout.payload.raw.assignDontCare()
            io.dout.payload.trap                := True
            io.dout.payload.cause               := bufStr.payload(1).cause
        } otherwise {
            // Misaligned 32-bit instruction.
            io.dout.payload.addr                := bufStr.payload(0).addr | 2
            io.dout.payload.raw(15 downto  0)   := bufStr.payload(0).raw(31 downto 16)
            io.dout.payload.raw(31 downto 16)   := bufStr.payload(1).raw(15 downto  0)
            io.dout.payload.trap                := False
            io.dout.payload.cause.assignDontCare()
        }
        
        // Stream logic.
        io.dout.valid := bufStr.valid && !branchTrig
        when (branchTrig) {
            if (cfg.isa.C) {
                pc1      := branchAddr(1)
            }
            bufStr.ready.assignDontCare()
        } elsewhen (!bufStr.valid) {
            bufStr.ready.assignDontCare()
        } elsewhen (!io.dout.ready) {
            bufStr.ready := False
        } elsewhen (is32) {
            bufStr.ready := True
        } otherwise {
            bufStr.ready := pc1
            if (cfg.isa.C) {
                pc1      := !pc1
            }
        }
    }
}
