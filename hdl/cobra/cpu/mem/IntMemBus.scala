package cobra.cpu.mem

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra.cpu._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import cobra.Riscv

/** Bus that interfaces between the CPU's internal L1 cache and the core. */
case class IntMemBus(cfg: CobraCfg, isData: Boolean) extends Bundle with IMasterSlave {
    val dataWidth = (if (isData) { cfg.XLEN } else { 64 });
    
    /** Enable triggers a memory access given a previous one is not currently stalled. */
    val enable = Bool()
    /** Cycle performs read access. */
    val read   = isData generate Bool()
    /** Cycle performs write access. */
    val write  = isData generate Bool()
    /** Cycle performs execute access. */
    val exec   = !isData generate Bool()
    /** Access privilege level. */
    val priv   = UInt(2 bits)
    /** Represents whether paging is enabled in SATP.MODE; false for BARE, true otherwise. */
    val pgEn   = Bool()
    /** Memory address; virtual if `priv` encodes S-mode or below and `pgEn` is true; otherwise, physical. */
    val addr   = SInt(cfg.vaddrWidth bits)
    /** Write data; if `write` is true, access shall write this to memory. */
    val wdata  = isData generate Bits(dataWidth bits)
    /** Log-base 2 of the access size in bytes. */
    val asize  = UInt(log2Up(dataWidth - 1) bits)
    // TODO: Add bus locking signals.
    
    /** Response is ready; while false, stalls the previous access. */
    val ready  = Bool()
    /** Raise a trap corresponding to `cause`. */
    val trap   = Bool()
    /** Value to be written to `mcause` or `scause` on memory access trap. */
    val cause  = UInt(4 bits)
    /** Read data; if `read` is true, memory shall be read into this. */
    val rdata  = Bits(dataWidth bits)
    
    override def asMaster(): Unit = {
        out(enable, read, write, exec, priv, pgEn, addr, wdata, asize);
        in(ready, trap, cause, rdata);
    }
    
    /** Adapt this bus directly to AHB. */
    def toAhb3Master(): AhbLite3Master = {
        val that = AhbLite3Master(AhbLite3Config(cfg.vaddrWidth, dataWidth))
        that.HADDR := this.addr.asUInt
        when (this.enable) {
            that.HTRANS := AhbLite3.NONSEQ
        } otherwise {
            that.HTRANS := AhbLite3.IDLE
        }
        that.HBURST := B"000"
        that.HPROT  := Cat(B"00", this.priv =/= U"00", Bool(isData))
        if (isData) {
            that.HWDATA := this.wdata
            that.HWRITE := this.write
        } else {
            that.HWDATA.assignDontCare
            that.HWRITE := False
        }
        that.HSIZE     := this.asize.asBits.resized
        that.HMASTLOCK := False

        this.rdata := that.HRDATA
        this.ready := that.HREADY
        this.trap  := that.HRESP
        if (isData) {
            this.cause.setAsReg
            when (this.write) {
                this.cause := Riscv.CAUSE_SACCESS
            } otherwise {
                this.cause := Riscv.CAUSE_LACCESS
            }
        } else {
            this.cause := Riscv.CAUSE_IACCESS
        }
        that
    }
}
