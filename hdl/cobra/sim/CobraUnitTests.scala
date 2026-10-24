package cobra.sim

// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

import cobra._
import cobra.cpu._
import cobra.cpu.fetch._
import cobra.util._
import java.io.File
import net.fornwall.jelf._
import scala.collection.mutable.ArrayBuffer
import scala.sys.process.Process
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._

case class CobraUnitTestBench(cfg: CobraCfg, ramSize: Long) extends Component {
    val io = new Bundle {
    }
    
    val ram = new AlignedAhb3Ram(AhbLite3Config(cfg.vaddrWidth, 64), ramSize)
    val cpu = CobraCpu(cfg)
    cpu.io.ibus.toAhbLite3 <> ram.io.ahb
}

object CobraUnitTests extends App {
    import CobraUnitTests._
    
    val cfg = CobraCfg(entrypoint = 0x1000)
    val ramSize = 0x10000
    // val compiled = Config.sim.compile(CobraUnitTestBench(cfg, ramSize))
    
    case class RegPreload(regno: Int, value: BigInt)
    case class RegExpect(addr: Long, regno: Int, value: BigInt)
    case class Segment(addr: Long, bytes: Array[Byte])
    case class TestCase(rom: Seq[Segment], preload: Seq[RegPreload], expect: Seq[RegExpect])
    
    object TestCase {
        private def read4(array: Array[Byte], index: Int): Long = {
            ((array(index + 0).toLong & 255) << 0) | 
            ((array(index + 1).toLong & 255) << 8) | 
            ((array(index + 2).toLong & 255) << 16) | 
            ((array(index + 3).toLong & 255) << 24)
        }
        
        private def read8(array: Array[Byte], index: Int): BigInt = {
            ((array(index + 0).toLong & 255) << 0) | 
            ((array(index + 1).toLong & 255) << 8) | 
            ((array(index + 2).toLong & 255) << 16) | 
            ((array(index + 3).toLong & 255) << 24) | 
            ((array(index + 4).toLong & 255) << 32) | 
            ((array(index + 5).toLong & 255) << 40) | 
            ((array(index + 6).toLong & 255) << 48) | 
            (BigInt(array(index + 7).toLong & 255) << 56)
        }
        
        def from(file: File): TestCase = {
            val elf = ElfFile.from(file)
            
            var rom = new ArrayBuffer[Segment]()
            var preload = new ArrayBuffer[RegPreload]()
            var expect = new ArrayBuffer[RegExpect]()
            
            for (i <- 0 until elf.e_phnum) {
                val phdr = elf.getProgramHeader(i)
                if (phdr.p_type == ElfSegment.PT_LOAD) {
                    rom += Segment(phdr.p_vaddr, phdr.getData())
                }
            }
            
            val preloadSect = elf.firstSectionByName("reg_preload")
            if (preloadSect != null) {
                val bytes = preloadSect.getData()
                val entrySize = 16
                for (i <- 0 until bytes.length / entrySize) {
                    preload += RegPreload(
                        bytes(i * entrySize + 0).toInt,
                        read8(bytes, i * entrySize + 8)
                    )
                }
            }
            
            val expectSect = elf.firstSectionByName("reg_expect")
            if (expectSect != null) {
                val bytes = expectSect.getData()
                val entrySize = 16
                for (i <- 0 until bytes.length / entrySize) {
                    expect += RegExpect(
                        read4(bytes, i * entrySize + 0),
                        bytes(i * entrySize + 4).toInt,
                        read8(bytes, i * entrySize + 8)
                    )
                }
            }
            
            assert(elf.e_entry == 0x1000, f"ELF entrypoint must be 0x1000, is 0x${elf.e_entry}%x")
            
            TestCase(rom, preload, expect)
        }
    }
    
    Process("make -C prog/tests").!!
    
    for (file <- new File("prog/tests/build").listFiles()) {
        val testCase = TestCase.from(file)
        
        printf("Test %s:\n", file.getName())
        
        printf("  %d segments:\n", testCase.rom.length)
        for (segment <- testCase.rom) {
            printf("    %d bytes at 0x%x\n", segment.bytes.length, segment.addr)
        }
        
        printf("  %d pre-loaded registers:\n", testCase.preload.length)
        for (preload <- testCase.preload) {
            printf("    x%d = 0x%x  %d  %d\n", preload.regno, preload.value, preload.value, preload.value.toLong)
        }
        
        printf("  %d register values expected:\n", testCase.expect.length)
        for (expect <- testCase.expect) {
            printf("    px = 0x%x  =>  x%d = 0x%x  %d  %d\n", expect.addr, expect.regno, expect.value, expect.value, expect.value.toLong)
        }
    }
}
