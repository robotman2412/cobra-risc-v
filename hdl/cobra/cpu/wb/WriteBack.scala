package cobra.cpu.id

import cobra.cpu.CobraCfg
import cobra.cpu.wb.Result
import spinal.core._
import spinal.lib._

/**
 * Write-Back stage:
 * Re-orders a continuous stream of instruction results into program order.
 * Instructions that do not have a result must set `regno` to 0.
 */
case class WriteBack(cfg: CobraCfg = CobraCfg(), ports: Int = 2) extends Component {
    val io = new Bundle {
        // Unordered instruction streams.
        val din     = Vec.fill(ports)(slave Stream(Result(cfg)))
        // Regfile write port.
        val commit  = out port Bool()
        val waddr   = out port UInt(5 bits)
        val dout    = out port UInt(cfg.XLEN bits)
        // Which instruction is about to be committed.
        val next    = out port UInt(cfg.orderBits bits)
    }
    
    // Next instruction to retire.
    val next     = RegInit(U(0, cfg.orderBits bits))
    io.next     := next
    // Matching instruction bitmask.
    val nextMask = Bits(ports bits)
    
    for (i <- 0 to cfg.orderBits-1) {
        // Compare available results.
        nextMask(i) := io.din(i).valid && io.din(i).payload.order === next
        // Consume the matching instruction.
        io.din(i).ready := nextMask(i)
    }
    
    when (io.commit) {
        next := next + 1
    }
    
    io.commit := nextMask =/= B(0, ports bits)
    io.waddr  := io.din.oneHotAccess(nextMask).payload.regno
    io.dout   := io.din.oneHotAccess(nextMask).payload.data
}
