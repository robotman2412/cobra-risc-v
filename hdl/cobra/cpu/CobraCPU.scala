package cobra.cpu

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import spinal.core._



/**
 * Cobra RISC-V CPU.
 */
case class CobraCPU(
    // CPU features and other parameters.
    cfg: CobraCfg,
    // I/O region list; all accesses within are in program order.
    io:  List[MemRange]
) extends Component {
}