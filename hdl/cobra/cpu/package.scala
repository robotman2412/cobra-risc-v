package cobra

// Copyright © 2024, Julian Scheffers, see LICENSE for info



package object cpu {
    /**
     * Implicit conversion from String to CobraISA.
     */
    implicit class StringToCobraISA(private val sc: StringContext) {
        def ISA(args: Any*): CobraISA = {
            var raw = sc.s(args: _*).toUpperCase().replaceAll("G", "IMAFD")
            
            // Check base ISA.
            var RV64  = raw.startsWith("RV64I")
            if (!RV64) assert(raw.startsWith("RV32I"), "ISA must start with RV32 or RV64 followed by I or G")
            raw = raw.substring(5)
            
            // Check for supported extensions.
            val supported = "MAFDC"
            for (c <- raw) {
                assert(supported.contains(c), "Extension '" + c + "' not supported by Cobra")
            }
            
            // Check order of extensions.
            val canon = "IMAFDQLCBJTPVN"
            val order = for (c <- raw) yield canon.indexOf(c)
            for (i <- 1 to order.length - 1) {
                assert(order(i-1) < order(i), "ISA string must be in canonical order")
            }
            
            return CobraISA(
                RV64,
                raw.contains('M'),
                raw.contains('A'),
                raw.contains('F'),
                raw.contains('D'),
                raw.contains('C')
            )
        }
    }
}
