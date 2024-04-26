
.PHONY: all verilog vhdl sim wave clean

MAKEFLAGS += --silent

SIM  = $(shell cd hdl/cobra; find sim -name '*.scala' | sed 's|\.scala$$||')
WAVE = $(shell echo '$(SIM)' | sed 's|sim/|wave/|'g)
HDL  = $(shell find hdl -name '*.scala')

.PHONY: $(SIM)

all:
	echo '$(SIM)'
	echo '$(WAVE)'

verilog:
	sbt "runMain cobra.TopVerilog"

vhdl:
	sbt "runMain cobra.TopVhdl"

$(SIM):
	sbt "runMain cobra.sim.$(@F)"

$(WAVE):
	sbt "runMain cobra.sim.$(@F)"
	gtkwave "simWorkspace/$(@F)$$/wave.fst"

clean:
	rm -rf tmp target simWorkspace project
