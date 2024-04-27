
# Copyright © 2024, Julian Scheffers, see LICENSE for info

.PHONY: all cmod vhdl sim wave clean

MAKEFLAGS += --silent

SIM  = $(shell cd hdl/cobra; find sim -name '*.scala' | sed 's|\.scala$$||')
WAVE = $(shell echo '$(SIM)' | sed 's|$$|.wave|'g)
HDL  = $(shell find hdl -name '*.scala')

.PHONY: $(SIM)

all:
	echo '$(SIM)'
	echo '$(WAVE)'

cmod:
	sbt "runMain cobranest.CmodA7Verilog"

$(SIM):
	sbt "runMain cobra.sim.$(@F)"

$(WAVE):
	sbt "runMain cobra.sim.$(shell echo '$(@F)' | sed 's|.wave$$||g')"
	gtkwave "simWorkspace/$(shell echo '$(@F)' | sed 's|.wave$$||g')$$/wave.fst"

clean:
	rm -rf tmp target simWorkspace project gen
