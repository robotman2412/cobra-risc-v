
# Copyright (c) 2024-2026 Julian Scheffers
# SPDX-License-Identifier: CERN-OHL-P-2.0

.PHONY: all setup cmod vhdl sim wave clean

MAKEFLAGS += --silent

SIM  = $(shell cd hdl/cobra; find sim -name '*.scala' | sed 's|\.scala$$||')
HDL  = $(shell find hdl -name '*.scala')

.PHONY: $(SIM)

all:
	echo '$(SIM)'
	echo '$(WAVE)'

setup:
	@if [ ! -f spinalhdl/build.sbt ]; then git submodule update --init spinalhdl; fi
	cp project/plugins.sbt spinalhdl/project/metals.sbt

cmod:
	sbt "runMain cobra.nest.CmodA7Verilog"

$(SIM):
	sbt "runMain cobra.sim.$(@F)"

clean:
	rm -rf tmp target simWorkspace project/target project/project gen
