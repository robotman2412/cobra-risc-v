
MAKEFLAGS += --silent --no-print-directory

.PHONY: all build clean

all: build

build:
	mkdir -p build
	cmake -B build
	cmake --build build
	riscv32-unknown-elf-objcopy -O binary build/rom.elf build/rom.bin

clean:
	rm -rf build
