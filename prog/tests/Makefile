
MAKEFLAGS += --no-print-directory

TARGET_CC     ?= riscv64-linux-gnu-gcc
TARGET_CFLAGS ?= -nostartfiles -nodefaultlibs -static -march=rv64ima_zifencei_zicsr -mabi=lp64
SRCS           = $(shell echo src/*.S)
OBJS           = $(shell echo src/*.S | sed 's|src/|build/|g' | sed 's|.S||g')
HDRS           = $(shell echo *.h)

all: $(OBJS)

build/%: src/%.S rv64.ld $(HDRS)
	@mkdir -p build
	$(TARGET_CC) $(TARGET_CFLAGS) -Wl,--no-warn-rwx-segments -Trv64.ld -I. $< -o $@
