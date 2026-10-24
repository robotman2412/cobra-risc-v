OUTPUT_FORMAT(elf64-littleriscv)
ENTRY(_start)

PHDRS {
    ram PT_LOAD FLAGS(7);
}

SECTIONS {
    .text 0x1000 : {
        *(.text) *(.text.*)
    } :ram
    
    .rodata 0x2000 : {
        *(.rodata) *(.rodata.*)
    } :ram
    
    .data 0x3000 : {
        *(.rodata) *(.rodata.*)
    } :ram
    
    reg_expect : {
        *(reg_expect) *(reg_expect.*)
    } :NONE
    
    reg_preload : {
        *(reg_preload) *(reg_preload.*)
    } :NONE
}
