/* LPC2148 linker script source.
 *  Notes on alignment:
 *  1) Sections should be 4-byte aligned otherwise ARM fetches
 *     will be misaligned.
 *  2) The stack should be 8-byte aligned for the APCS. That's because STRD
 *     and LDRD assume that they are on 8-byte boundaries.
 *  3) Align 16 is a good idea for any copied or initialised sections because
 *     this allows the fatser LDM/STM code to run instead.
 */

/* LPC2148 */

/* include precompiled libraries */

INPUT(-lgnat)
INPUT(-lgnarl)
INPUT(-lgcc)

OUTPUT_FORMAT(elf32-littlearm)

ENTRY(entry)

/*
 * Memory definitions.
 */

MEMORY
{
  FLASH (rx) : ORIGIN = 0x00000000, LENGTH = 512k
  RAM   (rw) : ORIGIN = 0x40000000, LENGTH = 32k
}

ROM_BASE = ORIGIN(FLASH);
ROM_SIZE = LENGTH(FLASH);

RAM_BASE = ORIGIN(RAM);
RAM_SIZE = LENGTH(RAM);

SECTIONS {

. = 0;

.text : {
    KEEP (*(.rom_system_vectors));       /* RTS Startup code */
    KEEP (*(.rom_system_handlers));      /* RTS Startup code */
    KEEP (*(.rom_external_vectors));     /* External Startup code */

    . = ALIGN(16);
    * (*.text *.text.* *.glue*)

    . = ALIGN(16);
    * (*.rodata *.rodata.*)
} > FLASH

.ARM.extab   : { *(.ARM.extab* .gnu.linkonce.armextab.*) }
 > FLASH
   PROVIDE_HIDDEN (__exidx_start = .);

.ARM.exidx   : { *(.ARM.exidx* .gnu.linkonce.armexidx.*) }
 > FLASH
   PROVIDE_HIDDEN (__exidx_end = .);

. = ALIGN(16);

   /* Place for .text section for code loaded into in ram */

.ramtext : AT (.)
   {
   } > RAM

. = ALIGN(16);

_etext = . ;
   PROVIDE (etext = .);

__ramtext_load_start__ = LOADADDR(.ramtext);
__ramtext_load_end__   = __ramtext_load_start__ + SIZEOF(.ramtext);

__ramtext_ram_start__ = ADDR(.ramtext);
__ramtext_ram_end__   = __ramtext_ram_start__ + SIZEOF(.ramtext);

   /* .data section which is used for initialised data */

.data : AT (_etext)
{
   _data = . ;
    KEEP (*(.ram_interrupt_handlers_table));
    * (*.data *.data.*)
} > RAM

   . = ALIGN(16);

_edata = . ;
   PROVIDE (edata = .);

__data_load_start__ = LOADADDR(.data);
__data_load_end__ = __data_load_start__ + SIZEOF(.data);

__data_ram_start__ = ADDR(.data);
__data_ram_end__   = __data_ram_start__ + SIZEOF(.data);

__data_size__ = SIZEOF(.data) / 4;

/* bss is zero initialised data */
.bss : {
    * (*.bss *.bss.*)
    . = ALIGN(16);
} > RAM

__bss_start__ = ADDR(.bss);
__bss_end__   = (ADDR(.bss) + SIZEOF(.bss));

/* not initialised system_clock value */
.system : {
    KEEP (*(.ram_system_clock));
    . = ALIGN(16);
} > RAM

__system_start__ = ADDR(.system);

/* stack is not initialised stack space */
.stack : {
    KEEP (*(.ram_system_stack));
    . = ALIGN(16);
} > RAM

__stack_start__ = ADDR(.stack);
__stack_end__   = (ADDR(.stack) + SIZEOF(.stack));

  _end = . ;
  PROVIDE (end = .);

__free_ram_start__ = _end ;
__free_ram_end__ = RAM_BASE + RAM_SIZE;

REPORTME_RAM_TOP = _end ;
REPORTME_FREE_RAM_SIZE = __free_ram_end__ - __free_ram_start__;

  /* DWARF debug sections.
     Symbols in the DWARF debugging sections are relative to the beginning
     of the section so we begin them at 0.  */
  /* DWARF 1 */
  .debug          0 : { *(.debug) }
  .line           0 : { *(.line) }
  /* GNU DWARF 1 extensions */
  .debug_srcinfo  0 : { *(.debug_srcinfo) }
  .debug_sfnames  0 : { *(.debug_sfnames) }
  /* DWARF 1.1 and DWARF 2 */
  .debug_aranges  0 : { *(.debug_aranges) }
  .debug_pubnames 0 : { *(.debug_pubnames) }
  /* DWARF 2 */
  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }
  .debug_abbrev   0 : { *(.debug_abbrev) }
  .debug_line     0 : { *(.debug_line) }
  .debug_frame    0 : { *(.debug_frame) }
  .debug_str      0 : { *(.debug_str) }
  .debug_loc      0 : { *(.debug_loc) }
  .debug_macinfo  0 : { *(.debug_macinfo) }
  /* SGI/MIPS DWARF 2 extensions */
  .debug_weaknames 0 : { *(.debug_weaknames) }
  .debug_funcnames 0 : { *(.debug_funcnames) }
  .debug_typenames 0 : { *(.debug_typenames) }
  .debug_varnames  0 : { *(.debug_varnames) }
  .note.gnu.arm.ident 0 : { KEEP (*(.note.gnu.arm.ident)) }
  .ARM.attributes 0 : { KEEP (*(.ARM.attributes)) }
  /DISCARD/ : { *(.note.GNU-stack)  }

}
