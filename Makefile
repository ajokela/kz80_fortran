# Makefile for kz80_fortran - Fortran 77 Interpreter for RetroShield Z80
#
# Uses SDCC to cross-compile C to Z80 machine code

# Toolchain
CC = sdcc
AS = sdasz80
LD = sdcc
OBJCOPY = sdobjcopy
MAKEBIN = makebin

# Directories
SRCDIR = src
INCDIR = include
OBJDIR = obj
BINDIR = bin

# Target
TARGET = fortran77

# Compiler flags
CFLAGS = -mz80 --std-c11 --opt-code-size -I$(INCDIR)
# Data location must be after code ends (code ~17KB, ends around 0x4400)
# Using 0x5000 to give code room to grow
LDFLAGS = -mz80 --no-std-crt0 --code-loc 0x0100 --data-loc 0x5000

# Source files
C_SOURCES = $(wildcard $(SRCDIR)/*.c)
ASM_SOURCES = $(SRCDIR)/crt0.s

# Object files
C_OBJECTS = $(patsubst $(SRCDIR)/%.c,$(OBJDIR)/%.rel,$(C_SOURCES))
ASM_OBJECTS = $(patsubst $(SRCDIR)/%.s,$(OBJDIR)/%.rel,$(ASM_SOURCES))
OBJECTS = $(ASM_OBJECTS) $(C_OBJECTS)

# Default target
all: dirs $(BINDIR)/$(TARGET).bin

# Create directories
dirs:
	@mkdir -p $(OBJDIR) $(BINDIR)

# Compile C source files
$(OBJDIR)/%.rel: $(SRCDIR)/%.c $(wildcard $(INCDIR)/*.h)
	$(CC) $(CFLAGS) -c $< -o $@

# Assemble assembly files
$(OBJDIR)/%.rel: $(SRCDIR)/%.s
	$(AS) -o $@ $<

# Link
$(BINDIR)/$(TARGET).ihx: $(OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $(OBJECTS)

# Convert Intel HEX to binary
$(BINDIR)/$(TARGET).bin: $(BINDIR)/$(TARGET).ihx
	$(MAKEBIN) -s 32768 $< $@
	@ls -la $@

# Clean
clean:
	rm -rf $(OBJDIR) $(BINDIR)

# Show disassembly
disasm: $(BINDIR)/$(TARGET).bin
	z80dasm -a -t -g 0x0000 $<

# Run in emulator (TUI)
run: $(BINDIR)/$(TARGET).bin
	../emulator/rust/target/release/retroshield_tui $<

# Run in emulator (passthrough)
run-simple: $(BINDIR)/$(TARGET).bin
	../emulator/rust/target/release/retroshield $<

# Show code size
size: $(BINDIR)/$(TARGET).ihx
	@echo "Code and data sizes from map file:"
	@grep -E "^(HOME|CODE|DATA|BSS)" $(BINDIR)/$(TARGET).map 2>/dev/null || echo "No map file"

# Run C unit tests
test-c:
	@cd tests/c && make test

# Run FORTRAN test programs
test-fortran: $(BINDIR)/$(TARGET).bin
	@tests/fortran/run_tests.sh

# Run all tests
test: test-c test-fortran

# Run tests with coverage report
coverage:
	@cd tests/c && make coverage-report

# Generate HTML coverage report
coverage-html:
	@cd tests/c && make coverage-html
	@echo "Open tests/c/coverage_html/index.html to view"

.PHONY: all clean dirs disasm run size test test-c test-fortran coverage coverage-html
