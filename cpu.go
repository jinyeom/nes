package main

// Status register
const (
	_C uint8 = (1 << 0) // carry
	_Z uint8 = (1 << 1) // zero
	_I uint8 = (1 << 2) // disable interrupts
	_D uint8 = (1 << 3) // decimal mode
	_B uint8 = (1 << 4) // break
	_U uint8 = (1 << 5) // unused
	_V uint8 = (1 << 6) // overflow
	_N uint8 = (1 << 7) // negative
)

// CPU implements MOS Technology 6502.
// https://en.wikipedia.org/wiki/MOS_Technology_6502
type CPU struct {
	a  uint8  // A register
	x  uint8  // X register
	y  uint8  // Y register
	sp uint8  // stack pointer
	pc uint16 // program counter
	sr uint8  // status register

	data    uint8  // fetched data byte
	addrAbs uint16 // read absolute location
	addrRel uint16 // read relative location
	opcode  uint8  // current opcode
	cycles  uint8  // remaining cycles

	ram *RAM
	ppu *PPU
	apu *APU
}

func (c *CPU) write(addr uint16, data uint8) {
	switch {
	case addr >= 0x0000 && addr <= 0x1FFF:
		c.ram.Memory[addr] = data
	case addr >= 0x2000 && addr <= 0x2007:
		c.ppu.Memory[addr] = data
	case addr >= 0x4000 && addr <= 0x401F:
		c.apu.Memory[addr] = data
	}
}

func (c *CPU) read(addr uint16, readOnly bool) uint8 {
	switch {
	case addr >= 0x0000 && addr <= 0x1FFF:
		return c.ram.Memory[addr]
	case addr >= 0x2000 && addr <= 0x2007:
		return c.ppu.Memory[addr]
	case addr >= 0x4000 && addr <= 0x401F:
		return c.apu.Memory[addr]
	}
	return 0x00
}

// Addressing modes
func (c *CPU) imp() uint8 { return 0 }
func (c *CPU) imm() uint8 { return 0 }
func (c *CPU) zp0() uint8 { return 0 }
func (c *CPU) zpx() uint8 { return 0 }
func (c *CPU) zpy() uint8 { return 0 }
func (c *CPU) rel() uint8 { return 0 }
func (c *CPU) abs() uint8 { return 0 }
func (c *CPU) abx() uint8 { return 0 }
func (c *CPU) aby() uint8 { return 0 }
func (c *CPU) ind() uint8 { return 0 }
func (c *CPU) izx() uint8 { return 0 }
func (c *CPU) izy() uint8 { return 0 }

// Opcodes
func (c *CPU) adc() uint8 { return 0 }
func (c *CPU) and() uint8 { return 0 }
func (c *CPU) asl() uint8 { return 0 }
func (c *CPU) bcc() uint8 { return 0 }
func (c *CPU) bcs() uint8 { return 0 }
func (c *CPU) beq() uint8 { return 0 }
func (c *CPU) bit() uint8 { return 0 }
func (c *CPU) bmi() uint8 { return 0 }
func (c *CPU) bne() uint8 { return 0 }
func (c *CPU) bpl() uint8 { return 0 }
func (c *CPU) brk() uint8 { return 0 }
func (c *CPU) bvc() uint8 { return 0 }
func (c *CPU) bvs() uint8 { return 0 }
func (c *CPU) clc() uint8 { return 0 }
func (c *CPU) cld() uint8 { return 0 }
func (c *CPU) cli() uint8 { return 0 }
func (c *CPU) clv() uint8 { return 0 }
func (c *CPU) cmp() uint8 { return 0 }
func (c *CPU) cpx() uint8 { return 0 }
func (c *CPU) cpy() uint8 { return 0 }
func (c *CPU) dec() uint8 { return 0 }
func (c *CPU) dex() uint8 { return 0 }
func (c *CPU) dey() uint8 { return 0 }
func (c *CPU) eor() uint8 { return 0 }
func (c *CPU) inc() uint8 { return 0 }
func (c *CPU) inx() uint8 { return 0 }
func (c *CPU) iny() uint8 { return 0 }
func (c *CPU) jmp() uint8 { return 0 }
func (c *CPU) jsr() uint8 { return 0 }
func (c *CPU) lda() uint8 { return 0 }
func (c *CPU) ldx() uint8 { return 0 }
func (c *CPU) ldy() uint8 { return 0 }
func (c *CPU) lsr() uint8 { return 0 }
func (c *CPU) nop() uint8 { return 0 }
func (c *CPU) ora() uint8 { return 0 }
func (c *CPU) pha() uint8 { return 0 }
func (c *CPU) php() uint8 { return 0 }
func (c *CPU) pla() uint8 { return 0 }
func (c *CPU) plp() uint8 { return 0 }
func (c *CPU) rol() uint8 { return 0 }
func (c *CPU) ror() uint8 { return 0 }
func (c *CPU) rti() uint8 { return 0 }
func (c *CPU) rts() uint8 { return 0 }
func (c *CPU) sbc() uint8 { return 0 }
func (c *CPU) sec() uint8 { return 0 }
func (c *CPU) sed() uint8 { return 0 }
func (c *CPU) sei() uint8 { return 0 }
func (c *CPU) sta() uint8 { return 0 }
func (c *CPU) stx() uint8 { return 0 }
func (c *CPU) sty() uint8 { return 0 }
func (c *CPU) tax() uint8 { return 0 }
func (c *CPU) tay() uint8 { return 0 }
func (c *CPU) tsx() uint8 { return 0 }
func (c *CPU) txa() uint8 { return 0 }
func (c *CPU) txs() uint8 { return 0 }
func (c *CPU) tya() uint8 { return 0 }

func (c *CPU) clock() {
	if c.cycles == 0 {
		opcode := c.read(c.pc, false)
		c.pc++
	}
}

func (c *CPU) reset() {

}

// interrupt request
func (c *CPU) irq() {

}

// non-maskable interrupt request
func (c *CPU) nmi() {

}
