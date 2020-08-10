package main

// Status register
const (
	C byte = (1 << 0) // carry flag
	Z byte = (1 << 1) // zero flag
	I byte = (1 << 2) // interrupt disable
	D byte = (1 << 3) // decimal mode
	B byte = (1 << 4) // break command
	U byte = (1 << 5) // unused
	V byte = (1 << 6) // overflow flag
	N byte = (1 << 7) // negative flag
)

type Instruction struct {
	Name     string
	Op       func() byte
	AddrMode func() byte
	Cycles   byte
}

// NewInstruction creates a new instruction.
func NewInstruction(name string, op func() byte, addrMode func() byte, cycles byte) *Instruction {
	return &Instruction{Name: name, Op: op, AddrMode: addrMode, Cycles: cycles}
}

// CPU implements 2A03 CPU.
type CPU struct {
	a  byte   // accumulator
	x  byte   // index register X
	y  byte   // index register Y
	sp byte   // stack pointer
	pc uint16 // program counter
	p  byte   // processor status

	data    byte   // fetched data byte
	addrAbs uint16 // read absolute location
	addrRel uint16 // read relative location
	opcode  byte   // current opcode
	cycles  byte   // remaining cycles

	table [256]*Instruction

	ram *RAM
	ppu *PPU
	apu *APU
}

// NewCPU creates a new CPU.
func NewCPU() *CPU {
	cpu := CPU{}
	cpu.createTable()
	return &cpu
}

func (c *CPU) createTable() {
	c.table = [256]*Instruction{
		NewInstruction("BRK", c.brk, c.imm, 7),
		NewInstruction("ORA", c.ora, c.izx, 6),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 8),
		NewInstruction("???", c.xxx, c.imp, 3),
		NewInstruction("ORA", c.ora, c.zp0, 3),
		NewInstruction("ASL", c.asl, c.zp0, 5),
		NewInstruction("???", c.xxx, c.imp, 5),
		NewInstruction("PHP", c.php, c.imp, 3),
		NewInstruction("ORA", c.ora, c.imm, 2),
		NewInstruction("ASL", c.asl, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("ORA", c.ora, c.abs, 4),
		NewInstruction("ASL", c.asl, c.abs, 6),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("BPL", c.bpl, c.rel, 2),
		NewInstruction("ORA", c.ora, c.izy, 5),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 8),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("ORA", c.ora, c.zpx, 4),
		NewInstruction("ASL", c.asl, c.zpx, 6),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("CLC", c.clc, c.imp, 2),
		NewInstruction("ORA", c.ora, c.aby, 4),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 7),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("ORA", c.ora, c.abx, 4),
		NewInstruction("ASL", c.asl, c.abx, 7),
		NewInstruction("???", c.xxx, c.imp, 7),
		NewInstruction("JSR", c.jsr, c.abs, 6),
		NewInstruction("AND", c.and, c.izx, 6),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 8),
		NewInstruction("BIT", c.bit, c.zp0, 3),
		NewInstruction("AND", c.and, c.zp0, 3),
		NewInstruction("ROL", c.rol, c.zp0, 5),
		NewInstruction("???", c.xxx, c.imp, 5),
		NewInstruction("PLP", c.plp, c.imp, 4),
		NewInstruction("AND", c.and, c.imm, 2),
		NewInstruction("ROL", c.rol, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("BIT", c.bit, c.abs, 4),
		NewInstruction("AND", c.and, c.abs, 4),
		NewInstruction("ROL", c.rol, c.abs, 6),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("BMI", c.bmi, c.rel, 2),
		NewInstruction("AND", c.and, c.izy, 5),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 8),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("AND", c.and, c.zpx, 4),
		NewInstruction("ROL", c.rol, c.zpx, 6),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("SEC", c.sec, c.imp, 2),
		NewInstruction("AND", c.and, c.aby, 4),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 7),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("AND", c.and, c.abx, 4),
		NewInstruction("ROL", c.rol, c.abx, 7),
		NewInstruction("???", c.xxx, c.imp, 7),
		NewInstruction("RTI", c.rti, c.imp, 6),
		NewInstruction("EOR", c.eor, c.izx, 6),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 8),
		NewInstruction("???", c.xxx, c.imp, 3),
		NewInstruction("EOR", c.eor, c.zp0, 3),
		NewInstruction("LSR", c.lsr, c.zp0, 5),
		NewInstruction("???", c.xxx, c.imp, 5),
		NewInstruction("PHA", c.pha, c.imp, 3),
		NewInstruction("EOR", c.eor, c.imm, 2),
		NewInstruction("LSR", c.lsr, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("JMP", c.jmp, c.abs, 3),
		NewInstruction("EOR", c.eor, c.abs, 4),
		NewInstruction("LSR", c.lsr, c.abs, 6),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("BVC", c.bvc, c.rel, 2),
		NewInstruction("EOR", c.eor, c.izy, 5),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 8),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("EOR", c.eor, c.zpx, 4),
		NewInstruction("LSR", c.lsr, c.zpx, 6),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("CLI", c.cli, c.imp, 2),
		NewInstruction("EOR", c.eor, c.aby, 4),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 7),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("EOR", c.eor, c.abx, 4),
		NewInstruction("LSR", c.lsr, c.abx, 7),
		NewInstruction("???", c.xxx, c.imp, 7),
		NewInstruction("RTS", c.rts, c.imp, 6),
		NewInstruction("ADC", c.adc, c.izx, 6),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 8),
		NewInstruction("???", c.xxx, c.imp, 3),
		NewInstruction("ADC", c.adc, c.zp0, 3),
		NewInstruction("ROR", c.ror, c.zp0, 5),
		NewInstruction("???", c.xxx, c.imp, 5),
		NewInstruction("PLA", c.pla, c.imp, 4),
		NewInstruction("ADC", c.adc, c.imm, 2),
		NewInstruction("ROR", c.ror, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("JMP", c.jmp, c.ind, 5),
		NewInstruction("ADC", c.adc, c.abs, 4),
		NewInstruction("ROR", c.ror, c.abs, 6),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("BVS", c.bvs, c.rel, 2),
		NewInstruction("ADC", c.adc, c.izy, 5),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 8),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("ADC", c.adc, c.zpx, 4),
		NewInstruction("ROR", c.ror, c.zpx, 6),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("SEI", c.sei, c.imp, 2),
		NewInstruction("ADC", c.adc, c.aby, 4),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 7),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("ADC", c.adc, c.abx, 4),
		NewInstruction("ROR", c.ror, c.abx, 7),
		NewInstruction("???", c.xxx, c.imp, 7),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("STA", c.sta, c.izx, 6),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("STY", c.sty, c.zp0, 3),
		NewInstruction("STA", c.sta, c.zp0, 3),
		NewInstruction("STX", c.stx, c.zp0, 3),
		NewInstruction("???", c.xxx, c.imp, 3),
		NewInstruction("DEY", c.dey, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("TXA", c.txa, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("STY", c.sty, c.abs, 4),
		NewInstruction("STA", c.sta, c.abs, 4),
		NewInstruction("STX", c.stx, c.abs, 4),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("BCC", c.bcc, c.rel, 2),
		NewInstruction("STA", c.sta, c.izy, 6),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("STY", c.sty, c.zpx, 4),
		NewInstruction("STA", c.sta, c.zpx, 4),
		NewInstruction("STX", c.stx, c.zpy, 4),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("TYA", c.tya, c.imp, 2),
		NewInstruction("STA", c.sta, c.aby, 5),
		NewInstruction("TXS", c.txs, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 5),
		NewInstruction("???", c.xxx, c.imp, 5),
		NewInstruction("STA", c.sta, c.abx, 5),
		NewInstruction("???", c.xxx, c.imp, 5),
		NewInstruction("???", c.xxx, c.imp, 5),
		NewInstruction("LDY", c.ldy, c.imm, 2),
		NewInstruction("LDA", c.lda, c.izx, 6),
		NewInstruction("LDX", c.ldx, c.imm, 2),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("LDY", c.ldy, c.zp0, 3),
		NewInstruction("LDA", c.lda, c.zp0, 3),
		NewInstruction("LDX", c.ldx, c.zp0, 3),
		NewInstruction("???", c.xxx, c.imp, 3),
		NewInstruction("TAY", c.tay, c.imp, 2),
		NewInstruction("LDA", c.lda, c.imm, 2),
		NewInstruction("TAX", c.tax, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("LDY", c.ldy, c.abs, 4),
		NewInstruction("LDA", c.lda, c.abs, 4),
		NewInstruction("LDX", c.ldx, c.abs, 4),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("BCS", c.bcs, c.rel, 2),
		NewInstruction("LDA", c.lda, c.izy, 5),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 5),
		NewInstruction("LDY", c.ldy, c.zpx, 4),
		NewInstruction("LDA", c.lda, c.zpx, 4),
		NewInstruction("LDX", c.ldx, c.zpy, 4),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("CLV", c.clv, c.imp, 2),
		NewInstruction("LDA", c.lda, c.aby, 4),
		NewInstruction("TSX", c.tsx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("LDY", c.ldy, c.abx, 4),
		NewInstruction("LDA", c.lda, c.abx, 4),
		NewInstruction("LDX", c.ldx, c.aby, 4),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("CPY", c.cpy, c.imm, 2),
		NewInstruction("CMP", c.cmp, c.izx, 6),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 8),
		NewInstruction("CPY", c.cpy, c.zp0, 3),
		NewInstruction("CMP", c.cmp, c.zp0, 3),
		NewInstruction("DEC", c.dec, c.zp0, 5),
		NewInstruction("???", c.xxx, c.imp, 5),
		NewInstruction("INY", c.iny, c.imp, 2),
		NewInstruction("CMP", c.cmp, c.imm, 2),
		NewInstruction("DEX", c.dex, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("CPY", c.cpy, c.abs, 4),
		NewInstruction("CMP", c.cmp, c.abs, 4),
		NewInstruction("DEC", c.dec, c.abs, 6),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("BNE", c.bne, c.rel, 2),
		NewInstruction("CMP", c.cmp, c.izy, 5),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 8),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("CMP", c.cmp, c.zpx, 4),
		NewInstruction("DEC", c.dec, c.zpx, 6),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("CLD", c.cld, c.imp, 2),
		NewInstruction("CMP", c.cmp, c.aby, 4),
		NewInstruction("NOP", c.nop, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 7),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("CMP", c.cmp, c.abx, 4),
		NewInstruction("DEC", c.dec, c.abx, 7),
		NewInstruction("???", c.xxx, c.imp, 7),
		NewInstruction("CPX", c.cpx, c.imm, 2),
		NewInstruction("SBC", c.sbc, c.izx, 6),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 8),
		NewInstruction("CPX", c.cpx, c.zp0, 3),
		NewInstruction("SBC", c.sbc, c.zp0, 3),
		NewInstruction("INC", c.inc, c.zp0, 5),
		NewInstruction("???", c.xxx, c.imp, 5),
		NewInstruction("INX", c.inx, c.imp, 2),
		NewInstruction("SBC", c.sbc, c.imm, 2),
		NewInstruction("NOP", c.nop, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("CPX", c.cpx, c.abs, 4),
		NewInstruction("SBC", c.sbc, c.abs, 4),
		NewInstruction("INC", c.inc, c.abs, 6),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("BEQ", c.beq, c.rel, 2),
		NewInstruction("SBC", c.sbc, c.izy, 5),
		NewInstruction("???", c.xxx, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 8),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("SBC", c.sbc, c.zpx, 4),
		NewInstruction("INC", c.inc, c.zpx, 6),
		NewInstruction("???", c.xxx, c.imp, 6),
		NewInstruction("SED", c.sed, c.imp, 2),
		NewInstruction("SBC", c.sbc, c.aby, 4),
		NewInstruction("NOP", c.nop, c.imp, 2),
		NewInstruction("???", c.xxx, c.imp, 7),
		NewInstruction("???", c.xxx, c.imp, 4),
		NewInstruction("SBC", c.sbc, c.abx, 4),
		NewInstruction("INC", c.inc, c.abx, 7),
		NewInstruction("???", c.xxx, c.imp, 7),
	}
}

func (c *CPU) write(addr uint16, data byte) {
	switch {
	case addr >= 0x0000 && addr <= 0x1FFF:
		c.ram.Memory[addr] = data
	case addr >= 0x2000 && addr <= 0x2007:
		c.ppu.Memory[addr] = data
	case addr >= 0x4000 && addr <= 0x401F:
		c.apu.Memory[addr] = data
	}
}

func (c *CPU) read(addr uint16, readOnly bool) byte {
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

func (c *CPU) clock() {
	if c.cycles == 0 {
		c.opcode = c.read(c.pc, false)
		c.pc++
		c.cycles = c.table[c.opcode].Cycles
		addCycle1 := c.table[c.opcode].AddrMode()
		addCycle2 := c.table[c.opcode].Op()
		c.cycles += addCycle1 & addCycle2
	}
	c.cycles--
}

// Addressing modes
func (c *CPU) imp() byte { return 0 }
func (c *CPU) imm() byte { return 0 }
func (c *CPU) zp0() byte { return 0 }
func (c *CPU) zpx() byte { return 0 }
func (c *CPU) zpy() byte { return 0 }
func (c *CPU) rel() byte { return 0 }
func (c *CPU) abs() byte { return 0 }
func (c *CPU) abx() byte { return 0 }
func (c *CPU) aby() byte { return 0 }
func (c *CPU) ind() byte { return 0 }
func (c *CPU) izx() byte { return 0 }
func (c *CPU) izy() byte { return 0 }

// Opcodes
func (c *CPU) adc() byte { return 0 }
func (c *CPU) and() byte { return 0 }
func (c *CPU) asl() byte { return 0 }
func (c *CPU) bcc() byte { return 0 }
func (c *CPU) bcs() byte { return 0 }
func (c *CPU) beq() byte { return 0 }
func (c *CPU) bit() byte { return 0 }
func (c *CPU) bmi() byte { return 0 }
func (c *CPU) bne() byte { return 0 }
func (c *CPU) bpl() byte { return 0 }
func (c *CPU) brk() byte { return 0 }
func (c *CPU) bvc() byte { return 0 }
func (c *CPU) bvs() byte { return 0 }
func (c *CPU) clc() byte { return 0 }
func (c *CPU) cld() byte { return 0 }
func (c *CPU) cli() byte { return 0 }
func (c *CPU) clv() byte { return 0 }
func (c *CPU) cmp() byte { return 0 }
func (c *CPU) cpx() byte { return 0 }
func (c *CPU) cpy() byte { return 0 }
func (c *CPU) dec() byte { return 0 }
func (c *CPU) dex() byte { return 0 }
func (c *CPU) dey() byte { return 0 }
func (c *CPU) eor() byte { return 0 }
func (c *CPU) inc() byte { return 0 }
func (c *CPU) inx() byte { return 0 }
func (c *CPU) iny() byte { return 0 }
func (c *CPU) jmp() byte { return 0 }
func (c *CPU) jsr() byte { return 0 }
func (c *CPU) lda() byte { return 0 }
func (c *CPU) ldx() byte { return 0 }
func (c *CPU) ldy() byte { return 0 }
func (c *CPU) lsr() byte { return 0 }
func (c *CPU) nop() byte { return 0 }
func (c *CPU) ora() byte { return 0 }
func (c *CPU) pha() byte { return 0 }
func (c *CPU) php() byte { return 0 }
func (c *CPU) pla() byte { return 0 }
func (c *CPU) plp() byte { return 0 }
func (c *CPU) rol() byte { return 0 }
func (c *CPU) ror() byte { return 0 }
func (c *CPU) rti() byte { return 0 }
func (c *CPU) rts() byte { return 0 }
func (c *CPU) sbc() byte { return 0 }
func (c *CPU) sec() byte { return 0 }
func (c *CPU) sed() byte { return 0 }
func (c *CPU) sei() byte { return 0 }
func (c *CPU) sta() byte { return 0 }
func (c *CPU) stx() byte { return 0 }
func (c *CPU) sty() byte { return 0 }
func (c *CPU) tax() byte { return 0 }
func (c *CPU) tay() byte { return 0 }
func (c *CPU) tsx() byte { return 0 }
func (c *CPU) txa() byte { return 0 }
func (c *CPU) txs() byte { return 0 }
func (c *CPU) tya() byte { return 0 }
func (c *CPU) xxx() byte { return 0 }

// non-maskable interrupt request
func (c *CPU) NMI() {

}

// interrupt request
func (c *CPU) IRQ() {

}

func (c *CPU) Reset() {

}
