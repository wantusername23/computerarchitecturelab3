// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleCPU(implicit val conf: CPUConfig) extends Module {
  val io = IO(new CoreIO())
  io := DontCare

  // All of the structures required
  val pc         = RegInit(0.U)
  val control    = Module(new Control())
  val registers  = Module(new RegisterFile())
  val aluControl = Module(new ALUControl())
  val alu        = Module(new ALU())
  val immGen     = Module(new ImmediateGenerator())
  val branchCtrl = Module(new BranchControl())
  val pcPlusFour = Module(new Adder())
  val branchAdd  = Module(new Adder())
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  // To make the FIRRTL compiler happy. Remove this as you connect up the I/O's
//  control.io    := DontCare
//  immGen.io     := DontCare
//  branchCtrl.io := DontCare
//  branchAdd.io  := DontCare

  io.imem.address := pc

  pcPlusFour.io.inputx := pc
  pcPlusFour.io.inputy := 4.U

  val instruction = io.imem.instruction

  control.io.opcode := instruction(6, 0)

  registers.io.readreg1 := instruction(19,15)
  registers.io.readreg2 := instruction(24,20)

  val writereg = instruction(11,7)
  registers.io.writereg := writereg
  registers.io.wen :=  Mux(writereg === 0.U, false.B, true.B)

  immGen.io.instruction := instruction

//  aluControl.io.add       := false.B
//  aluControl.io.immediate := false.B
  aluControl.io.add       := control.io.add
  aluControl.io.immediate := control.io.immediate
  aluControl.io.funct7    := instruction(31,25)
  aluControl.io.funct3    := instruction(14,12)

  val aluInputX = MuxLookup(control.io.alusrc1, 0.U, Array(
    0.U -> registers.io.readdata1,
    1.U -> 0.U,
    2.U -> pc
  ))

  // inputy: immediate or reg2
  val aluInputY = Mux(control.io.immediate, immGen.io.sextImm, registers.io.readdata2)

  alu.io.operation := aluControl.io.operation
  alu.io.inputx := registers.io.readdata1
  alu.io.inputy := registers.io.readdata2

  io.dmem.address   := alu.io.result
  io.dmem.writedata := registers.io.readdata2
  io.dmem.memread   := control.io.memread
  io.dmem.memwrite  := control.io.memwrite
  io.dmem.maskmode  := instruction(13, 12)
  io.dmem.sext      := !instruction(14)
  val writeData = MuxLookup(control.io.toreg, 0.U, Array(
    0.U -> alu.io.result,
    1.U -> io.dmem.readdata,
    2.U -> pcPlusFour.io.result
  ))

  registers.io.writedata := alu.io.result

  branchCtrl.io.branch := control.io.branch
  branchCtrl.io.funct3 := instruction(14, 12)
  branchCtrl.io.inputx := registers.io.readdata1
  branchCtrl.io.inputy := registers.io.readdata2

  //pc := pcPlusFour.io.result
//  pcPlusFour.io.inputx := pc
//  pcPlusFour.io.inputy := 4.U

  // Branch Target = PC + Immediate
  branchAdd.io.inputx := pc
  branchAdd.io.inputy := immGen.io.sextImm
  val pcPlusImm = branchAdd.io.result

  // Next PC Selection
  val next_pc = Wire(UInt(32.W))

  when (control.io.jump === 3.U) { // JALR
    // PC = (rs1 + imm) & ~1
    next_pc := (registers.io.readdata1 + immGen.io.sextImm) & ~1.U(32.W)
  } .elsewhen (control.io.jump === 2.U) { // JAL
    // PC = PC + imm
    next_pc := pcPlusImm
  } .elsewhen (branchCtrl.io.taken) { // Branch Taken
    // PC = PC + imm
    next_pc := pcPlusImm
  } .otherwise {
    // PC = PC + 4
    next_pc := pcPlusFour.io.result
  }

  pc := next_pc


  // Debug / pipeline viewer
  val structures = List(
    (control, "control"),
    (registers, "registers"),
    (aluControl, "aluControl"),
    (alu, "alu"),
    (immGen, "immGen"),
    (branchCtrl, "branchCtrl"),
    (pcPlusFour, "pcPlusFour"),
    (branchAdd, "branchAdd")
  )

  printf("DASM(%x)\n", instruction)
  printf(p"CYCLE=$cycleCount\n")
  printf(p"pc: $pc\n")
  for (structure <- structures) {
    printf(p"${structure._2}: ${structure._1.io}\n")
  }
  printf("\n")

}
