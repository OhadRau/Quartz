#include "qvm_instrs.h"

namespace qz { namespace vm {

Operand::Operand(const Operand &o) {
  this->type = o.type;
  switch (o.type) {
  case ILiteral:
    this->int_ = o.int_;
    break;
  case FLiteral:
    this->float_ = o.float_;
    break;
  case String:
    this->string = o.string;
    break;
  case Symbol:
    this->symbol = o.symbol;
    break;
  case StackRef:
    this->stackref = o.stackref;
    break;
  case FuncRef:
    this->funcref = o.funcref;
    break;
  }
}

Operand &Operand::operator=(const Operand &o) {
  this->type = o.type;
  switch (o.type) {
  case ILiteral:
    this->int_ = o.int_;
    break;
  case FLiteral:
    this->float_ = o.float_;
    break;
  case String:
    this->string = o.string;
    break;
  case Symbol:
    this->symbol = o.symbol;
    break;
  case StackRef:
    this->stackref = o.stackref;
    break;
  case FuncRef:
    this->funcref = o.funcref;
    break;
  }
}

Operand::~Operand() {
}

Instruction::Instruction(Opcode oc) {
  this->rator = oc;
}

Instruction::Instruction(Opcode oc, Operand o1) {
  this->rator = oc;
  this->rand1 = o1;
}

Instruction::Instruction(Opcode oc, Operand o1, Operand o2) {
  this->rator = oc;
  this->rand1 = o1;
  this->rand2 = o2;
}

Instruction::Instruction(Opcode oc, Operand o1, Operand o2, Operand o3) {
  this->rator = oc;
  this->rand1 = o1;
  this->rand2 = o2;
  this->rand3 = o3;
}

Instruction::Instruction(const Instruction &i) {
  this->rator = i.rator;
  this->rand1 = i.rand1;
  this->rand1 = i.rand2;
  this->rand1 = i.rand3;
}

} }
