#include "qvm_instrs.h"

namespace qz { namespace vm {

Operand::Operand(TILiteral i) {
  this->type = ILiteral;
  this->int_ = i.i;
}
Operand::Operand(TFLiteral f) {
  this->type = FLiteral;
  this->float_ = f.f;
}
Operand::Operand(TString s) : string{} {
  this->type = String;
  this->string = std::make_shared<std::string>(s.s);
}
Operand::Operand(TSymbol s) : symbol{} {
  this->type = Symbol;
  this->symbol = std::make_shared<std::string>(s.s);
}
Operand::Operand(TStackRef s) {
  this->type = StackRef;
  this->stackref = s.s;
}
Operand::Operand(TFuncRef f) : funcref{} {
  this->type = FuncRef;
  this->funcref = std::make_shared<std::string>(f.f);
}

// WARNING: unions don't allow multiple initialization.
// However, if we don't initialize the shared_ptr's this
// leads to undefined behavior and segfaults in some
// cases. To get around this, I only initialize string.
// This ONLY works because string, symbol, and funcref
// are all shared_ptr<string>.
Operand::Operand(const Operand &o) : string{} {
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
  switch (this->type) {
  case String:
    this->string.~shared_ptr();
    break;
  case Symbol:
    this->symbol.~shared_ptr();
    break;
  case FuncRef:
    this->funcref.~shared_ptr();
    break;
  }
}

Instruction::Instruction(Opcode oc) : rand1{}, rand2{}, rand3{} {
  this->rator = oc;
}

Instruction::Instruction(Opcode oc, Operand o1) : rand1{}, rand2{}, rand3{} {
  this->rator = oc;
  this->rand1 = std::optional<Operand>{o1};
}

Instruction::Instruction(Opcode oc, Operand o1, Operand o2) : rand1{}, rand2{}, rand3{} {
  this->rator = oc;
  this->rand1 = std::optional<Operand>{o1};
  this->rand2 = std::optional<Operand>{o2};
}

Instruction::Instruction(Opcode oc, Operand o1, Operand o2, Operand o3) : rand1{}, rand2{}, rand3{} {
  this->rator = oc;
  this->rand1 = std::optional<Operand>{o1};
  this->rand2 = std::optional<Operand>{o2};
  this->rand3 = std::optional<Operand>{o3};
}

Instruction::Instruction(const Instruction &i) : rand1{}, rand2{}, rand3{} {
  this->rator = i.rator;
  this->rand1 = i.rand1;
  this->rand2 = i.rand2;
  this->rand3 = i.rand3;
}

} }
