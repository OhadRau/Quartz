#ifndef QVM_INSTRS_H
#define QVM_INSTRS_H

#include <string>
#include <memory>
#include <cstdint>
#include <optional>

namespace qz { namespace vm {

// TODO: REFACTOR: Make statically typed versions of ADD, SUB, etc.
enum Opcode {
  NOP,
  PUSH,
  POP,
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  AND,
  OR,
  XOR,
  NOT,
  DUP,
  SWAP,
  OVER,
  ROT,
  EXCHANGE,
  CMP,
  JEQ,
  JNE,
  JLT,
  JGT,
  JMP,
  CALL,
  RET,
  CONSTRUCT,
  CONSTRUCT_ASYNC,
  SPAWN_EMPTY,
  SPAWN_CONSTRUCT,
  SEND_MSG,
  AWAIT_MSG,
  KILL,
  CLOSE
};

enum OperandType {
  ILiteral,
  FLiteral,
  String,
  Symbol,
  StackRef,
  FuncRef
};

struct Operand {
  OperandType type;

  union {
    std::int64_t                 int_;
    double                       float_;
    std::shared_ptr<std::string> string;
    std::shared_ptr<std::string> symbol;
    std::size_t                  stackref;
    std::shared_ptr<std::string> funcref;
  };

  Operand(const Operand &o);

  Operand &operator=(const Operand &o);

  ~Operand();
};

struct Instruction {
  Opcode rator;
  std::optional<Operand> rand1;
  std::optional<Operand> rand2;
  std::optional<Operand> rand3;

  Instruction(Opcode oc);
  Instruction(Opcode oc, Operand o1);
  Instruction(Opcode oc, Operand o1, Operand o2);
  Instruction(Opcode oc, Operand o1, Operand o2, Operand o3);

  Instruction(const Instruction &i);
};

} }

#endif
