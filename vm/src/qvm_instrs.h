#ifndef QVM_INSTRS_H
#define QVM_INSTRS_H

#include <string>
#include <memory>
#include <cstdint>
#include <variant>
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
  EXCHANGE,
  CMP,
  TCHECK,
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
  CLOSE,
  CLOSE_ERR
};

enum OperandType {
  ILiteral,
  FLiteral,
  String,
  Symbol,
  StackRef,
  FuncRef
};

struct TILiteral { std::int64_t i; };
struct TFLiteral { double f; };
struct TString { std::string s; };
struct TSymbol { std::string s; };
struct TStackRef { std::size_t s; };
struct TFuncRef { std::string f; };

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

  Operand(TILiteral);
  Operand(TFLiteral);
  Operand(TString);
  Operand(TSymbol);
  Operand(TStackRef);
  Operand(TFuncRef);

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
