# Quartz

Quartz is a statically typed, concurrent programming language for the BEAM VM based on the actor model with a sleek, Ruby-based syntax and support for functional and object-oriented programming.

## Features

* Familiar Ruby-like syntax
* First-class concurrency primitives
* Actor model message passing à la Erlang
* Static typing for messages based on multiparty session types
* Duality checking
* Structural subtyping
* First-class functions

## Roadmap

- [x] AST
- [x] Pretty Printing
- [x] Core Compiler
- [X] Lexer
- [X] Parser
- [ ] FFI
- [ ] Tuples
- [ ] Instance Variables (Mutability)
- [ ] Comparison, arithmetic operators
- [ ] Module Compiler
- [X] Type.t
- [ ] Type Inference/Type Checking
- [ ] Pattern Matching
- [ ] Syntax Cleanup

## Examples

```
# Server: forall c < ...!{Start ~> ...?{Ok ~> Eps, Err(String) ~> Eps}}.
#         rec self.
#         [client : c]
#         ?client{Start ~> !client{Ok ~> self, Err(String) ~> self}}
session Server
  branch Start from sender
    case File.open("file.txt")
    when Success(f)
      sender!Ok
      File.read_into(f, |x| sender!Value(x) end)
    else
      sender!Err("Could not open file.txt!")
    end
  end
  loop
end

# Client: (target : Server) ->
#         !target{Start ~> ?target{Ok ~> Eps, Err(String) ~> Eps}}
session Client (target : Server)
  target!Start

  branch Ok from target
    branch Value(status) from target
      print(status)
    end
  or Err(e)
    print("Server-side error when opening file: " ++ e)
  end
  close # Closes by default, but can be provided explicitly. Can also use `loop`.
end

fun main
  let server = spawn Server
  spawn Client(server)
end
```

The above example compiles into the following Erlang output:

```erlang
-export([server/1, client/1, main/1]).

server(unit) ->
  receive {start, Sender} ->
    io:format("Starting~n", []),
    case file:open("file.txt") of
    {success, F} ->
       Sender!{ok, self()},
       file:read_into(F)(fun(Value) -> Sender!{{value, Value}, self()} end);
    _ ->
        Sender!{err, self()}
    end
  end.

client(Target) ->
  io:format("Starting~n", []),
  Target!{start, self()},
  
  receive
    {ok, Sender} ->
      receive {{value, Status}, Sender} ->
        print(Status),
        close(unit)
      end;
    {err, Sender} ->
      print("Server-side error when opening file"),
      close(unit)
  end.

main(_) ->
  Server = spawn(ex, server, [unit]),
  spawn(ex, client, [Server]).
```
