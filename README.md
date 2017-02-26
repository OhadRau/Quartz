# Quartz

Quartz is a statically typed programming concurrent language for the BEAM VM based on the actor model with a sleek, Ruby-based syntax and support for functional and object-oriented programming.

## Features

* Familiar Ruby-like syntax
* First-class concurrency primitives
* Actor model message passing à la Erlang
* Static typing for messages based on session types
* Duality checking
* First-class functions

## Examples

```
session Server
  branch Start
    case File.open "file.txt"
    when Success f
      sender!Ok
      File.read_into f sender!Value
    else
      sender!Err
    end
  end
end

session Client (target : Server)
  initialize
    target!Start
  end

  branch Ok
    branch Value status
      print status
      close
    end
  end

  branch Err
    print "Server-side error when opening file"
    close
  end
end

fun main
  let server = spawn Server
  spawn Client server
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
