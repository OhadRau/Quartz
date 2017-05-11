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

## Examples

```
# Server: forall c < ...!{start: ~> ...?{ok: ~> Eps, err: ~> Eps}}.
#         [client : c]
#         ?client{start: ~> !client{ok: ~> Eps, err: ~> Eps}}
session Server
  branch start from sender
    case File.open "file.txt"
    when Success f
      sender!ok
      File.read_into f sender!Value
    else
      sender!err
    end
  end
end

# Client: (target : Server) ->
#         !target{start: ~> ?target{ok: ~> Eps, err: ~> Eps}}
session Client (target : Server)
  init
    target!start
  end

  branch ok from target
    branch Value status from target
      print status
      close
    end
  end

  branch err from target
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
