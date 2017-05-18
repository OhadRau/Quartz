:- set_prolog_flag(occurs_check, true).

type(X, T) :- type([], X, T).

type(Gamma, var(X), U) :- contains(Gamma, X:T), instantiate(T, U).
type(Gamma, lam(X, E), T -> U) :- type([X:mono(T) | Gamma], E, U).
type(Gamma, app(F, X), U) :- type(Gamma, F, T -> U), type(Gamma, X, T).
type(Gamma, let(X, E0, E1), T) :- type(Gamma, E0, U), type([X:poly(Gamma, U) | Gamma], E1, T).

type(Gamma, sendMsg(Who, Msg, Args, Then), sendMsg(Who, [Msg-(AT, TT)])) :-
  type(Gamma, Who, W),
  compatible(sendMsg(Who, [Msg-(AT, TT)]), W),
  type(Gamma, Args, AT),
  type(Gamma, Then, TT).

type(Gamma, recvMsg(Who, Msg, Args, Then), recvMsg(Who, [Msg-(AT, TT)])) :-
  compatible(recvMsg(Who, [Msg-(AT, TT)]), W),
  % FIXME: What to do if Who is already bound? Conditional breaks this.
  Gamma1 = [Who:W, Args:mono(AT) | Gamma],
  type(Gamma1, Then, TT).

type(Gamma, [E], T) :- type(Gamma, E, T).

type(Gamma, [E | Es], sendMsg(Who, Msgs)) :-
  type(Gamma, E, sendMsg(Who, M)),
  type(Gamma, Es, sendMsg(Who, Ms)),
  append(M, Ms, Msgs).

type(Gamma, [E | Es], recvMsg(Who, Msgs)) :-
  type(Gamma, E, recvMsg(Who, M)),
  type(Gamma, Es, recvMsg(Who, Ms)),
  append(M, Ms, Msgs).

type(Gamma, [_ | Es], T) :- type(Gamma, Es, T).

type(_, [], eps).

contains([K1:V1 | _], K:V) :- K = K1, V = V1.
contains([K1:_ | Gamma], K:V) :- K \== K1, contains(Gamma, K:V).

instantiate(poly(Gamma, T), U) :- copy_term(Gamma |- T, Gamma |- U).
instantiate(mono(T), T).

% Check that a session is able to receive all incoming messages
receivesAll(CList, AList) :-
  catch(keysort(CList, Concrete), _, true),
  catch(keysort(AList, Abstract), _, true),
  pairs_keys(Abstract, Keys), % Sent messages => all matched
  allCompatibleAux(Keys, Concrete, Abstract),
  CList = Concrete,
  AList = Abstract.

% Check that a session is only sending valid messages that can be received
sendsValid(CList, AList) :-
  catch(keysort(CList, Concrete), _, true),
  catch(keysort(AList, Abstract), _, true),
  pairs_keys(Concrete, Keys), % Sent messages => all matched
  allCompatibleAux(Keys, Concrete, Abstract),
  CList = Concrete,
  AList = Abstract.

allCompatibleAux([], _, _).
allCompatibleAux([Key | Keys], [Key-(CArg, CThen) | Concrete], [Key-(AArg, AThen) | Abstract]) :-
  % All keys in concrete are tested
  CArg = AArg,
  compatible(CThen, AThen),
  allCompatibleAux(Keys, Concrete, Abstract).
allCompatibleAux([Key | Keys], [Key-(CArg, CThen) | Concrete], [_ | Abstract]) :-
  allCompatibleAux([Key | Keys], [Key-(CArg, CThen) | Concrete], Abstract).
allCompatibleAux([Key | Keys], [_ | Concrete], [Key-(AArg, AThen) | Abstract]) :-
  allCompatibleAux([Key | Keys], Concrete, [Key-(AArg, AThen) | Abstract]).

compatible(recvMsg(_, B1), sendMsg(B2)) :- receivesAll(B1, B2).
compatible(sendMsg(_, B1), recvMsg(B2)) :- sendsValid(B1, B2).

compatible(T, T) :-
  T \= sendMsg(_),
  T \= recvMsg(_),
  T \= sendMsg(_, _),
  T \= recvMsg(_, _). % Any normal type is compatible with itself

test(T) :-
   G = [ get_time : mono(unit -> time)
       , unit : mono(unit)
       , diff_time : mono(time -> time -> time)
       , string_of_time : mono(time -> string)
       , print : mono(string -> unit)
       , server : mono(recvMsg(client, [
                    login-(strings, sendMsg(client, [success-(token, eps), failure-(unit, eps)])),
                    ping-(unit, sendMsg(client, [pong-(time, eps)]))]))
       ], 
   Expr =
     let(client, lam(s,
                     let(time, app(var(get_time), var(unit)),
                         sendMsg(var(s), ping, var(unit), [
                           recvMsg(var(s), pong, t, [
                             let(ping, app(app(var(diff_time), var(t)), var(time)),
                                 app(var(print), app(var(string_of_time), var(ping))))])]))),
         var(client)),
  type(G, Expr, T).
