% Ebbe Elsborg, 25/01/2007
%
% This is the "sensor" part S of a Plato-graphical system;
% C || P || A = C || (S || L) || A.

signature sensor =
  sig	% arity is: (binding -> free)
    id: passive (0 -> 0)	% host identifier control
    loc : passive (0 -> 0)	% location
    dev : atomic (0 -> 1)	% mobile device
    devs :  (0 -> 1)		% known devices
    app : active (0 -> 0)	% application
    appl : active (0 -> 0)
    appr : active (0 -> 0)
    fst: active (0 -> 0)	% first projection on a pair
    snd: active (0 -> 0)	% second projection on a pair
    var : atomic (0 -> 1)	% variable
    exp : passive (0 -> 0)	% delay evaluation
    i0,i1,i2,... : atomic (0 -> 0)	% inf. family of identifiers
    invoke : active (0 -> 1)	% hosts "function calls"
end

using sensor

rule observe =
  loc(id([0]) | dev([1]) | [2]) || invoke_g([3])
	->
  loc(id([0]) | dev([1]) | [2])
  || invoke_g([3] | app(appl(app(appl(fst(var_g))
			       | appr(exp([1]))))
		      | appr(exp([0]))))

rule lose =
  devs([0] | dev([1])) || invoke_g([2])
	->
  devs([0] | dev([1]))
  || invoke_g([2] | app(appl(fst(snd(var_g))) | appr(exp([1]))))