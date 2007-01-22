% Ebbe Elsborg 22/01/2007
%
% This is the "world" part C of a Plato-graphical system;
% C || P || A = C || (S || L) || A.

signature world =
  sig 	% arity is: (binding -> free)
    id: passive (0 -> 0)		% host identifier control
    loc : passive (0 -> 0)		% location
    dev : atomic (0 -> 1)		% mobile device
    devs :  (0 -> 1)			% known devices
    i0,i1,i2,... : atomic (0 -> 0)	% inf. family of identifiers
end

using world

rule discover =
  loc([0]) || devs([1] | dev([2]))
	->
  loc([0] | dev([2])) || devs([1])

rule lose =
  loc([0] | dev([2])) || devs([1])
	->
  loc([0]) || devs([1] | dev([2]))

rule moveup =
  loc([0] | loc([1] | dev([2])))
	->
  loc([0] | loc([1]) | dev([2]))

rule movedown =
  loc([0] | loc([1]) | dev([2]))
	->
  loc([0] | loc([1] | dev([2])))

% specific model coherent with l.sml
val model =
  loc(i1 | dev(i15) |
      loc(i2 | dev(i10) | dev(i11)) |
      loc(i3) |
      loc(i4 | loc(i5 | dev(i12)) |
	       loc(i6 | loc(i7 | loc(i8 | dev(i13)) |
				 loc(i9 | dev(i14))))))