% Ebbe Elsborg 12/02/2007
%
% This is the "world" part C of a Plato-graphical system;
% C || P || A = C || (S || L) || A.

signature world =
  sig 	% arity is: (binding -> free)
    id: passive (0 -> 0)		% host identifier control
    loc : passive (0 -> 0)		% location
    dev : atomic (0 -> 0)		% mobile device
    devs : active (0 -> 0)		% inactive devices
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
state model =
  loc(id(i1) | dev(i15) |
      loc(id(i2) | dev(i10) | dev(i11)) |
      loc(id(i3)) |
      loc(id(i4) | loc(id(i5) | dev(i12)) |
	           loc(id(i6) | loc(id(i7) | loc(id(i8) | dev(i13)) |
				             loc(id(i9) | dev(i14))))))

state devs =
  loc(id(i0) | dev(i16) | dev(i17) | dev(i18))
