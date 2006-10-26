% Ebbe Elsborg and Henning Niss, 18/12/2005
%
% This is the "world" part C of a Plato-graphical system;
% C || P || A = C || (S || L) || A.

signature world =
  sig
    loc : passive (0 -> 1)	% arity is: (binding -> free)
    dev : atomic (0 -> 1)
    lost: passive (0 -> 0)	% assumed unique
end

using world

% Invariant: A device is in at most one location at any given time.
% Secured by the theory of local bigraphs, TR603 Def 3.2, because of
% the condition I \extension H.

rule discover = 
  loc_a([0])
    ->
  /x.loc_a(dev_x | [0])

rule move =
  /x.(loc_a(dev_x | [0]) || loc_b([1]))
    -> 
  /x.(loc_a([0]) || loc_b(dev_x | [1]))

rule lose =
  /x.loc_a(dev_x | [0]) || lost([1])
    ->
  loc_a([0]) || lost(dev_x | [1])

% specific model coherent with l.sml
val model =
  loc_l1(loc_l2(dev_d1 | dev_d2 | loc_l5() | loc_l6()) |
         loc_l3() |
	 loc_l4(dev_d3 | loc_l7() | loc_l8(dev_d4 | dev_d5 | dev_d6)))