% Ebbe Elsborg and Henning Niss, 18/12/2005
%
% This is the "sensor" part S of a Plato-graphical system;
% C || P || A = C || (S || L) || A.
%
% Idea: Let S observe the state of C and L, and inform L.
%
% $e$ is the translation of expression e in l.sml into local bigraphs.

signature sensor =
  sig
    % arity is: (binding -> free)
    var : atomic (0 -> 1)
    loc : passive (0 -> 1)      % corresponds to loc in c.bpl
    map : passive (0 -> 1)
    lost: passive (0 -> 0)	% assumed unique, like in C
end

using sensor

val mapping =
  map_l1(var_l1) | map_l2(var_l2) | map_l3(var_l3) |
  map_l4(var_l4) | map_l5(var_l5) | map_l6(var_l6) |
  map_l8(var_l8) | map_l8(var_l8)

% Semantics: Inform L that C has dev d in loc l.
rule inform_move =
  /d.loc_l(dev_d | [0]) ||			% C
  map_l(var_l) ||				% S
  1						% L
    ->
  /d.loc_l(dev_d | [0]) ||			% C
  map_l(var_l) ||				% S
  $sMove d l$					% L'

rule inform_lost =
  /d.loc_l(dev_d | [0]) || lost(dev_d | [1]) ||	% C
  1						% L
    ->
  /d.loc_l(dev_d | [0]) || lost([1]) ||		% C'
  $sLose d$					% L'