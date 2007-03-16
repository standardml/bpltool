% Encoding of Extended Mini-ML using LOCAL bigraphs.
%
% Bug-fixing and extending Lars Birkedal's encoding of 2005-07-11.
%
% Ebbe Elsborg and Henning Niss, 2005-01-03.
% Revised by Ebbe Elsborg, 2007-03-15.
%
% In CBV, the activity (evaluation order) for an application node
% changes during evaluation.
%
% Sets:
% n ranges over positive integers and zero.
% x,f belong to a set Vars of variable names.
% l belongs to a set Cells of reference cells.
% D ranges over a set Dats of datatype names.
% C ranges over a set Cons of contructor names.
% Vars, Cells, Dats, and Cons are pairwise disjoint.
%
% Shorthands:
%  '{C_i e_i}^n'  for  'C_0 e_0 | ... | C_n e_n'
%  '{C_i x_i => e_i}^n'  for  'C_0 x_0 => e_1 | ... | C_n x_n => e_n'
%
% BNF:
%  p ::= datatype D = {C_i e_i}^n
%  e ::= x | e1 e2 | (e1,e2) | fst e | snd e | let x = e1 in e2 end
%      | ref e | !e | e1 := e2 | swap(e1,e2)
%      | C e | case e of {C_i x_i => e_i}^n | v
%  v ::= lam x. e | fix f(x) = e | (v,v) | unit | l | C v | n
%  E ::= [ ] | E e | v E | (E,e) | (v,E) | fst E | snd E
%       | let x = E in e end | let x = v in E end
%       | ref E | !E | E := e | v := E | swap(E,e) | swap(l,E)
%       | C E | case E of {C_i x_i => e_i}^n
%
% The evaluation contexts, E, are _certain_ kinds of applications.
% We model the different kinds of activities for the different 
% application nodes using different controls (activities are associated
% with controls, hence need different controls, it seems).
%
%
% DYNAMICS:
%
% Expressions are evaluated "in" a store s, only mentioned when relevant.
% We use _explicit_ substitution, [-].
%
%    fst (v1,v2) -> v1
%    snd (v1,v2) -> v2
%    let x = v in e end -> e[v/x]
%    (lam x. e) v -> e[v/x]
%    (fix f(x) = e) v -> e[v/x, fix f(x)=e/f]
%    case C_j v of {C_i x_i => e_i}^n -> e_j[v/x_j] % j in {0,1,...n}
%    <ref v,s> -> <l,(s,l->v)>	% l fresh
%    <!l,s[l->v]> -> <v,s[l->v]>
%    <l:=v1,s[l->v2]> -> <unit,s[l->v1]>
%    <swap(l,l'),s[l->v,l'->v']> -> <unit,s[l->v',l'->v]>
%
% 
% TRANSLATION:
%
% \o denotes the extension operator (\oplus i LaTeX) and
% \u denotes disjoint union (\uplus in LaTeX).
%
% The subscript X on [e]_X contains the set of free variable names of e.
% 'id_X' in BPL notation is 'x_1/x_1,...,x_n/x_n' for X={x_1,...,x_n}.
%
% [x]_{X\uplus {x}}      = X \o var_x
% [(e1,e2)]_X            = (pair \o id_X)
%	                     ((pairl \o id_X)[e1]_X |
%                             (pairr \o id_X)(exp \o id_X)[e2]_X
% [fst e]_X              = (fst \o id_X)[e]_X
% [snd e]_X              = (snd \o id_X)[e]_X
% [let x = e1 in e2]_X   = (let \o id_X)
%			     ((letd \o id_X)[e1]_X |
%                             (letb_(x) \o id_X)[e2]_{X \u {x}})
% [lam x. e]_X           = (val \o id_X)(lam_(x) \o id_X)[e]_{X \u {x}}
% [fix f(x) = e]_X       = (val \o id_X)
%			     (fix_(f,x) \o id_X)[e]_{X \u {f,x}}
% [e1 e2]_X              = (app \o id_X)
%			     ((appl \o id_X)[e1]_X | 
%                             (appr \o id_X)(exp \o id_X)[e2]_X)
% [unit]_X               = (val | X) unit
% [n]_X                  = (val | X) n
% [C e]_X	         = (C \o id_X)[e]_X
% [case e of {C_i x_i => e_i}^n]_X =
%   (case \o id_X)
%     ((casel \o id_X)([e]_X) |
%      (casee_(x0) \o id_X)(C0 \o id_X)(exp \o id_X)([e0]_{X \u {x0}}) |
%      ...
%      (casee_(xn) \o id_X)(Cn \o id_X)(exp \o id_X)([en]_{X \u {xn}}))
% [ref e]_X		 = (ref \o id_X)[e]_X
% [!e]_X		 = (deref \o id_X)[e]_X
% [e1 := e2]_X		 = (asgn \o id_X)
%			     ((acell \o id_X)[e1]_X |
%			      (aval \o id_X)(exp \o id_X)[e2]_X)
% [swap(e1,e2)]_X	 = (swap \o id_X)
%			     ((swapl \o id_X)[e]_X |
%			      (swapr \o id_X)(exp \o id_X)[e2]_X)

signature eMiniml =
  sig	% arity is (binding -> free)
    var   : atomic (0 -> 1)
    app   : active (0 -> 0)
    appl  : active (0 -> 0)
    appr  : active (0 -> 0)
    pair  : active (0 -> 0)
    pairl : active (0 -> 0)
    pairr : active (0 -> 0)
    fst   : active (0 -> 0)
    snd   : active (0 -> 0)
    let   : active (0 -> 0)
    letd  : active (0 -> 0)
    letb  : passive (1 -> 0)
    ref   : active (0 -> 0)
    deref : active (0 -> 0)
    asgn  : active (0 -> 0)
    acell : active (0 -> 0)
    aval  : active (0 -> 0)
    case  : active (0 -> 0)
    casel : active (0 -> 0)
    casee : passive (1 -> 0)
    Ci    : active (0 -> 0)	% a control for each member of Cons
    val   : passive (0 -> 0)
    lam   : passive (1 -> 0)
    fix   : passive (2 -> 0)
    unit  : atomic (0 -> 0)
    0,1,2... : atomic (0 -> 0)	% a control for each natural number (+zero)
    cell  : atomic (0 -> 1)	% reference cell (rc)
    cell' : passive (0 -> 1)	% reference cell in store, linked to rc
    swap  : active (0 -> 0)	% atomically swap contents of two ref. cells
    swapl : active (0 -> 0)
    swapr : active (0 -> 0)
    store : passive (0 -> 0)
    exp   : passive (0 -> 0)    % delay evaluation
    sub   : active (1 -> 0)
    def   : active (0 -> 1)
    def'  : active (0 -> 1)	% non-discardable definition

  end

using eMiniml

rule app_arg =
  app(appl(val([0]) | appr(exp([1])))
    ->                                
  app(appl(val([0]) | appr([1])))	% now it is ok to evaluate in [1]

rule app_lam =
  app(appl(val(lam_(x)[0]<x>)) | appr(val([1])))
    ->                                
  sub_(x)([0]<x> | def_x(val([1])))

rule app_fix =
  app(appl(val(fix_(f,x)[0]<x,f>)) | appr(val([1])))
    ->
  sub_(f)(sub_(x)([0]<x,f> | def_x(val([1]))) |
          def_f(val(fix_(f,x)[0]<x,f>))) % parallel subst. as two
	                                 % nested substitutions

rule sub =
  var_x || def_x(val([0]))
    ->
  val([0]) | {x} || def_x(val([0]))

rule gc =
  sub_(x)([0] | def_x([1]))
    ->                     
  [0]

rule sub' =
  var_x || def'_x([0])
    ->
  [0] \o {x} || def'_x([0])

% there is no gc' rule, on purpose

rule let =
  let(letd(val([0])) | letb_(x)([1]<x>))
    ->
  sub_(x)([1]<x> | def_x(val([0])))

rule pair_1 =
  pair(pairl(val([0])) | pairr(exp([1]))) 
   ->
  pair(pairl(val([0])) | pairr([1]))	% now ok to evaluate in [1]

rule pair_2 =
  pair(pairl(val([0])) | pairr(val([1])))
   ->
  val(pair(pairl(val([0])) | pairr(val([1]))))	% mark whole pair as value

rule fst =
  fst(val(pair(pairl([0]) | pairr([1]))))
   ->
  [0]	% [0] value by invariant

rule snd =
  snd(val(pair(pairl([0]) | pairr([1]))))
   ->
  [1]	% [1] value by invariant

% we have a rule for each declared constructor C
rule val_C =
  C(val([0]))
    ->
  val(Ci(val([0])))

% we have a rule for each declared constructor C
rule case_C =
  case(casel(val(C([0]))) | casee_(x)(Ci(exp([1]<x>))) | [2])
    ->
  sub_(x)([1]<x> | def_x([0]))	% [0] value by invariant

rule ref =
  ref(val([0])) || store([1])
    ->
  /l. val(cell_l) || store(cell'_l(val([0])) | [1])

rule deref =
  deref(val(cell_l)) || store(cell'_l([0]) | [1])
    ->
  [0] | l/ || store(cell'_l([0]) | [1])	% [0] value by invariant

rule asgn_1 =
  asgn(acell(val(cell_l)) | aval(exp([0])))
    ->
  asgn(acell(val(cell_l)) | aval([0]))

rule asgn_2 =
  asgn(acell(val(cell_l)) | aval(val([0]))) || store(cell'_l([1]) | [2])
    ->
  val(unit) \o {l} || store(cell'_l(val([0])) | [2])

rule swap_1 =
  swap(swapl(val([0])) |swapr(exp([1])))
   ->
  swap(swapl(val([0])) |swapr([1]))

rule swap_2 =
  swap(swapl(val(cell_l1)) | swapr(val(cell_l2)))
	|| store(cell'_l1([0]) | cell'_l2([1]) | [2])
    ->
  val(unit) \o {l1,l2}
	|| store(cell'_l1([1]) | cell'_l2([0])|[2])
