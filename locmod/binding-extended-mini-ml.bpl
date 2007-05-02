% Encoding of Extended Mini-ML using BINDING bigraphs.
%
% Much like for local bigraphs except for some 'abstractions' in the
% translation, and use of tensor product instead of the extension operator.
% The semantics is very similar to that of new-extended-miniml.bpl.
%
% Ebbe Elsborg, May 2 2007.
%
% Sets:
% n ranges over positive integers and zero.
% x,f belong to a set Vars of variable names.
% a is a subset of Vars.
% l belongs to a set Cells of reference cells.
% D ranges over a set Dats of datatype names.
% C ranges over a set Cons of contructor names.
% Vars, Cells, Dats, and Cons are pairwise disjoint.
%
% Shorthands and notation:
%  '{C_i e_i}^n'  for  'C_0 e_0 | ... | C_n e_n'
%  '{C_i x_i) => e_i}^n'  for  'C_0 x_0 => e_1 | ... | C_n x_n => e_n'
%
% BNF:
%  p ::= export a from t | t
%  t ::= datatype D = {C_i of e_i}^n ; t | d
%  T ::= Nat | Unit | T1 -> T2 | T1 * T2 | Ref T | C_i : T_i
%  d ::= val x = e | val x = e ; d
%  e ::= x | e1 e2 | (e1,e2) | fst e | snd e | let val x = e1 in e2 end
%      | ref e | !e | e1 := e2 | exchange(e1,e2)
%      | C e | case e of {C_i x_i => e_i}^n | v
%  v ::= lam x. e | fix f(x) = e | (v1,v2) | unit | l | C v | n
%  E ::= [ ] | E e | v E | (E,e) | (v,E) | fst E | snd E
%       | let val x = E in e end | let val x = v in E end
%       | ref E | !E | E := e | l := E | exchange(E,e) | exchange(l,E)
%       | C E | case E of {C_i x_i => e_i}^n
%
%
% DYNAMICS:
%
% Expressions are evaluated "in" a store s, only mentioned when relevant.
% We use _explicit_ substitution, [-].
%    fst (v1,v2) -> v1
%    snd (v1,v2) -> v2
%    let val x = v in e end -> e[v/x]
%    (lam x. e) v -> e[v/x]
%    (fix f(x) = e) v -> e[v/x, fix f(x)=e/f]
%    case C_j v of {C_i x_i => e_i}^n -> e_j[v/x_j] % j in {0,1,...n}
%    <ref v,s> -> <l,(s,l->v)>	,  l fresh
%    <!l,s[l->v]> -> <v,s[l->v]>
%    <l:=v1,s[l->v2]> -> <unit,s[l->v1]>
%    <exchange(l,l'),s[l->v,l'->v']> -> <unit,s[l->v',l'->v]>
%
% 
% TRANSLATION:
%
% The store is provided by translation on "system level"!
% I.e. M = /X . ( C || S || [L]^prog_X || [A]^prog_X ) | store() , where 
% X = {enqL,enqA..}, and store:passive (0->0) belongs to Sig_M.
%
% \t denotes tensor product (\otimes in LaTeX), and
% \u denotes disjoint union (\uplus in LaTeX).
%
% The superscript on [-] identifies the semantic function.
% The subscript X on [-] contains the set of free variable names of '-'.
% The (S) in front of the semantic function [-]^dtypes denotes the
% signature used, and in this case, updated by the translation.
% Composition is implicit.
% The signature is unchanged by all other semantic functions, thus omitted.
% 'id_X' in BPL notation is 'x_1/x_1,...,x_n/x_n' for X={x_1,...,x_n}.
%
% [t]^prog_X               = [t]^dtypes_X
% [export a from t]^prog_X = ([a]^names_a \t id_X \t id_1)[t]^dtypes_{X \u a}
%
% [a]^names_a = id_a
%	Notice that these names are added to the outer face.
%
% (S)[datatype D = C_i of T_i ; t]^dtypes_X = (S,C_i)[t]^dtypes_X , i=1..n
%	where (S,C_i) denotes update of S by the active control C_i
%	with arity (0 -> 0).
% (S)[d]^dtypes_X                           = (S)[d]^decls_X
%	Notice how types are thrown away.
%
% [val x = e]^decls_X     = (def'_x \t id_X)[e]^exp_X
% [val x = e ; d]^decls_X = (def'_x \t id_X)[e]^exp_X | [d]^decls_{X \u {x}}
% 	Notice how previous declarations can be referenced.
%
% [x]^exp_{X\uplus {x}}    = X/ \t var_x
% [(e1,e2)]^exp_X          = (pair \t id_X)
%	                      ((pairl \t id_X)[e1]^exp_X |
%                              (pairr \t id_X)(exp \t id_X)[e2]^exp_X)
% [fst e]^exp_X            = (fst \t id_X)[e]^exp_X
% [snd e]^exp_X            = (snd \t id_X)[e]^exp_X
% [let val x = e1 in e2 end]^exp_X
%			   = (let \t id_X)
%			      ((letd \t id_X)[e1]^exp_X |
%                              (letb_(x) \t id_X)(({x})[e2]^exp_{X \u {x}}))
% [lam x. e]^exp_X         = (val \t id_X)
%			      (lam_(x) \t id_X)(({x})[e]^exp_{X \u {x}})
% [fix f(x) = e]^exp_X     = (val \t id_X)
%			      (fix_(f,x) \t id_X)(({f,x})[e]^exp_{X \u {f,x}})
% [e1 e2]^exp_X            = (app \t id_X)
%			      ((appl \t id_X)[e1]^exp_X | 
%                              (appr \t id_X)(exp \t id_X)[e2]^exp_X)
% [unit]^exp_X             = (val \t X/) unit
% [n]^exp_X                = (val \t X/) n
% [C e]^exp_X	           = (C \t id_X)[e]^exp_X
% [case e of {C_i x_i => e_i}^n]^exp_X =
%   (case \t id_X)
%    ((casel \t id_X)([e]^exp_X) |
%     (casee_(x0) \t id_X)(({x0})
%      (C0 \t id_{X \u {x0}})(exp \t id_{X \u {x0}})([e0]^exp_{X \u {x0}})) |
%     ...
%     (casee_(xn) \t id_X)(({xn})
%      (Cn \t id_{X \u {xn}})(exp \t id_{X \u {xn}})([en]^exp_{X \u {xn}})))
% [ref e]^exp_X		   = (ref \t id_X)[e]^exp_X
% [!e]^exp_X		   = (deref \t id_X)[e]^exp_X
% [e1 := e2]^exp_X	   = (asgn \t id_X)
%			      ((acell \t id_X)[e1]^exp_X |
%			       (aval \t id_X)(exp \t id_X)[e2]^exp_X)
% [exchange(e1,e2)]^exp_X  = (exc \t id_X)
%			      ((excl \t id_X)[e]^exp_X |
%			       (excr \t id_X)(exp \t id_X)[e2]^exp_X)

signature beMiniml =
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
    exc : active (0 -> 0)	% atomically swap contents of two ref. cells
    excl : active (0 -> 0)
    excr : active (0 -> 0)
    store : passive (0 -> 0)
    exp   : passive (0 -> 0)    % delay evaluation
    def   : active (0 -> 1)
    defs  : active (0 -> 0)
    def'  : active (0 -> 1)	% non-discardable definition

  end

using beMiniml

rule app_exp =
  app(appl(val([0]) | appr(exp([1])))
    ->                                
  app(appl(val([0]) | appr([1])))

rule app_lam =
  app(appl(val(lam_(x)[0]<x>)) | appr(val([1]))) || defs([2])
    ->                                
  /x. [0]<x> || defs([2] | def_x(val([1])))

rule app_fix =
  app(appl(val(fix_(f,x)[0]<x,f>)) | appr(val([1]))) || defs([2])
    ->
  /x,f. [0]<x,f> || defs([2] | def_x(val([1])) | def_f(val(fix_(f,x)[0]<x,f>)))

rule let =
  let(letd(val([0])) | letb_(x)([1]<x>))
    ->
  /x. [1]<x> || defs(sub_x(val([0])))

rule pair_exp =
  pair(pairl(val([0])) | pairr(exp([1]))) 
   ->
  pair(pairl(val([0])) | pairr([1]))

rule pair_val =
  pair(pairl(val([0])) | pairr(val([1])))
   ->
  val(pair(pairl(val([0])) | pairr(val([1]))))

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
  val(C(val([0])))

% we have a rule for each declared constructor C
rule case_C =
  case(casel(val(C([0]))) | casee_(x)(Ci(exp([1]<x>))) | [2]) || defs([3])
    ->
  /x. [1]<x> || defs([3] | def_x([0]))  % [0] value by invariant

rule ref =
  ref(val([0])) || store([1])
    ->
  /l. val(cell_l) || store(cell'_l(val([0])) | [1])

rule deref =
  deref(val(cell_l)) || store(cell'_l([0]) | [1])
    ->
  [0] | l/ || store(cell'_l([0]) | [1])  % [0] value by invariant

rule asgn_exp =
  asgn(acell(val(cell_l)) | aval(exp([0])))
    ->
  asgn(acell(val(cell_l)) | aval([0]))

rule asgn_store =
  asgn(acell(val(cell_l)) | aval(val([0]))) || store(cell'_l([1]) | [2])
    ->
  val(unit) \t l/ || store(cell'_l(val([0])) | [2])

rule exc_exp =
  exc(excl(val([0])) | excr(exp([1])))
    ->
  exc(excl(val([0])) | excr([1]))

% works because l1,l2 are closed outer names, not bound by a port in the context
rule exc_store =
  exc(excl(val(cell_l1)) | excr(val(cell_l2)))
	|| store(cell'_l1([0]) | cell'_l2([1]) | [2])
    ->
  val(unit) \t l1,l2/
	|| store(cell'_l1([1]) | cell'_l2([0])|[2])

% works because x is a closed outer name, not bound by a port in the context
rule def =
 var_x || def_x(val([0]))
   ->
 val([0]) \t x/ || def_x(val([0]))

rule gc =
 [0] | defs(/x o def_x([1]))
   ->                     
 [0]  % can garbage collect when link is idle

% works because x is a closed outer name, not bound by a port in the context
rule sub' =
  var_x || def'_x(val([0]))
    ->
  val([0]) \t x/ || def'_x(val([0]))