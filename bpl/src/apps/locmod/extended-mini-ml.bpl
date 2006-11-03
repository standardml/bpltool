% Encoding of Extended Mini-ML using local bigraphs
% bug-fixing and extending Lars Birkedal's encoding of 2005-07-11.
% Ebbe Elsborg and Henning Niss, 2005-01-03.
% Revised by Ebbe Elsborg, 2005-04-21.
%
% In CBV, the activity (evaluation order) for an application node
% changes during evaluation.
%
% n ranges over positive integers.
% d belongs to a set Types of type names.
% x,f belong to a set Vars of variable names.
% l belongs to a set Loc of store locations.
% C belongs to a set Cons of contructor names.
% 'a belongs to some set A of base types.
% Types, Vars, Loc, Cons, and A are pairwise disjoint.
% Shorthands:
%  '{C_i of t_i}'   for   'C_1 of t_1 | ... | C_n of t_n'
%  '{C_i(x_i) => e_i}'   for   'C_1(x_1) => e_1 | ... | C_n(x_n) => e_n'
%
%  p ::= q;e | e
%  q ::= T | T q
%  T ::= datatype <'a> d = {C_i of t_i} % 'a optional, d fresh,
%					% t_i's can refer to 'a
%  t ::= Unit | t * t | t -> t | ref t | d
%
%  e ::= v | e e | x | let x = e in e | (e,e) | fst e | snd e
%      | C e | case e of {C_i(x_i) => e_i}
%      | ref e | !e | e := e
%  v ::= lam x. e | fix f(x) = e | unit | (v,v) | C v | l
%  E ::= _ e | v _ | (_,e) | (v,_) | fst _ | snd _ | let x = _ in e
%       | C _ | case _ of {C_i(x_i) => e_i}
%       | ref _ | !_ | _ := e | v := _
%
% Expressions are evaluated "in" a store s, only mentioned when relevant.
%    (lam x. e) v -> e[v/x]
%    (fix f(x) = e) v -> e[v/x, fix f(x)=e/f]
%    let x = v in e -> e[v/x]
%    fst (v1,v2) -> v1
%    snd (v1,v2) -> v2
%    case C_j v of {C_i(x_i) => e_i} -> e_j[v/x_j]   % where 1<=i<=n_k and
%						     % C_i belongs to d_k
%    <ref v,s> -> <l,(s,l->v)>	 		     % l fresh
%    <!l,s[l->v]> -> <v,s[l->v]>
%    <l:=v1,s[l->v2]> -> <unit,s[l->v1]>
%
%
% The evaluation contexts, E, are _certain_ kinds of applications.
% We model the different kinds of activities for the different 
% application nodes using different controls (activities are associated
% with controls, hence need different controls, it seems).
% 
% TRANSLATION:
%
% We use \o to denote the extension operator (\oplus i LaTeX), and
% \u to denote disjoint union (\uplus in LaTeX). fl(e) is a function taking
% a miniml-expression e and returning its free location names. fv(e) returns
% the free variables, and fn(e)=fl(e) \u fv(e).
%
% [e1 e2]_X              = (app \o id_X)
%			     ((appl \o id_X)[e1]_X | 
%                             (appr \o id_X)(exp \o id_X)[e2]_X)
% [lam x. e]_X           = (val \o id_X)(lam_(x) \o id_X)[e]_{X \u {x}}
% [x]_{X\uplus {x}}      = var_x \o X
% [let x = e1 in e2]_X   = (let \o id_X)
%			     ((letd \o id_X)[e1]_X |
%                             (letb_(x) \o id_X)[e2]_{X \u {x}})
% [fix f(x) = e]_X       = (val \o id_X)
%			     (fix_(f,x) \o id_X)[e]_{X \u {f,x}}
% [(e1,e2)]_X            = (pair \o id_X)
%	                     ((pairl \o id_X)[e1]_X |
%                             (pairr \o id_X)(exp \o id_X)[e2]_X)
% [fst e]_X              = (fst \o id_X)[e]_X
% [snd e]_X              = (snd \o id_X)[e]_X
% [unit]_X               = (val | X)unit
% [C e]_X	         = (C \o id_X)[e]_X
% [case e of {C_i(x_i) => e_i}]_X =
%   (case \o id_X)
%     ((casel \o id_X)([e]_X) |
%      (casee_(x1) \o id_X)(C1 \o id_X)(exp \o id_X)([e1]_{X \u {x1}}) |
%      ...
%      (casee_(xn) \o id_X)(Cn \o id_X)(exp \o id_X)([en]_{X \u {xn}}))
%
%   where 1<=i<=n and all C_i belong to some d_k with n_k constructors
%
% [ref e]_X		 = (ref \o id_X)[e]_X
% [!e]_X		 = (deref \o id_X)[e]_X
% [e1 := e2]_X		 = (assign \o id_X)
%			     ((aloc \o id_X)[e1]_X |
%			      (aval \o id_X)(exp \o id_X)[e2]_X)
% [l]_{X \u {l}}	 = (val \o (X | id_{l}))loc_l
% [{l_i->v_i}_{i=1..n}]_{X \u Y} = (store \o id_{X \u Y})
%				     ({(loc'_{l_i} \o id_{X \u Y})
%					[v_i]_{X \u Y}}_{i=1..n})
%						       , Y=\union_i l_i
% [<e,sigma>]_X		 = /Y.[e]_X | [sigma]_X	, Y=fl(e)\subseteq X
%
% Note: id_X in BPL notation is x_1/x_1,...,x_n/x_n for X={x_1,...,x_n}

signature eMiniml =
  sig
    lam   : passive (1 -> 0)    % recall, arity is: (binding -> free)
    app   : active (0 -> 0)
    appl  : active (0 -> 0)
    appr  : active (0 -> 0)
    var   : atomic (0 -> 1)
    sub   : active (1 -> 0)
    def   : active (0 -> 1)
	
    fix   : passive (2 -> 0)

    let   : active (0 -> 0)
    letd  : active (0 -> 0)     % let def
    letb  : passive (1 -> 0)    % let body

    pair  : active (0 -> 0)
    pairl : active (0 -> 0)
    pairr : active (0 -> 0)
    fst   : active (0 -> 0)
    snd   : active (0 -> 0)

    unit  : atomic (0 -> 0)

    case  : active (0 -> 0)
    casel : active (0 -> 0)
    casee : passive (1 -> 0)
    Ci    : active (0 -> 0)	% a control for each (uniquely) declared
				% constructor

    exp   : passive (0 -> 0)    % used to delay evaluation, see
	                        % translation of application, because
    val   : passive (0 -> 0)    % we will have several kinds of
	                        % values in general, not only lambdas

    loc   : atomic (0 -> 1)
    loc'  : passive (0 -> 1)	% because bindings are embeddings in store
    ref   : active (0 -> 0)
    deref : active (0 -> 0)
    assign: active (0 -> 0)
    aloc  : active (0 -> 0)
    aval  : active (0 -> 0)
    store : active (0 -> 0)

  end

using eMiniml

rule app_1 =
  app(appl(val([0]) | appr(exp([1])))
    ->                                
  app(appl(val([0]) | appr([1])))   	% now it is ok to evaluate in [1]

rule app_2 =
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
  var_x || def_x([0])                    % [0] is value by invariant
    ->
  [0] \o {x} || def_x([0])

rule gc =
  sub_(x)([0] | def_x([1]))
    ->                     
  [0]

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
  val(pair(pairl(val([0])) | pairr(val([1]))))  % mark whole pair as value

rule fst =
  fst(val(pair(pairl([0]) | pairr([1]))))	% the invariant says that
   ->						% [0] and [1] are values
  [0]

rule snd =
  snd(val(pair(pairl([0]) | pairr([1]))))	% the invariant says that
   ->						% [0] and [1] are values
  [1]

% we have a rule for each declared constructor C_i
rule val_Ci =
  Ci(val([0]))
    ->
  val(Ci(val([0])))

% we have a rule for each declared constructor C_i
rule case_Ci =
  case(casel(val(Ci(val([0])))) | (casee_(xi)(Ci(exp([1]<xi>))) | [2]))
    ->
  sub_(xi)([1]<xi> | def_xi([0]))

rule ref =
  ref(val([0])) || store([1])
    ->
  /l. val(loc_l) || store(loc'_l(val([0])) | [1])

rule deref =
  deref(val(loc_l)) || store(loc'_l([0]) | [1]) % [0] value by invariant
    ->
  [0] \o l/ || store(loc'_l([0]) | [1])

rule assign_1 =
  assign(aloc(val(loc_l)) | aval(exp([0])))
    ->
  assign(aloc(val(loc_l)) | aval([0]))

rule assign_2 =
  assign(aloc(val(loc_l)) | aval(val([0]))) || store(loc'_l([1]) | [2])
    ->
  val(unit) \o {l} || store(loc'_l(val([0])) | [2])

% make sure to make exchange active to allow the two expressions to be reduced
rule exchange =
  exchange(val(loc_l1)|val(loc_l2)) || store(loc'_l1([0])|loc'_l2([1]) | [2])
    ->
  val(unit) \o {l1,l2} || store(loc'_l1([1])|loc'_l2([0])|[2])

  