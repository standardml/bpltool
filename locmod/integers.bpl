% Encoding of integers in local bigraphs.
% Ebbe Elsborg and Henning Niss, 2005-11-18.

signature Int =
  sig
    true  : atomic (0 -> 0)    % arity: binding -> free
    false : atomic (0 -> 0)

    zero  : atomic (0 -> 0)
    succ  : active  (0 -> 0)    % add needs to evaluate under succ

    argl  : active  (0 -> 0)
    argr  : active  (0 -> 0)

    eqi   : passive (0 -> 0)
    lti   : passive (0 -> 0)
    addi  : passive (0 -> 0)
    subi  : passive (0 -> 0)
    muli  : passive (0 -> 0)
  end

using Int

% eqi: equality on integers
rule eq_zero = 
  eqi(argl(zero) | argr(zero)) -> true

rule eq_succ =
  eqi(argl(succ([0])) | argr(succ([1])))
    -> 
  eqi(argl([0]) | argr([1]))

rule eq_false_1 =
  eqi(argl(zero) | argr(succ([0]))) -> false

rule eq_false_2 =
  eqi(argl(succ([0])) | argr(zero)) -> false

% lti: less than on integers
rule lt_zero = 
  lti(argl(zero) | argr(zero)) -> false

rule lt_succ =
  lti(argl(succ([0])) | argr(succ([1])))
    ->
  lti(argl([0]) | argr([1]))

rule lt_true =
  lti(argl(zero) | argr(succ([0]))) -> true

rule lt_false =
  lti(argl(succ([0])) | argr(zero)) -> false

% addi: addition on integers
rule add_zero_1 =
  addi(argl(zero) | argr([0])) -> [0]

rule add_zero_2 =
  addi(argl([0]) | argr(zero)) -> [0]

rule add_succ =
  addi(argl(succ([0])) | argr(succ([1])))
    ->
  succ(addi(argl(succ([0])) | argr([1])))

% subi: (non-negative) subtraction on integers
rule sub_zero_1 =
  subi(argl(zero) | argr([0])) -> zero

rule sub_zero_2 =
  subi(argl([0]) | argr(zero)) -> [0]

rule sub_succ =
  subi(argl(succ([0])) | argr(succ([1]))) 
    -> 
  subi(argl([0]) | argr([1]))

% addi: multiplication on integers
rule mul_zero_1 =
  subi(argl(zero) | argr([0])) -> zero

rule mul_zero_2 =
  subi(argl([0]) | argr(zero)) -> zero

rule mul_succ =         % (m+1)*(n+1) = (n+1)+m*(n+1)
  muli(argl(succ([0])) | argr(succ([1])))
    ->
  addi(argl(succ([1])) | argr(muli(argl([0]) | argr(succ([1])))))