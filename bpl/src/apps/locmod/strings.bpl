% Encoding of strings in (local) bigraphs.
% Ebbe Elsborg and Henning Niss, 2005-11-25.

% Strings represented as lists of characters;
% Characters represented by nodes.

signature String =
  sig
    % arguments
    argl  : active    (0 -> 0)
    argr  : active    (0 -> 0)

    % booleans
    true :    atomic  (0 -> 0)
    false :   atomic  (0 -> 0)
    andalso : active  (0 -> 0)

    % chars - uncapitalized letters and numerical digits left implicit
    A : atomic        (0 -> 0)
      ...
    Z : atomic        (0 -> 0)
    eqc :  passive    (0 -> 0)

    null : atomic     (0 -> 0)
    cell : active     (0 -> 0)
    char : passive    (0 -> 0)
    rest : active     (0 -> 0)

    % operations
    len  : passive    (0 -> 0)
    append : passive  (0 -> 0)
    equal  : passive  (0 -> 0)
  end

using String

% example string (HELLO)
val h = cell(char(H)|rest(cell(char(E)|rest(cell(char(L)|rest(cell(char(L)|rest(cell(char(O)|null)))))))))

% allow myself to use integers as built-in type

% length: compute length of string
rule length_1 =
  len(null) -> 0

rule length_2 =
  len(cell(char([0]) | rest([1]))) -> 1 + len([1])

% append: concatenate two strings
rule append_1 =
  append(argl(null) | argr([0])) -> [0]

rule append_2 =
  append(argl([0]) | argr(null)) -> [0]

rule append_3 =
  append(argl(cell(char([0]) | rest([1]))) | argr([2]))
    ->
  cell(char([0]) | rest(append(argl([1]) | argr([2]))))


% equal: compare two strings for equality
rule equal_1 =
  equal(argl(null) | argr(null)) -> true

rule equal_2 =
  equal(argl(null) | argr(cell([0]))) -> false

rule equal_3 =
  equal(argl(cell([0])) | argr(null)) -> false

rule equal_4 = 
  equal(argl(cell(char([0])|rest([1]))) | 
        argr(cell(char([2])|rest([3]))))
    ->
  andalso(argl(eqc(argl([0]) | argr([2]))) |
          argr(equal(argl([1]) | argr([2]))))

rule eqc_A_true =
  eqc(argl(A) | argr(A)) -> true

rule eqc_A_false_B =
  eqc(argl(A) | argr(B)) -> false

...

rule eqc_A_false_Z =
  eqc(argl(A) | argr(Z)) -> false

.
.
.
rule eqc_Z_true =
  eqc(argl(Z) | argr(Z)) -> true

rule eqc_A_false_B =
  eqc(argl(Z) | argr(A)) -> false

...

rule eqc_A_false_Z =
  eqc(argl(Z) | argr(Y)) -> false


rule andalso_1 =
  andalso(argl(true) | argr(true)) -> true

rule andalso_2 =
  andalso(argl(true) | argr(false)) -> false

rule andalso_3 =
  andalso(argl(false) | argr([0])) -> false