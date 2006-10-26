% Representation of a location model with queries in PURE bigraphs.
% Based on work with Henning Niss.
% Ebbe Elsborg, 15-08-2005
%
% Preamble:
% We represent the location model and the queries directly in pure
% bigraphs. This is done to explore how easy/difficult it is to
% model real-world systems in bigraphs. Can probably be done by
% using the encoding of ML data structures found in 'mini-ml.bpl'.
% It is not clear, however, that we want to do this.
%
% Notes on the representation:
% - We model buildings, floors, rooms etc. by introducing "containers".
%   This generality makes the representation more elegant, and more
%   information about a container can be given by simply adding a link,
%   if desired.
% - Containers can be empty, contain other container(s), or contain
%   device(s) at any given point in time. Specifically, a container
%   cannot contain both a container and a device at the same time. This
%   is a reasonable assumption (if not convinced, think about it). By
%   'leaves' we denote containers that do not contain other containers.
% - Regarding topology (place graph): The one tree representing the
%   location model is placed as only child of a "root node".
%   This is done to be able to recognise that a search is completed.
%   The whole system is a forest.
% - Assume that there is some process that creates queries by
%   introducing nodes with certain controls into the existing model of
%   the system. I.e. an interface between the user and the model.
%   (Lars has a similar idea about such a sensor process).
%   Algorithm: (1) Exactly one query node is inserted into the "global"
%   (global means sibling of the unique root container) 'in' control to
%   avoid interference with the queries. (2) Calculate. (3) Output is
%   likewise put in the "global" 'out' control for immediate reading by
%   "the IO process" -- and 'in' cleared. We use online garbage
%   collection.
% - Queries are of two kinds: "Reconfiguring" and "asking".
%   Reconfiguring queries alter the place graph, asking queries do not.
%   The rewrite rules are constructed s.t. they get stuck when the
%   termination condition is met (see below). Also, we apply dichotomy
%   so that all cases are covered.
% - Regarding the "nearest neighbour query": The metric is: (1) Look
%   in same container first, if unsuccessful do depth-first search.
%   We do not use the distance links of the range queries, since we
%   are not yet sure whether it is the right way to do range queries.
%   (Nearest neighbour was encoded before the range query.) The
%   controls 'a' and 'b' are used to make sure that one set of rewrite
%   rules is executed before another -- this is needed to separate
%   search from clean-up. (Clever, I think.)
% - [Ignore this paragraph, should be done differently]
%   Links can exist as usual, but furthermore we assume "distance"
%   links between all containers that do *not* contain other
%   containers (these containers also have "0" links to themselves).
%   These links represent the distance between the centers of two
%   containers -- this is realistic wrt. Bluetooth technology. Each
%   distance link is connected, through the outer face, to an "integer
%   name", e.g., "7".
% - Types: We want to be able to enquire about devices of certain
%   types (without having to write rules for each pair). Thus, we need
%   subtyping. We do this be assuming a global control "types" (sibling
%   to root) which, for all pairs (t,t') of device types, holds either
%   a control 'leq(left,right)' where t is linked to left and t' to
%   right, or 'gt(left,right)' where t is linked to left and t' to
%   right. Intuition: The larger the type, the more general. If we want
%   to find all devices we just ask for devices with type 'top', which
%   is greater than all other types in control 'types'. Subtyping could
%   also have been done by having a "types tree" to traverse (using a
%   similar technique as in the nearest neighbour query).
% - The termination condition (when the IO process can report back to
%   the user) for the queries is that 'in' is empty. Also, we assume
%   some meta-conditions/invariants (see encoding).
%
% Now, for the ML-like encoding. (It may help to draw the bigraphs...)

signature Locmod =
  sig                	% Recall: arity is number of ports, we take the
		     	% l.u.b. of ports required in the different
		     	% queries, to use fewest different controls.
    a : atomic (0)	% exists while still searching
    b : atomic (0)	% exists when found a d, enable clean-up rules
    c : active (2)	% container. ports: id,distance
    d : atomic (2)	% device. ports: id,type
    d' : atomic (2)	% device, answer. ports: id,id
    e : atomic (0)	% exists when 'near' query may terminate
    f : atomic (1)	% find all. ports: type
    f' : atomic (2)	% find all, one answer. ports: id,type
    g : atomic (0)	% dummy control for termination
    in : passive (0)	% input, one query at a time
    left : atomic (1)	% left part of a binary relation. ports: type
    m : passive (2)	% move. ports: id,id
    n : atomic (2)	% nearest neighbour. ports: id,type
    n' : atomic (3)	% nearest neighbour, answer. ports: id,id,type
    out : passive (0)	% output, answer to latest query
    r : atomic (2)	% range query. ports: id,id
    r' : atomic (3)	% range query, answer. ports: id,id,distance
    right : atomic (1)	% right part of a binary relation. ports: type
    s : passive (0)	% searched
    tmp : passive (0)	% for aux. nodes during 'near' search
    types : passive (0) % holds/implements the subtyping relation
    void : atomic (1)	% void, in case we do not find any. ports: id
    w : atomic (1)	% where. ports: id
    w' : atomic (2)	% where, answer. ports: id,id
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Following the syntax of mini-ml.bpl we write A o B as A(B).         %
%								      %
% I've checked interfaces, should be okay. Should be checked by tool. %
%								      %
% Omitted mentioning of open links, their names are irrelevant and    %
% clutter the syntax, should be inferred by the tool.		      %
% Example: For ions K, K', and K''				      %
%             K_(xy) || K'_(z)(K''_(x) | [0]) || K'(y)([1])	      %
%	   -> 1 || K'_(z)([0]) || K'_(y)(K''_(x) | [1])		      %
%          is written as					      %
%	      K_(xy) || K'(K''_(x) | [0]) || K'(y)([1])		      %
%	   -> 1 || K'([0]) || K'_(y)(K''_(x) | [1])		      %
%          i.e. without the open link z as subscript.		      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

using Locmod

rule move =	% move a device, the sole reconfiguration query
  in(m_x,y) || c(d_x | [0]) || c_y([1]) || out([2])
    ->
  in() || c([0]) || c_y(d_x | [1]) || out()

rule where =	% where is a certain device?
  in(w_x) || c_y(d_x | [0]) || out([1])
    ->
  in() || c_y(d_x | [0]) || out(w'_x,y)

% Invariant: Exactly one container is linked to the name 'root'.
rule find_all_1 =	% find all devices (of type at most t), init
  in(f_t) || c_root(c([0])) || out([1])
    ->
  in(g) || c_root(c(f_t | s() | [0])) || out()

rule find_all_2 =	% found dev of leq type, add to s and out
  c(f_t | [0] | d_x,t' | s([1])) ||
  types(leq(left_t' | right_t)) ||
  out([2])
    ->
  c(f_t | [0] | s(d_x,t' | [1])) ||
  types(leq(left_t' | right_t)) ||
  out(f'_x,t | [2])

rule find_all_3 =	% found dev of gt type, add to s only
  c(f_t | [0] | d_x,t' | s([1])) ||
  types(gt(left_t' | right_t)) ||
  out([2])
    ->
  c(f_t | [0] | s(d_x,t' | [1])) ||
  types(gt(left_t' | right_t)) ||
  out([2])

rule find_all_4 =	% no more here, go up while s-merging
  c(c(f_t | s([2])) | s([1]) | [0])
    ->
  c(f_t | s(c([2]) | [1]) | [0])

rule find_all_5 =	% only containers here, go down
  c(f_t | c([1]) | s([2]) | [0])
    ->
  c(c(f_t | s() | [1]) | s([2]) | [0])

rule find_all_6 =	% done, 'out' has all - clean up s,f,g
  in(g) || c_root(c(f_t | s([0])))
    ->
  in() || c_root([0])

% The following two rules work, but distance links should be altered...
%
%rule range_dev_to_dev =	% lookup range between two devices
%  in(r_x,y) || c_i(d_x | [0]) || c_i(d_y | [1]) || out([2])
%    ->
%  in() || c_i(d_x | [0]) || c_i(d_y | [1]) || out(r'_x,y,i)
%
%rule range_dev_to_con =	% lookup range from device to leaf (container)
%  in(r_x,y) || c_i(d_x | [0]) || c_y,i([1]) || out([2])
%    ->
%  in() || c_i(d_x | [0]) || c_y,i([1]) || out(r_x,y,i)

rule near_1 =	% find nearest neighbour of type at most t, init
  in(n_x,t) || c(d_x,t' | [0]) || out([1])
    ->
  in(g) || c(n_x,t | d_x,t' | s() | [0]) || out()

rule near_2 =	% found one suitable in same container, done
  in(g) ||
  c(n_x,t | d_x,t' | d_y,t'' | s([1]) | [0]) ||
  types(leq(left_t'' | right_t)) ||
  out()
    ->
  in() ||
  c(d_x,t' | d_y,t'' | [1] | [0]) ||
  types(leq(left_t'' | right_t)) ||
  out(n'_x,y,t)

rule near_3 =	% found one of wrong type, add to s and stay
  c(n_x,t | d_x,t' | d_y,t'' | s([1]) | [0]) ||
  types(gt(left_t'' | right_t))
    ->
  c(n_x,t | d_x,t' | s(d_y,t'' | [1]) | [0]) ||
  types(gt(left_t'' | right_t))

rule near_4 =	% not here, go one up and create proxy d' plus 'a'
  c(c(n_x,t | d_x,t' | s([1])) | [0]) || 1
    ->
  c(s(d_x,t' | [1]) | n_z,t | [0]) || tmp(d'_z | a)

rule near_5 =	% only containers here, go down and create s there
  c(n_x,t | c([1]) | [0]) || tmp(d'_x | a)
    ->
  c(c(n_x,t | s() | [1]) | [0]) || tmp(d'_x | a)

rule near_6 =	% this c's children were empty, expand s and go up
  c(c(n_x,t | s([1])) | [0]) || tmp(d'_x | a)
    ->
  c(s(c([1])) | n_x,t | [0]) || tmp(d'_x | a)

rule near_7 =	% found wrong type somewhere, enclose in s but stay
  c(n_x,t | d_y,t'' | s([1]) | [0])
  types(gt(left_t'' | right_t)) ||
  tmp(d'_x | a)
    ->
  c(n_x,t | s(d_y,t'' | [1]) | [0])
  types(gt(left_t'' | right_t)) ||
  tmp(d'_x | a)

rule near_8 =	% found suitable somewhere, replace 'a' with n' and
		% disconnect n from d' while connecting d' with n'
  c(c(n_x,t | d_y,t'' | [1] | s([2])) | [0]) ||
  types(leq(left_t'' | right_t)) ||
  tmp(d'_x | a)
    ->
  c(s(c(d_y,t'' | [1] | [2])) | n_x,t | [0]) ||
  types(leq(left_t'' | right_t)) ||
  tmp(d'_p,u | n'_u,y,t)

rule near_9 =	% searched all here, go up and cont. cleaning
  c(c(n_t | s([0])) | [1]) || tmp(d'_u | n'_u,t)
   ->
  c(s(c([0])) | n_t | [1]) || tmp(d'_u | n'_u,t)

rule near_10 =	% enclose remaining c's before going up
  c(n_t | c([0]) | s([1]) | [2]) || tmp(d'_u | n'_u,t)
    ->
  c(n_t | s(c([0]) | [1]) | [2]) || tmp(d'_u | n'_u,t)

rule near_11 = % collapse s's, to secure that all s's are cleaned up
  c(n | s([0]) | s([1]) | [2]) || tmp(d')
    ->
  c(n) | s([0] | [1]) | [2]) || tmp(d')

rule near_12 =	% search w. proxy reached root *without* finding one
  c_root(c(n_t | s([0]))) || tmp(d'_u | a)
    ->
  c_root(c([0])) || tmp(d'_u | n'_u,v,t | void_v | e)

rule near_13 =	% search w. proxy reached root *with* one found
  c_root(c(n_t | s([0]))) || tmp(d'_u | n'_u,t)
    ->
  c_root(c([0])) || tmp(d'_u | n'_u,t | e)

rule near_14 =	% return, when we had to use proxy
  c(d_x,t) || d_y || tmp(d'_x,u | n'_u,y,t | e) || in(g)
    ->
  c(d_x,t) || d_y || out(n'_x,y,t) || in()

rule near_15 =	% return, special case
  c_root(c(d_x,t' | n_x,t)) || in(g) || 1
    ->
  c_root(c(d_x,t')) || out(n'_x,v,t | void_v) || in()

% can also find the distance to nearest d with type leq(t)
% by first calling 'near' and then just using dist-links, TODO