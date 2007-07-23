
MATCH
SIGNATURE
[active0(K), passive0(L)]
ENDSIGNATURE
AGENT
K o L o 1
ENDAGENT
RULES
[{
  redex = M[][[x]] o (<[x]> (x//[x1,x0] * merge(2)) o (`[x0]` * `[x1]`)),
  react = N[][[z0]] * N[][[z1]],
  inst = [0&[z0] |--> 1&[x1], 1&[z1] |--> 1&[x1]]
 },
 {
  redex = K,
  react = L o K,
  inst = [0|->0]
 }
]
ENDRULES
USERULES:-1
MATCHCOUNT:-1
ENDMATCH