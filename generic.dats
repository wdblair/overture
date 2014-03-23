staload "rat.sats"
staload "flow.sats"


(**

In order to get this program I'd have to use the Prelude
inference rules to determine the types of i,j. Once I do
that, I'd see the contradiction immediately in the fails
function, so producing a corresponding ATS program would
be a little futile.

imported node imp(i, j: int) returns (o: bool) wcet 5;

node fails(i, j) returns (o)
var v;
let
    v=imp(i, j);
    o=imp(v, i);
tel
*)

extern fun 
imp {n,m: nat} {p,q:rat} (
  i: strict_flow (int, n, p), j: strict_flow (int, m, q)
): [o:nat] [r:rat] strict_flow (bool, o, r)

fun fails {n,m:nat} {p,q:rat} (
  i: strict_flow (int, n, p), j: strict_flow (int, m, q)
): [o:nat] [r:rat] strict_flow (bool, o, r) = let
  val v = imp (i, j)
  val o = imp (v, i)
in
  o
end