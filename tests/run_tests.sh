#!/bin/bash
#
# Overture test harness: tests/pos/*.ovt must typecheck (exit 0);
# tests/neg/*.ovt must fail (exit 1) with stderr matching the
# adjacent .err pattern file.
#

set -u

here="$(cd "$(dirname "$0")" && pwd)"
overture="$here/../src/overture"

if [ ! -x "$overture" ]; then
  echo "error: $overture not built (run make first)" >&2
  exit 1
fi

pass=0
fail=0

for f in "$here"/pos/*.ovt; do
  name="pos/$(basename "$f")"
  if out=$("$overture" --typecheck "$f" 2>&1); then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    echo "FAIL $name (expected success)"
    echo "$out" | sed 's/^/  | /'
  fi
done

for f in "$here"/neg/*.ovt; do
  name="neg/$(basename "$f")"
  errfile="${f%.ovt}.err"
  out=$("$overture" --typecheck "$f" 2>&1)
  status=$?
  if [ "$status" -eq 0 ]; then
    fail=$((fail + 1))
    echo "FAIL $name (expected failure, got success)"
    continue
  fi
  if [ -f "$errfile" ] && ! echo "$out" | grep -qE "$(cat "$errfile")"; then
    fail=$((fail + 1))
    echo "FAIL $name (wrong error; expected /$(cat "$errfile")/)"
    echo "$out" | sed 's/^/  | /'
    continue
  fi
  pass=$((pass + 1))
done

# ---- code generation: generate, compile with patscc, run, check ----

export PATSHOME="${PATSHOME:-$here/../ATS2-Postiats-int-0.4.2}"
export PATH="$PATSHOME/bin:$PATH"
gendir=$(mktemp -d)

codegen_build () {
  local base="$1"
  (cd "$gendir" \
    && "$overture" --codegen=ats "$here/pos/$base.ovt" > /dev/null \
    && patscc -DATS_MEMALLOC_LIBC -o "$base" \
         "${base}_gen.dats" "${base}_stubs.dats" \
         overture_sched.dats > /dev/null 2>&1)
}

codegen_build_c () {
  local base="$1"
  (cd "$gendir" \
    && "$overture" --codegen=c "$here/pos/$base.ovt" > /dev/null \
    && cc -std=c99 -O2 -o "${base}_c" \
         "${base}_gen.c" "${base}_stubs.c" > /dev/null 2>&1)
}

if codegen_build sampling && "$gendir/sampling" 320 > "$gendir/sampling.out"; then
  if grep -q '^\[t=0\] alert = 0$' "$gendir/sampling.out" \
     && grep -q '^\[t=100\] alert = 101$' "$gendir/sampling.out" \
     && grep -q '^\[t=200\] alert = 302$' "$gendir/sampling.out" \
     && grep -q '^\[t=300\] alert = 603$' "$gendir/sampling.out"; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    echo "FAIL codegen/sampling (unexpected output)"
    cat "$gendir/sampling.out" | sed 's/^/  | /'
  fi
else
  fail=$((fail + 1))
  echo "FAIL codegen/sampling (generate/compile/run)"
fi

for base in operators exi_output rat_phase current_hold kind_poly sched; do
  if codegen_build "$base" && "$gendir/$base" > /dev/null; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    echo "FAIL codegen/$base (generate/compile/run)"
  fi
done

# gated actuation: present ticks carry values, absent ticks are silent
if codegen_build when && "$gendir/when" 400 > "$gendir/when.out"; then
  if grep -q '^\[t=0\] alert = 0$' "$gendir/when.out" \
     && grep -q '^\[t=300\] alert = 300$' "$gendir/when.out" \
     && ! grep -q '\[t=100\]' "$gendir/when.out" \
     && ! grep -q '\[t=200\]' "$gendir/when.out"; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    echo "FAIL codegen/when (unexpected gating)"
    cat "$gendir/when.out" | sed 's/^/  | /'
  fi
else
  fail=$((fail + 1))
  echo "FAIL codegen/when (generate/compile/run)"
fi

# record flows: both backends, byte-identical transcripts
if codegen_build record_flow && "$gendir/record_flow" 1000 > "$gendir/record_flow.out" \
   && codegen_build_c record_flow \
   && "$gendir/record_flow_c" 1000 > "$gendir/record_flow_c.out"; then
  if grep -q '^\[t=0\] rec = {3, 4, 5}$' "$gendir/record_flow.out" \
     && grep -q '^\[t=100\] rec = {303, 304, 305}$' "$gendir/record_flow.out" \
     && grep -q '^\[t=0\] led = 3$' "$gendir/record_flow.out" \
     && diff -q "$gendir/record_flow.out" "$gendir/record_flow_c.out" > /dev/null; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    echo "FAIL codegen/record_flow (bad transcript or backend divergence)"
    diff "$gendir/record_flow.out" "$gendir/record_flow_c.out" | sed 's/^/  | /'
  fi
else
  fail=$((fail + 1))
  echo "FAIL codegen/record_flow (generate/compile/run)"
fi

# ---- the schedule certificate rejects infeasible task sets ----
#
# sched_infeasible typechecks in Overture (each task alone fits its
# period) but its generated ATS harness must NOT compile: patsopt
# refuses the slot that sums 6 + 6 > 10.

if (cd "$gendir" \
      && "$overture" --codegen=ats "$here/pos/sched_infeasible.ovt" > /dev/null) \
   && ! (cd "$gendir" \
      && patscc -DATS_MEMALLOC_LIBC -o sched_infeasible \
           sched_infeasible_gen.dats sched_infeasible_stubs.dats \
           overture_sched.dats > /dev/null 2>&1); then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
  echo "FAIL certificate/sched_infeasible (patsopt accepted an infeasible set)"
fi

# ---- the C backend: same programs, same goldens, built with cc ----

if codegen_build_c sampling && "$gendir/sampling_c" 320 > "$gendir/sampling_c.out"; then
  if diff -q "$gendir/sampling.out" "$gendir/sampling_c.out" > /dev/null; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    echo "FAIL codegen-c/sampling (output differs from the ATS backend)"
    diff "$gendir/sampling.out" "$gendir/sampling_c.out" | sed 's/^/  | /'
  fi
else
  fail=$((fail + 1))
  echo "FAIL codegen-c/sampling (generate/compile/run)"
fi

# note sched_infeasible: the C backend carries no certificate, so it
# builds and runs -- the documented asymmetry
for base in operators exi_output rat_phase current_hold kind_poly when sched sched_infeasible; do
  if codegen_build_c "$base" && "$gendir/${base}_c" > /dev/null; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    echo "FAIL codegen-c/$base (generate/compile/run)"
  fi
done

# ---- stage flags compose in one invocation ----

if (cd "$gendir" && "$overture" --typecheck --emit-graph --codegen=c \
      "$here/pos/sampling.ovt" > "$gendir/combined.out" 2>&1) \
   && grep -q '^typechecking succeeded$' "$gendir/combined.out" \
   && grep -q '"sensors"' "$gendir/combined.out" \
   && [ -f "$gendir/sampling_gen.c" ]; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
  echo "FAIL combined-flags invocation"
fi

rm -rf "$gendir"

echo "----"
echo "passed: $pass, failed: $fail"
[ "$fail" -eq 0 ]
