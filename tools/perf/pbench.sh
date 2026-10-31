#!/bin/bash
# tools/perf/pbench.sh LABEL [wide] [PROGRAM.8xp]
# Every benchmark scene in one parallel batch (each measures 6-10 s of emulated
# time; zelda-items first runs ~50 s to reach the treasure list; results are
# deterministic, CPU load does not change them). PROGRAM defaults to this
# tree's bin/FAMICALC.8xp; give another build's .8xp (its
# .map must sit next to it) to measure a baseline. Output: one line per scene,
# "LABEL scene NES fps drawn fps", also kept in obj/perf/b-LABEL-*.txt
# (wide runs are labelled LABEL-wide so a normal and a wide batch can run at once).
cd "$(dirname "$0")/../.."
O=obj/perf; mkdir -p $O
label=$1; wide=False; [ "$2" = wide ] && { wide=True; label=$1-wide; }
prog=${3:-None}; [ "$prog" != None ] && prog="'$(realpath "$prog")'"
export FC_OUT=${FC_OUT:-$PWD/tests/pc/out}
cd tests/pc
scenes="smb3-1-1 smb3-map tetris ff1-intro zelda-title smb3-run zelda-story zelda-items"
for s in $scenes; do
  python3 -c "
import sys; sys.argv=['x']; import fctest
fctest.bench($prog, '$label', scenes=['$s'], wide=$wide)
" > ../../$O/b-$label-$s.txt 2>&1 &
done
wait
for s in $scenes; do cat ../../$O/b-$label-$s.txt; done
