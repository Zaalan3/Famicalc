#!/bin/bash
# tools/perf/pbench.sh LABEL [wide] [fps] [PROGRAM.8xp]
# Every benchmark scene in one parallel batch (each measures 6-10 s of emulated
# time; zelda-items first runs ~50 s to reach the treasure list; results are
# deterministic, CPU load does not change them). wide: the wide picture; fps:
# the FPS counter on (Show FPS). PROGRAM defaults to this tree's
# bin/FAMICALC.8xp; give another build's .8xp (its .map must sit next to it)
# to measure a baseline. Output: one line per scene, "LABEL scene NES fps drawn
# fps", also kept in obj/perf/b-LABEL-*.txt (LABEL gets -wide and -fps added,
# so the batches can run at once).
cd "$(dirname "$0")/../.."
O=obj/perf; mkdir -p $O
label=$1; shift
wide=False; fps=False; prog=None
for a in "$@"; do
  case "$a" in
    wide) wide=True; label=$label-wide;;
    fps) fps=True; fps_l=1;;
    *.8xp) prog="'$(realpath "$a")'";;
  esac
done
[ -n "$fps_l" ] && label=$label-fps
export FC_OUT=${FC_OUT:-$PWD/tests/pc/out}
cd tests/pc
scenes="smb3-1-1 smb3-map tetris ff1-intro zelda-title smb3-run zelda-story zelda-items"
for s in $scenes; do
  python3 -c "
import sys; sys.argv=['x']; import fctest
fctest.bench($prog, '$label', scenes=['$s'], wide=$wide, fps_label=$fps)
" > ../../$O/b-$label-$s.txt 2>&1 &
done
wait
for s in $scenes; do cat ../../$O/b-$label-$s.txt; done
