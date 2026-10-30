#!/bin/bash
# tools/perf/ptest.sh [N]: every PC test, N at a time (default 6), on this
# tree's bin/FAMICALC.8xp. Prints the PASS/FAIL lines and then the names of
# tests that neither passed nor failed (crashed). Logs in obj/perf/tests/.
cd "$(dirname "$0")/../.."
out=obj/perf/tests; rm -rf $out; mkdir -p $out
cd tests/pc
python3 fctest.py list | tr ' ' '\n' | xargs -P ${1:-6} -I{} sh -c "python3 fctest.py {} > ../../$out/{}.txt 2>&1"
cd ../..
grep -hE "^(PASS|FAIL)" $out/*.txt
echo "passed: $(grep -l '^PASS' $out/*.txt | wc -l) / $(ls $out/*.txt | wc -l)"
crashed=$(grep -L -E "^(PASS|FAIL)" $out/*.txt)
[ -n "$crashed" ] && echo "no verdict (crashed?): $crashed"
exit 0
