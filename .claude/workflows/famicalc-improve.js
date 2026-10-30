export const meta = {
  name: 'famicalc-improve',
  description: 'One round of FamiCalc perf work: profile, plan from 3 angles, implement each candidate in its own worktree, test + bench + review, report the winners',
  whenToUse: 'When the user wants FamiCalc faster (60 fps goal). args: {candidates: 4, baseline: "path/to/FAMICALC.8xp", focus: "optional scene or area", profile: "optional profiler report text; skips Measure"}',
  phases: [
    { title: 'Measure', detail: 'bench all scenes normal + wide, profile the slow ones' },
    { title: 'Plan', detail: 'renderer / emulator-JIT / pacing planners, then one ranker' },
    { title: 'Implement', detail: 'one fc-engineer per candidate in an isolated worktree' },
    { title: 'Verify', detail: 'fc-tester + fc-reviewer on every committed candidate' },
  ],
}

// Role prompts, same text as .claude/agents/fc-*.md (embedded so the workflow does not
// depend on the agents being registered; keep both in sync).
const ROLE = {
  "PROFILER": "You are the measurement specialist for FamiCalc, an NES emulator in eZ80\nassembly for the TI-84 Plus CE. You never change source files. You build if\nbin/FAMICALC.8xp is missing or older than src/, then measure.\n\nMethod:\n1. `tools/perf/pbench.sh base` and `tools/perf/pbench.sh base wide`\n   (run both in the background at once, they are deterministic).\n2. For every scene under 59 drawn fps: `tools/perf/bgtrace.py SCENE [wide]`\n   and `tools/perf/prof2.py SCENE [wide]`. Run them in parallel too.\n3. Read DEVNOTES.md \"Frame pacing\", \"Row skipping\", \"Wide picture\" and the\n   cost notes so your candidates don't repeat things already tried\n   (two tile-pointer sets, per-row written masks: done or reverted).\n4. For each slow scene give the per-drawn-frame budget: emu, background\n   tiles (count + units), sprites (count + units), restores, row checks,\n   pacing waits, and the gap to 3120 units.\n\nReturn raw data, not prose: the bench table, the per-scene cost breakdown,\nand a ranked list of candidate optimizations, each with the scene it helps,\nthe units it could save, the files involved, and the evidence line that\nsupports it. Mark anything you are guessing as a guess.",
  "PLANNER": "You design optimizations for FamiCalc from the profiling data you are given\nand from reading the code. You are assigned ONE angle (renderer, emulator\nand JIT, or frame pacing); stay in it so other planners cover the rest.\n\nFor each proposal: name, the scene(s) it helps, the mechanism in one\nparagraph with the instruction-level reasoning, estimated units saved per\ndrawn frame (show the arithmetic), files and routines to change, RAM and\nprogram size cost, risk (what test could break), and how it can be\nverified. Proposals must be independent of each other so they can be\nimplemented in parallel worktrees; say so if two conflict.\n\nReject your own idea if DEVNOTES.md says it was tried and failed, if it\nneeds RAM that does not exist, or if it raises NES speed instead of\ndrawing speed. Three strong proposals beat eight weak ones.",
  "ENGINEER": "You are a senior eZ80 assembly engineer working on FamiCalc. You are given\nONE change to make. Make that change only; no drive-by refactors, no\nrenaming, no reformatting of lines you did not need to touch.\n\nWorking method:\n1. Read the relevant DEVNOTES.md sections and the code around the change\n   before editing. Check the memory map (vars.inc) before using any RAM.\n   Cursor RAM ($E30800-$E30BBF) has ~190 bytes free; render_cache layout is\n   fixed; check `make` output and the .map for size.\n2. Make the change. Build. If it does not assemble, fix it; if it needs a\n   different design than you were told, do the smallest version that keeps\n   the intent and say so in the result.\n3. Run the covering tests: at least `python3 tests/pc/fctest.py rotation\n   rowskip tearing panel_frames` plus the wide tests if renderwide.asm or\n   the wide pacing changed, then `tools/perf/ptest.sh` once it looks right.\n   Look at the screenshots of the scenes you touched in tests/pc/out.\n4. Bench: `tools/perf/pbench.sh after` (and `after wide`) and compare with\n   the baseline you were given. If the gain is under 0.5 drawn fps on the\n   target scene or anything else regressed, say so plainly; do not spin it.\n5. If tests pass and the change helps: commit (git add the files you\n   changed, message: what changed + fps before/after). If it does not help\n   or breaks tests after a reasonable fix attempt: `git checkout -- .` and\n   report why, with the numbers. Never leave uncommitted edits.\n\nReturn: commit sha (or \"reverted\"), the bench table before/after for all\nscenes, test results (N/N), program size before/after, and what you learned\nthat the next attempt should know. Raw facts, no adjectives.",
  "TESTER": "You verify FamiCalc builds. You do not edit source files.\n\n1. Build with the v14.2 toolchain. Note the program size from\n   bin/FAMICALC.8xp and the free RAM / Wide margin if the .map or build\n   output shows it.\n2. `make -B -C tests/unit`, `make -C tests/pc`, then `tools/perf/ptest.sh`.\n3. `tools/perf/pbench.sh verify` and `tools/perf/pbench.sh verify wide`.\n   If you were given a baseline .8xp path, also run both with it as the\n   3rd argument so the comparison is same-harness.\n4. Open (Read) the screenshots in tests/pc/out for: bench-verify-*.png,\n   panel-*.png, rotation*.png, tearing*.png, menu-*.png. Look for torn\n   rows, shifted status bars, garbage columns at the edges, missing\n   sprites, a wrong aspect in wide. A test passing does not excuse a bad\n   picture.\n5. Verdict: PASS only when every test passes and no scene lost more than\n   0.3 drawn fps against the baseline you were given (or the DEVNOTES table\n   when none was given).\n\nReturn: verdict, test counts, the bench table (baseline vs this build, all\nscenes, normal and wide), size, and any visual problem with the PNG path.",
  "REVIEWER": "You are a hostile reviewer of eZ80 assembly for FamiCalc. You are given a\ncommit or diff. Your job is to find what is wrong with it, not to praise it.\n\nCheck, with the code open:\n- Register and flag clobbers across the new paths (ix/iy, shadow regs, the\n  JIT's register conventions in jit.asm/codegen.asm, interrupt safety).\n- Memory: every new byte of RAM is inside the map in vars.inc and does not\n  overlap render_cache's reserved regions, cursor RAM, or the wide buffer.\n  Program size growth versus the stated budget.\n- Pacing/LCD: can the panel ever take a half-drawn frame or the wrong\n  rotation? Read DEVNOTES \"Frame pacing\" and judge the new path against it.\n- Cycle counts: for hot loops (draw_tile, sprite blits, row checks) count\n  the cycles of the old and new inner loop; a saving claimed from a\n  benchmark should be explainable from the instructions.\n- Wide picture and FPS-counter-on paths: both still correct?\n- Tests: does an existing PC test actually exercise the new code? If not,\n  say which test should be added and what it would assert.\n\nTry to refute the commit's claimed fps gain: if the bench numbers are\nwithin noise or came from a scene with inputs (see DEVNOTES on\ndeterminism), say so.\n\nReturn a list of findings: file:line, what is wrong, how it fails\n(concrete input or scene), severity (blocker / should-fix / nit). Then\none line: MERGE or DO NOT MERGE. Empty list + MERGE is a valid answer when\nyou genuinely found nothing."
}
const role = (r, p) => ROLE[r] + '\n\n' + p

const N = (args && args.candidates) || 4
const BASELINE = (args && args.baseline) || 'obj/perf/baseline/FAMICALC.8xp'
const FOCUS = (args && args.focus) ? `The user wants to focus on: ${args.focus}.` : ''
// args.worktrees: pre-created worktree paths, one per candidate (needed when the session's cwd is not
// the repo: the Agent tool's worktree isolation only works from inside a git repository).
const WT = (args && args.worktrees) || []

// ---- Measure -------------------------------------------------------------
phase('Measure')
// args.profile: a profiler report from an earlier run (skips the Measure phase).
const profile = (args && args.profile) || await agent(role('PROFILER',
  `Measure the current FamiCalc build in this worktree (build it first if bin/ is stale).
Bench all scenes normal and wide, profile every scene under 59 drawn fps with bgtrace and prof2
(normal and wide where wide is slow). ${FOCUS}
Return the bench table, per-scene cost breakdown in units per drawn frame, and the ranked candidate list.`),
  { label: 'profile', phase: 'Measure' })
if (!profile) throw new Error('profiler returned nothing')
log('profile done')

// ---- Plan ----------------------------------------------------------------
const PLAN = {
  type: 'object',
  properties: {
    proposals: {
      type: 'array',
      items: {
        type: 'object',
        properties: {
          name: { type: 'string' },
          scenes: { type: 'string', description: 'scenes it helps, with current drawn fps' },
          mechanism: { type: 'string', description: 'what to change and why it is faster, instruction-level' },
          units_saved: { type: 'string', description: 'estimated units/drawn frame saved, with arithmetic' },
          files: { type: 'string' },
          risk: { type: 'string' },
          verification: { type: 'string', description: 'tests and bench scenes that prove it' },
        },
        required: ['name', 'scenes', 'mechanism', 'units_saved', 'files', 'risk', 'verification'],
      },
    },
  },
  required: ['proposals'],
}
const ANGLES = [
  ['renderer', 'background tile drawing, sprite drawing and save-under/restore, row checks and compares, the wide picture renderer (render.asm, renderwide.asm, rowskip.asm)'],
  ['emulator-jit', 'the 6502 JIT and runtime: block headers, JSR/RTS, memory access paths, mapper bank switching, PPU register writes (jit.asm, codegen.asm, runtime.asm, io.asm, mapper.asm, optable.inc)'],
  ['pacing', 'frame pacing against the LCD: when frames are drawn or skipped, lateness budget, guards, draw estimates, hardware scroll use (pacing.asm, DEVNOTES Frame pacing)'],
]
phase('Plan')
const plans = (await parallel(ANGLES.map(([angle, scope]) => () =>
  agent(role('PLANNER', `Your angle: ${angle} — ${scope}. ${FOCUS}
Here is the profiler's report:\n\n${profile}\n\nPropose up to 3 independent optimizations within your angle.`),
    { label: `plan:${angle}`, phase: 'Plan', schema: PLAN })
))).filter(Boolean).flatMap(p => p.proposals)
log(`${plans.length} proposals from ${ANGLES.length} planners`)

const RANK = {
  type: 'object',
  properties: {
    chosen: {
      type: 'array',
      items: {
        type: 'object',
        properties: {
          name: { type: 'string' },
          brief: { type: 'string', description: 'complete, self-contained instructions for the engineer: files, routines, mechanism, what to measure, what must not break' },
          target_scenes: { type: 'string' },
          why: { type: 'string' },
        },
        required: ['name', 'brief', 'target_scenes', 'why'],
      },
    },
    dropped: { type: 'array', items: { type: 'string' }, description: 'name: reason, for every proposal not chosen' },
  },
  required: ['chosen', 'dropped'],
}
const ranked = await agent(
  `You are the tech lead. Profiler report:\n\n${profile}\n\nProposals from three planners:\n\n${JSON.stringify(plans, null, 2)}\n
Pick at most ${N} proposals to implement in parallel. They must touch different code so they can be
implemented in separate worktrees and merged in sequence; prefer the largest credible units saved
on the slowest scenes (zelda-title, smb3-run, and their wide variants), penalize RAM and program-size
cost and anything DEVNOTES.md says was already tried. Write each brief so an engineer who has only the
repo and that brief can do it. List every dropped proposal with the reason.`,
  { label: 'rank', phase: 'Plan', schema: RANK })
if (!ranked || !ranked.chosen.length) throw new Error('no candidates chosen')
log(`implementing ${ranked.chosen.length}: ${ranked.chosen.map(c => c.name).join(', ')}`)
for (const d of ranked.dropped) log(`dropped: ${d}`)

// ---- Implement → Verify (pipeline: each candidate verified as soon as it lands) ----
const RESULT = {
  type: 'object',
  properties: {
    status: { type: 'string', enum: ['committed', 'reverted', 'failed'] },
    sha: { type: 'string' },
    worktree: { type: 'string', description: 'absolute path of the worktree you worked in (git rev-parse --show-toplevel)' },
    bench: { type: 'string', description: 'before/after table, all scenes, normal and wide' },
    tests: { type: 'string' },
    size: { type: 'string', description: 'program size before/after' },
    notes: { type: 'string', description: 'what the next attempt should know' },
  },
  required: ['status', 'worktree', 'bench', 'tests', 'notes'],
}
const VERIFY = {
  type: 'object',
  properties: {
    verdict: { type: 'string', enum: ['PASS', 'FAIL'] },
    report: { type: 'string' },
  },
  required: ['verdict', 'report'],
}
const REVIEW = {
  type: 'object',
  properties: {
    decision: { type: 'string', enum: ['MERGE', 'DO NOT MERGE'] },
    findings: { type: 'array', items: { type: 'string' } },
  },
  required: ['decision', 'findings'],
}

const results = await pipeline(
  ranked.chosen,
  (c, _, i) => agent(role('ENGINEER',
    `Implement exactly this change in ${WT[i] ? `the worktree ${WT[i]} (cd there first; it is your own copy of the repo on its own branch)` : 'the worktree you are in (you have your own copy of the repo)'};
you can build, test and commit freely.\n\nNAME: ${c.name}\nTARGET SCENES: ${c.target_scenes}\n\nBRIEF:\n${c.brief}\n
Baseline numbers (current build, before your change):\n${profile.slice(0, 4000)}\n
A baseline .8xp of the unchanged build is at ${BASELINE} (absolute path relative to the main worktree
${args && args.main ? args.main : ''}); you may pass it to tools/perf/pbench.sh as the 3rd argument for a same-harness comparison.
Report worktree = output of git rev-parse --show-toplevel.`),
    { label: `impl:${c.name}`, phase: 'Implement', schema: RESULT, ...(WT[i] ? {} : { isolation: 'worktree' }) }),
  async (r, c) => {
    if (!r || r.status !== 'committed') return { candidate: c.name, impl: r, verify: null, review: null }
    const [verify, review] = await parallel([
      () => agent(role('TESTER', `Verify the FamiCalc build in the worktree ${r.worktree} (cd there; build and test THAT tree, not any other).
Baseline build for comparison: ${BASELINE} (resolve it from ${args && args.main ? args.main : 'the main worktree'}).
The change under test: ${c.name} — commit ${r.sha}. Engineer's claimed result:\n${r.bench}`),
        { label: `test:${c.name}`, phase: 'Verify', schema: VERIFY }),
      () => agent(role('REVIEWER', `Review commit ${r.sha} in the worktree ${r.worktree} (cd there; git show ${r.sha}).
It claims: ${c.name}. Engineer's bench table:\n${r.bench}\nEngineer's notes: ${r.notes}\nTry to refute the gain and find what breaks.`),
        { label: `review:${c.name}`, phase: 'Verify', schema: REVIEW }),
    ])
    return { candidate: c.name, impl: r, verify, review }
  },
)

const out = results.filter(Boolean)
const winners = out.filter(x => x.impl && x.impl.status === 'committed'
  && x.verify && x.verify.verdict === 'PASS' && x.review && x.review.decision === 'MERGE')
log(`${winners.length} of ${out.length} candidates passed test + review`)
return {
  baseline_profile: profile,
  dropped: ranked.dropped,
  candidates: out,
  winners: winners.map(w => ({ name: w.candidate, sha: w.impl.sha, worktree: w.impl.worktree, bench: w.impl.bench, size: w.impl.size })),
}
