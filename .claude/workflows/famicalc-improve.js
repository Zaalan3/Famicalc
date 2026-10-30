export const meta = {
  name: 'famicalc-improve',
  description: 'One round of FamiCalc perf work: profile, plan from 3 angles, implement each candidate in its own worktree, test + bench + review, report the winners',
  whenToUse: 'When the user wants FamiCalc faster (60 fps goal). args: {candidates: 4, baseline: "path/to/FAMICALC.8xp", focus: "optional scene or area"}',
  phases: [
    { title: 'Measure', detail: 'bench all scenes normal + wide, profile the slow ones' },
    { title: 'Plan', detail: 'renderer / emulator-JIT / pacing planners, then one ranker' },
    { title: 'Implement', detail: 'one fc-engineer per candidate in an isolated worktree' },
    { title: 'Verify', detail: 'fc-tester + fc-reviewer on every committed candidate' },
  ],
}

const N = (args && args.candidates) || 4
const BASELINE = (args && args.baseline) || 'obj/perf/baseline/FAMICALC.8xp'
const FOCUS = (args && args.focus) ? `The user wants to focus on: ${args.focus}.` : ''

// ---- Measure -------------------------------------------------------------
phase('Measure')
const profile = await agent(
  `Measure the current FamiCalc build in this worktree (build it first if bin/ is stale).
Bench all scenes normal and wide, profile every scene under 59 drawn fps with bgtrace and prof2
(normal and wide where wide is slow). ${FOCUS}
Return the bench table, per-scene cost breakdown in units per drawn frame, and the ranked candidate list.`,
  { agentType: 'fc-profiler', label: 'profile', phase: 'Measure' })
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
  agent(`Your angle: ${angle} — ${scope}. ${FOCUS}
Here is the profiler's report:\n\n${profile}\n\nPropose up to 3 independent optimizations within your angle.`,
    { agentType: 'fc-planner', label: `plan:${angle}`, phase: 'Plan', schema: PLAN })
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
  (c, _, i) => agent(
    `Implement exactly this change in the worktree you are in (you have your own copy of the repo;
you can build, test and commit freely).\n\nNAME: ${c.name}\nTARGET SCENES: ${c.target_scenes}\n\nBRIEF:\n${c.brief}\n
Baseline numbers (current build, before your change):\n${profile.slice(0, 4000)}\n
A baseline .8xp of the unchanged build is at ${BASELINE} (absolute path relative to the main worktree
${args && args.main ? args.main : ''}); you may pass it to tools/perf/pbench.sh as the 3rd argument for a same-harness comparison.
Report worktree = output of git rev-parse --show-toplevel.`,
    { agentType: 'fc-engineer', label: `impl:${c.name}`, phase: 'Implement', schema: RESULT, isolation: 'worktree' }),
  async (r, c) => {
    if (!r || r.status !== 'committed') return { candidate: c.name, impl: r, verify: null, review: null }
    const [verify, review] = await parallel([
      () => agent(`Verify the FamiCalc build in the worktree ${r.worktree} (cd there; build and test THAT tree, not any other).
Baseline build for comparison: ${BASELINE} (resolve it from ${args && args.main ? args.main : 'the main worktree'}).
The change under test: ${c.name} — commit ${r.sha}. Engineer's claimed result:\n${r.bench}`,
        { agentType: 'fc-tester', label: `test:${c.name}`, phase: 'Verify', schema: VERIFY }),
      () => agent(`Review commit ${r.sha} in the worktree ${r.worktree} (cd there; git show ${r.sha}).
It claims: ${c.name}. Engineer's bench table:\n${r.bench}\nEngineer's notes: ${r.notes}\nTry to refute the gain and find what breaks.`,
        { agentType: 'fc-reviewer', label: `review:${c.name}`, phase: 'Verify', schema: REVIEW }),
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
