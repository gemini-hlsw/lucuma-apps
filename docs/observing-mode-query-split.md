# Splitting the `observingMode` fetch: ODB ask + Explore plan

## Problem

The `AllProgramObservations` query
(`explore/app/src/clue/scala/queries/common/ProgramSummaryQueriesGQL.scala`)
pulls the full `observingMode` for every observation via
`ObservingModeSubquery` (`schemas/lib/src/main/scala/lucuma/schemas/odb/ObservingModeSubquery.scala`).
That subquery fans out across 7 instrument-mode branches (GMOS-N/S LongSlit,
GMOS-N/S Imaging, Flamingos2 LongSlit, IGRINS-2 LongSlit, GHOST IFU), each with
20-30 fields. The resulting SQL is large and hard to optimise, and it dominates
cold-load time for programs with many observations.

The plan is to move to a **"modes + 1"** architecture:

1. A bulk summary query that returns the list of observations plus the cheap
   fields (ids, workflow, group, science band, `observingModeType`, and the
   narrow set of fields needed to build `ObservingModeSummary`).
2. One bulk detail query per instrument (or per `ObservingModeType`, if the ODB
   adds the finer filter below), fetching the full `ObservingMode` only for
   observations of that kind.

`WhereObservation` already exposes an `instrument: WhereOptionEqInstrument`
filter, which is enough to prototype this client-side. GMOS-N and GMOS-S each
still span two mode tables (LongSlit + Imaging), so an instrument-filtered
detail query for GMOS joins two tables instead of seven. To collapse every
detail query to a single instrument-mode table we need one small addition from
the ODB.

## Ask for the ODB team

Add an `observingModeType` filter on `WhereObservation` so clients can
partition observations by mode.

### Concrete change

```graphql
input WhereObservation {
  # ... existing fields ...

  """Matches on the observation's observing mode type, if any."""
  observingModeType: WhereOptionEqObservingModeType
}
```

with the supporting input (standard shape, modelled on
`WhereOptionEqInstrument` / `WhereOptionEqSite`):

```graphql
input WhereOptionEqObservingModeType {
  IS_NULL: Boolean
  EQ: ObservingModeType
  NEQ: ObservingModeType
  IN: [ObservingModeType!]
  NIN: [ObservingModeType!]
}
```

`ObservingModeType` already exists as a GraphQL enum, so this is purely a new
filter input plus a resolver mapping to the existing SQL column.

### Why

- Lets Explore issue exactly one detail query per observing mode actually
  present in the program, hitting exactly one instrument-mode table per query.
- Without it, GMOS-N / GMOS-S still join two tables per detail query. GHOST,
  IGRINS-2, Flamingos2 are already single-table under an instrument filter.
- Pattern is consistent with the existing `instrument` and `site` filters.
- No schema-breaking changes, backwards compatible, and unblocks future
  per-mode tooling (counts, per-mode configuration-request matching, etc.).

### Priority / fallback

Not a blocker. We can ship the client-side split using the existing
`instrument` filter and pick up the finer filter when it's available. If the
ODB team agrees, the Explore wiring would switch from `instrument: {EQ ...}`
to `observingModeType: {EQ ...}` with a one-line change per detail query.

### Nice-to-haves (optional, lower priority)

- GraphQL `@defer` support on the `observingMode` field in the observation
  query. If available, the summary/detail split could collapse back into a
  single request with the server streaming the heavy branch after the cheap
  fields. Orthogonal to the filter ask above.
- A server-side resolver that returns only the populated instrument-mode
  branch rather than a union-shaped object with 6 null branches. Would reduce
  wire size even under the current client strategy.

---

## Client-side implementation plan

Sequential steps, each independently shippable.

### 1. Use the existing `BasicConfiguration` as the summary carrier

Rather than inventing a parallel narrow projection we reuse what already
exists:

- `lucuma.schemas.model.BasicConfiguration` is the canonical "lightweight
  observing mode" type and already carries `siteFor`, `obsModeType`,
  `centralWv`, `agsWavelength`, `conditionsWavelength`, `guideProbe`,
  `shortName`, etc.
- `lucuma.schemas.odb.BasicConfigurationSubquery` already projects exactly the
  GraphQL fields needed to decode it — no new GraphQL subquery required.
- `Observation.basicConfiguration` (currently a derived `lazy val` from
  `observingMode.toBasicConfiguration`) becomes a **field** populated directly
  from the bulk summary query. Call sites like `ObsBadge`, `ObsSummaryColumns`,
  and `ObsTabContents` keep working with no change in semantics.

The existing `ObservingModeSummary` enum (used only for
`ProgramSummaries.observingModeGroups`) can continue to be derived from the
full `ObservingMode` once detail loads. The observingModeGroups view simply
becomes per-observation `Pot` — the grouping populates as detail queries
resolve. If that's a problem in practice (a flashing groups tab), we can add
the `ampReadMode` / `roi` / `variant` fields to `BasicConfigurationSubquery`
and derive `ObservingModeSummary` from `BasicConfiguration` too. Not needed
for the first cut.

### 2. Split the bulk observation query

- `ObservationSubquery` gets two GraphQL changes:
  - Replace `observingMode $ObservingModeSubquery` with
    `observingMode $BasicConfigurationSubquery`.
  - The `BasicConfigurationSubquery` already returns `instrument`, which
    gives us the per-obs partitioning key for step 3 — no extra field needed.
- Keep everything else in `ObservationSubquery` (target environment,
  constraints, timing windows, workflow, execution, etc.) unchanged for this
  phase. We can trim workflow/execution later via the already-existing
  `obsCalcSubscription` orphans channel, but that's a separate change.

### 3. Add per-mode detail queries

Create one GraphQL trait per instrument (or per mode type, once the ODB filter
lands):

```
AllProgramObservationsGmosNorth      -- instrument: EQ GmosNorth
AllProgramObservationsGmosSouth      -- instrument: EQ GmosSouth
AllProgramObservationsFlamingos2     -- instrument: EQ Flamingos2
AllProgramObservationsIgrins2        -- instrument: EQ Igrins2
AllProgramObservationsGhost          -- instrument: EQ Ghost
```

Each returns `{ id, observingMode $ObservingModeSubquery }` only. Paginated
with the same `drain` helper in `OdbObservationApiImpl`.

### 4. Model changes in `explore.model.Observation`

```scala
case class Observation(
  ...
  basicConfiguration: Option[BasicConfiguration],       // field, from bulk
  observingMode:      Pot[Option[ObservingMode]],       // hydrated later
  ...
)
```

- `basicConfiguration` flips from a derived `lazy val` to a real field
  populated by the summary query. Existing call sites read it identically.
- `observingMode` wraps in `Pot`:
  - Fresh from the summary query: `Pot.Pending`.
  - After the matching detail query lands: `Pot.Ready(Some(mode))` or
    `Pot.Ready(None)` if the obs genuinely has no mode.
- `site` derives from `basicConfiguration.map(_.siteFor)`.
- The `observingModeSummary` accessor (used by `ProgramSummaries.observingModeGroups`)
  becomes `observingMode.toOption.flatten.map(ObservingModeSummary.fromObservingMode)`
  — returns `Option[ObservingModeSummary]`, so groups populate as detail
  queries resolve.
- Config panels (`GmosLongslitConfigPanel`, `Flamingos2LongslitConfigPanel`,
  `Igrins2LongslitConfigPanel`) gate themselves on `Pot.Ready` — show a
  skeleton while `Pending`.

### 4a. Lens update

`Observation.observingMode` goes from
`Lens[Observation, Option[ObservingMode]]` to
`Lens[Observation, Pot[Option[ObservingMode]]]`. Call sites that `zoom`
through it (e.g. `ObsTabTiles.scala:629`
`(Observation.posAngleConstraint, Observation.observingMode).disjointZip`)
need adjustment. Usually the fix is to add `.toOption.flatten` or a
`.zoom(Pot.readyOption)` helper.

### 4b. Arbitrary / testkit

`ArbObservation` needs updating to generate `Pot[Option[ObservingMode]]`
values. Simplest: always generate `Pot.Ready(arbObservingMode)` for
roundtrip parity with existing tests.

### 5. Wire the new flow into `ProgramCacheController`

- Replace `OdbObservationApi.allProgramObservations(programId)` with a
  sequential two-phase flow:
  1. `allProgramObservationsSummary(programId)` → `List[Observation]` with
     `observingMode = Pot.Pending` and `observingModeSummary` populated.
  2. After the summary lands, look at the distinct instruments actually
     present and fire the corresponding detail queries (in parallel via
     `parTraverse`). Merge each `ObservingMode` back into the matching
     `Observation` by id as it arrives.
- Present `ProgramSummaries` to the UI **as soon as the summary phase
  completes**. Don't hold the whole cache as `Pot.Pending` waiting on detail
  queries — the summary-only view gets the user to the list page faster, and
  individual config panels gate themselves on the per-obs `Pot`.
- Both the current `ProgramSummaries.fromLists` factory and the delta
  subscription plumbing stay the same; only the `initial` loader changes.

### 6. Subscription handling

- `programObservationsDeltaSubscription` currently carries the full
  `ObservationSubquery`. Two options:
  1. **Leave it alone.** Single-obs updates are cheap even with the 7-way
     join; the bottleneck was the bulk load, not the streaming path.
  2. **Split it too** to keep the wire consistent. Slightly more work; only
     worth doing if subscription latency is also a problem.
- Recommended: start with option 1.

### 7. Observability / rollout

- Add `logTime` markers around each phase:
  `AllProgramObservationsSummary`, and one per detail query
  (`AllProgramObservationsGmosNorth`, etc.).
- Ship directly on the new path — no feature flag. Rely on git-revert if we
  need to back out.
- Verify on a large-program fixture (many observations, several instruments)
  before rolling out broadly.

### 8. Cleanup

Once validated:

- Delete the old `ObservationSubquery` use of the full `ObservingMode` in the
  bulk query.
- When the ODB adds `observingModeType`, switch each detail query's WHERE
  clause from `instrument: {EQ ...}` to `observingModeType: {EQ ...}` and
  collapse the two GMOS queries into four mode-specific ones (or keep them
  instrument-wide if the two-table join turns out to be fine).

---

## Decisions

- `observingMode` on `Observation` is `Pot[Option[ObservingMode]]`.
- Detail queries run **sequentially** after the summary — we wait to see which
  instruments are actually present before firing per-instrument queries.
- `ProgramSummaries` is presented to the UI as soon as the summary phase
  completes; config panels gate themselves on the per-obs `Pot`.
- The `AllProgramObservations` trait is reused in place; no rename.
- The new path ships directly — no feature flag.
- Delta subscription payload stays as-is for now; revisit only if profiling
  flags it.
