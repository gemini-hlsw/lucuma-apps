# Plan: Refactor observe engine from atom-based to step-based execution

## Context

The observe engine in `observe/server_new` currently operates atom-by-atom: it loads an entire atom (which contains multiple steps) from ODB, executes all steps within it, then loads the next atom. The goal is to simplify the engine to load and execute **one step at a time**, loading the next step just before executing it, and unloading it upon completion. When idle, no step should be loaded.

The ODB API still returns atoms with nested steps — we extract individual steps from the atom response.

**Acquisition prompt behavior**: Pause when an acquisition atom completes. Detect this by querying the ODB for `nextAtom` after a step completes and comparing its `atomId` to the just-completed step's `atomId`. If different (and the completed step was acquisition type), show the prompt.

---

## Implementation Steps

### Step 1: Rename `IsWaitingNextAtom` → `IsWaitingNextStep` in SequenceState

**File**: `observe/model/shared/src/main/scala/observe/model/SequenceState.scala`

Rename `IsWaitingNextAtom` → `IsWaitingNextStep` and `waitingNextAtom` → `waitingNextStep` in the `Running` case class + all pattern matches across the codebase. Mechanical rename.

**Affected files** (grep for `waitingNextAtom` and `IsWaitingNextAtom`):
- `observe/model/shared/src/main/scala/observe/model/SequenceState.scala`
- `observe/server_new/src/main/scala/observe/server/engine/Engine.scala`
- `observe/server_new/src/main/scala/observe/server/ObserveEngine.scala`
- `observe/server_new/src/main/scala/observe/server/ObserveEngineImpl.scala`
- Any test files referencing these

---

### Step 2: Simplify step types, remove `SequenceGen` and `Sequence`

#### 2a. Remove dead and unnecessary types from `SequenceGen.scala`
- **Remove `CompletedStepGen`** — never constructed anywhere (dead code).
- **Remove `InstrumentStepGen`** — only implementor was `StepGen`.
- **Remove `StepGen` and `PendingStepGen`** — fields inlined into `CurrentStep`.
- **Remove `AtomGen`** — data moves into `CurrentStep`.
- **Remove `SequenceGen`** — its fields (`obsData`, `staticCfg`, `instrument`) move into `SequenceData`.
- **Keep `StepActionsGen`** — still needed for action pipeline generation within a step.
- **Keep `StepStatusGen`** — still referenced.

#### 2b. Introduce `CurrentStep[F[_], D]`
**File**: new type in `observe/server_new/src/main/scala/observe/server/CurrentStep.scala` (or in `SequenceGen.scala` renamed)

```scala
case class CurrentStep[F[_], D](
  atomId:          Atom.Id,
  sequenceType:    SequenceType,
  id:              Step.Id,
  dataId:          DataId,
  resources:       Set[Resource | Instrument],
  obsControl:      SystemOverrides => InstrumentSystem.ObserveControl[F],
  generator:       StepActionsGen[F],
  instConfig:      D,
  config:          StepConfig,
  telescopeConfig: CoreTelescopeConfig,
  signalToNoise:   Option[SignalToNoise],
  breakpoint:      Breakpoint
)
```

Companion object has `generate(step, overrides, headerExtra): (EngineStep[F], Breakpoint)` (moved from `StepGen.generate`).

#### 2c. Simplify `Sequence.State` and remove `Sequence.Zipper` from engine

**File**: `observe/server_new/src/main/scala/observe/server/engine/Sequence.scala`

`Sequence` held multiple steps with a Zipper to track multi-step progress. With single-step execution, the outer `Sequence.Zipper` is unnecessary. What remains is:

- **`EngineStep.Zipper`** — still needed to track progress through execution groups within a step (preStep → configs → observe → postStep).
- **`Execution[F]`** — still needed to track parallel actions within an execution group.

Keep the name **`Sequence.State[F]`** but simplify its internals:

```scala
case class State[F[_]](
  obsId:       Observation.Id,
  status:      SequenceState,
  currentStep: Option[EngineStep.Zipper[F]],  // None = idle/done, Some = executing
  breakpoints: Breakpoints,
  singleRuns:  Map[ActionCoordsInSeq, ActionState]
)
```

Key simplifications vs old `Sequence.State`:
- No `Zipper`/`Final` sealed trait — `currentStep: Option` covers both cases (`None` = idle/done, `Some` = active)
- No `pending`/`done` step lists — only one step at a time
- No `Sequence.Zipper` wrapper — `EngineStep.Zipper` is accessed directly
- `next` delegates to `currentStep.flatMap(_.next)` and wraps result
- `current` = `currentStep.map(_.focus).getOrElse(Execution.empty)`
- `isLastAction` = `currentStep.forall(_.pending.isEmpty)`
- Pattern matching on `Zipper`/`Final` becomes `currentStep.isDefined` / `currentStep.isEmpty`

Preserve the companion object utility methods (`isRunning`, `canUnload`, `userStopSet`, `internalStopSet`, status lens, etc.) adapted to the new structure.

#### 2d. Refactor `SequenceData[F]`
**File**: `observe/server_new/src/main/scala/observe/server/SequenceData.scala`

Follow the same sealed-trait-with-type-members pattern as `SequenceGen` to preserve type safety for `S` and `D`:

```scala
sealed trait SequenceData[F[_]] {
  type S
  type D
  def instrument: Instrument

  def observer:       Option[Observer]
  def overrides:      SystemOverrides
  def obsData:        OdbObservation
  def staticCfg:      S
  def currentStep:    Option[CurrentStep[F, D]]
  def seq:            Sequence.State[F]
  def pendingObsCmd:  Option[PendingObserveCmd]
  def visitStartDone: Boolean
  def cleanup:        F[Unit]
}

object SequenceData {
  case class GmosNorth[F[_]](
    observer:       Option[Observer],
    overrides:      SystemOverrides,
    obsData:        OdbObservation,
    staticCfg:      gmos.StaticConfig.GmosNorth,
    currentStep:    Option[CurrentStep[F, gmos.DynamicConfig.GmosNorth]],
    seq:            Sequence.State[F],
    pendingObsCmd:  Option[PendingObserveCmd],
    visitStartDone: Boolean,
    cleanup:        F[Unit]
  ) extends SequenceData[F] {
    type S = gmos.StaticConfig.GmosNorth
    type D = gmos.DynamicConfig.GmosNorth
    val instrument: Instrument = Instrument.GmosNorth
  }

  case class GmosSouth[F[_]](
    observer:       Option[Observer],
    overrides:      SystemOverrides,
    obsData:        OdbObservation,
    staticCfg:      gmos.StaticConfig.GmosSouth,
    currentStep:    Option[CurrentStep[F, gmos.DynamicConfig.GmosSouth]],
    seq:            Sequence.State[F],
    pendingObsCmd:  Option[PendingObserveCmd],
    visitStartDone: Boolean,
    cleanup:        F[Unit]
  ) extends SequenceData[F] {
    type S = gmos.StaticConfig.GmosSouth
    type D = gmos.DynamicConfig.GmosSouth
    val instrument: Instrument = Instrument.GmosSouth
  }

  case class Flamingos2[F[_]](
    observer:       Option[Observer],
    overrides:      SystemOverrides,
    obsData:        OdbObservation,
    staticCfg:      Flamingos2StaticConfig,
    currentStep:    Option[CurrentStep[F, Flamingos2DynamicConfig]],
    seq:            Sequence.State[F],
    pendingObsCmd:  Option[PendingObserveCmd],
    visitStartDone: Boolean,
    cleanup:        F[Unit]
  ) extends SequenceData[F] {
    type S = Flamingos2StaticConfig
    type D = Flamingos2DynamicConfig
    val instrument: Instrument = Instrument.Flamingos2
  }
}
```

This replaces `SequenceGen` entirely — `SequenceData` now owns both the ODB data (`obsData`, `staticCfg`, `instrument`, `currentStep`) and the execution state (`seq`, `observer`, `overrides`, etc.). No more separate `seqGen` field.

Helper methods from `SequenceGen` (`resources`, `configActionCoord`, `resourceAtCoords`) move to methods on `SequenceData` that delegate to `currentStep`.

---

### Step 3: Update `SeqTranslate` to produce `CurrentStep`

**File**: `observe/server_new/src/main/scala/observe/server/SeqTranslate.scala`

Replace `nextAtom()` with:
```scala
def nextStep(
  odbObsData: OdbObservationData,
  atomType:   SequenceType
): (List[Throwable], Option[SequenceGen.CurrentStep[F]])
```

This method:
1. Extracts the atom from ODB data (same logic as current `buildNextAtom`)
2. Takes the **first step** from the atom (ODB always returns only pending steps — completed steps are removed)
3. Translates it directly into a `CurrentStep` (with `atomId`, `sequenceType` from the atom)
4. Returns `None` if no more atoms/steps available

Remove `nextAtom()` and `buildNextAtom()` — replace with `buildNextStep()` that produces `CurrentStep[F, D]` directly. The per-instrument `constructAtom` lambdas are no longer needed since `CurrentStep` is a single case class — just construct it directly with the appropriate `D` type.

---

### Step 4: Update `toStepList` helper in `package.scala`

**File**: `observe/server_new/src/main/scala/observe/server/package.scala`

Simplify to generate 0 or 1 `EngineStep` from the current step:
```scala
def toEngineStep[F[_]](step: CurrentStep[F, ?], overrides: SystemOverrides, headerExtra: HeaderExtraData): (EngineStep[F], Breakpoint) =
  CurrentStep.generate(step, overrides, headerExtra)
```

---

### Step 5: Update `Engine.scala` for step-based execution

**File**: `observe/server_new/src/main/scala/observe/server/engine/Engine.scala`

1. Rename constructor params: `atomLoad` → `stepLoad`, `atomReload` → `stepReload`
2. Rename `Engine.build` params: `loadNextAtom` → `loadNextStep`, `reloadNextAtom` → `reloadNextStep`
3. Rename `startNewAtom` → `startNewStep` (same logic, just renamed)
4. **Update `Sequence.State` usage** (name stays, but internals simplified per step 2c):
   - Pattern matches on `Zipper`/`Final` become checks on `currentStep.isDefined`/`currentStep.isEmpty`
   - `Engine.execute`: instead of matching `State.Zipper(z, ...)`, check `state.currentStep` for `Some(z)` to get step id and current actions
   - `Engine.next`: instead of matching `State.Final`, check `currentStep.isEmpty` after advancing — if done, call `stepLoad`
   - `Engine.startNewStep`: instead of matching `Final`, check `currentStep.isEmpty`
5. **Simplify `next()`**: With single steps, after the step's execution groups complete:
   - `currentStep.flatMap(_.next)` returns `None` → step is done → call `stepLoad(this, obsId)` to load next
   - If `Some(newZipper)` → still within execution groups → emit `executing`
   - The `atomReload` between steps is replaced by `stepLoad` when the step completes

6. Remove `Engine.load` and `Engine.reload` — these constructed `Sequence.State`. Replace with `Sequence.State.init` / `Sequence.State.reload` equivalents.

---

### Step 6: Replace atom orchestration in `ObserveEngine.scala`

**File**: `observe/server_new/src/main/scala/observe/server/ObserveEngine.scala`

1. **Replace `onAtomComplete` with `onStepComplete`**:
   - Gets the completed step's `sequenceType` and `atomId` from `seqData.currentStep`
   - Reads ODB to get the next step via `tryNewStep`
   - **Acquisition prompt detection**: After loading the next step, compare `atomId`. If the completed step was `Acquisition` type AND the new step's `atomId` differs (meaning the acquisition atom is fully done), emit `SeqEvent.AtomCompleted(obsId, Acquisition, completedAtomId)` and don't auto-start. Otherwise, auto-start the next step.

2. **Replace `onAtomReload` with `onStepReload`**: Same pattern, calls `nextStep` instead of `nextAtom`.

3. **Replace `tryNewAtom` with `tryNewStep`**:
   - Calls `odb.read(obsId)`
   - Calls `translator.nextStep(odbObsData, atomType)`
   - Calls `updateStep(obsId, nextStep)` to set `currentStep` and re-initialize `Sequence.State`
   - Calls `executeEngine.startNewStep(obsId)` to resume
   - Returns `SeqEvent.NewStepLoaded(...)` or `SeqEvent.NoMoreAtoms(...)`

4. **Replace `updateAtom` with `updateStep`**:
   - Sets `seqData.currentStep` to the new `CurrentStep`
   - Generates `EngineStep` from `CurrentStep` and initializes `Sequence.State` with it
   - Same status transition logic (Completed → Idle if step available, → Completed if none)

5. **Update `build()`**: Wire `onStepComplete` and `onStepReload`.

---

### Step 7: Update `ObserveEngineImpl.scala`

**File**: `observe/server_new/src/main/scala/observe/server/ObserveEngineImpl.scala`

1. Rename `loadNextAtom` → `loadNextStep`, `userNextAtom` → `userNextStep`
2. Update `findStartingStep` and `findFirstCheckRequiredStep`: access `seqData.currentStep` directly
3. Update `viewSequence`: access `seqData.currentStep.map(_.sequenceType)`, use `seqData.seq` (same field name, simplified internals)
4. Update event mapping in `toClientModifyEventStream`: `NewStepLoaded` → `ClientEvent.StepLoaded` (or similar)
5. Update `processObsEditOdbSignal`: use `onStepReload` instead of `onAtomReload`

---

### Step 8: Update SeqEvent and ClientEvent

**`observe/server_new/src/main/scala/observe/server/SeqEvent.scala`**:
- Rename `NewAtomLoaded` → `NewStepLoaded`, add `stepId: Step.Id`
- Keep `AtomCompleted` (still used for acquisition prompt — emitted when an acquisition atom finishes)
- Keep `NoMoreAtoms` (ODB says sequence is done)

**`observe/model/shared/src/main/scala/observe/model/events/ClientEvent.scala`**:
- Rename `AtomLoaded` → `StepLoaded`, add `stepId: Step.Id`

---

### Step 9: Update `ODBSequencesLoader.scala`

**File**: `observe/server_new/src/main/scala/observe/server/ODBSequencesLoader.scala`

Update `loadSequenceEndo` and `reloadSequenceEndo` to work with `SequenceData` directly (no `SequenceGen`). Use `seqData.currentStep` and initialize `Sequence.State`. The `atomId` comes from `currentStep.map(_.atomId)`.

---

### Step 10: Update client-side event handler

**File**: `observe/web/client/src/main/scala/observe/ui/components/services/ServerEventHandler.scala`

Update handler for `AtomLoaded` → `StepLoaded`.

---

### Step 11: Update tests

**Files**:
- `observe/server_new/src/test/scala/observe/server/TestCommon.scala` — change `sequence()`, `sequenceNSteps()`, `sequenceWithResources()` to create `SequenceData` with `CurrentStep` instead of `SequenceGen`/`AtomGen`/`PendingStepGen`
- `observe/server_new/src/test/scala/observe/server/ObserveEngineSuite.scala` — update expectations for step-level events, replace `Sequence.State` usage
- `observe/server_new/src/test/scala/observe/server/engine/` — tests referencing `Sequence.Zipper` need updating (but `Sequence.State` name stays)
- Other test files referencing `AtomGen`, `nextAtom`, atom-related events

---

## Verification

```bash
# Compile everything
sbt -J-Xmx6g compile

# Run observe server tests
sbt -J-Xmx6g observe_server_new/test

# Run observe model tests (cross-compiled)
sbt -J-Xmx6g observe_model/test

# Run full test suite
sbt -J-Xmx6g test
```

## Key Risks

1. **Acquisition prompt**: Detecting atom completion by comparing `atomId` of completed step vs next step from ODB. If the ODB returns a different `atomId` after an acquisition step, the prompt fires correctly.

2. **`visitStartDone` preservation**: The `SequenceData.visitStartDone` flag must be preserved across step loads. The current `updateAtom` only modifies `seq` and `seqGen` — same approach for `updateStep`.
