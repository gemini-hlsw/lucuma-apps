# CLAUDE.md — observe/server

This module is the **observation execution engine** for the Gemini Observatory. It is a library (no main entry point) consumed by `observe/web/server`, which provides HTTP routes and application bootstrap.

## Build & Test

```bash
# Compile
sbt -J-Xmx6g observe_server/compile

# Run all tests
sbt -J-Xmx6g observe_server/test

# Run a specific test suite
sbt -J-Xmx6g "observe_server/testOnly *ObserveEngineSuite*"

# Run a single test
sbt -J-Xmx6g "observe_server/testOnly *ObserveEngineSuite* -- *testname*"
```

## Engine Architecture

### Core Primitives (`engine/` package)

- **`Handle[F, S, E, O]`** (`Handle.scala`) — Opaque type for `StateT[F, S, (O, Stream[F, E])]`. A state transition that produces both a result and a stream of output events.
- **`Engine[F]`** (`Engine.scala`) — Main event loop. Processes `Event[F]` via `handleUserEvent`/`handleSystemEvent`, using `Handle` to transition `EngineState[F]` and emit event streams. Built via `Engine.build[F](loadNextStep)`.
- **`Event[F]`** (`Event.scala`) — Sealed trait: `EventUser` (Pause, CancelPause, Breakpoints, Poll, GetState, ModifyState, ActionStop, ActionResume, Log\*, Pure) and `EventSystem` (Completed, Failed, Paused, StepComplete, SequenceComplete, Executing, Executed, etc.).
- **`EngineHandle[F, A]`** — Type alias: `Handle[F, EngineState[F], Event[F], A]`. `EngineHandle` companion object provides convenience constructors and sequence state accessors (`getSequenceState`, `modifySequenceState`, `replaceSequenceState`).

The event loop runs as an `fs2.Stream` pipeline: events arrive via input/stream queues, each processed by `run`, state threaded via `evalMapAccumulate`, output events fed back into the stream queue.

### Global State

**`EngineState[F]`** (`EngineState.scala`):

```
queues:     ExecutionQueues          — Map[QueueId, ExecutionQueue]
selected:   Selected[F]             — Currently loaded sequence per instrument
conditions: Conditions               — Observing conditions
operator:   Option[Operator]
```

**`Selected[F]`** holds one `Option[SequenceData[F]]` per active instrument (gmosNorth, gmosSouth, flamingos2).

**`SequenceData[F]`** is a sealed trait with instrument-specific subclasses (`GmosNorth`, `GmosSouth`, `Flamingos2`). Each holds observer, overrides, target environment, constraint set, static config, `SequenceState[F]`, and pending observe command.

State is navigated via Monocle optics: `EngineState.atSequence(obsId)`, `EngineState.sequenceStateAt(obsId)`, `EngineState.instrumentLoaded(instrument)`, etc.

## Step-Based Execution Model

The engine uses a **single-step-at-a-time** execution model. Only one step is loaded into memory at any time — there is no full-sequence zipper.

### State Hierarchy

```
EngineState[F]
  └─ SequenceData[F]              — per-instrument observation data + static config
      └─ SequenceState[F]         — runtime execution state for one observation
          ├─ status: SequenceStatus
          ├─ currentSequenceType: SequenceType
          ├─ breakpoints: Breakpoints
          ├─ singleRuns: Map[ActionCoordsInSeq, ActionState]
          └─ loadedStep: Option[LoadedStep[F]]   ← None = idle/done, Some = executing
              ├─ stepGen: StepGen[F]             — ODB-derived step definition
              └─ executionZipper: ExecutionZipper[F]   — runtime progress within the step
                  ├─ pending: List[ParallelActions[F]]
                  ├─ focus: Execution[F]         — currently executing parallel actions
                  ├─ done: List[ParallelActions[F]]
                  └─ rolledback: (Execution[F], List[ParallelActions[F]])
```

**`LoadedStep[F]`** (`LoadedStep.scala`) bundles a `StepGen[F]` (the ODB step definition) with an `ExecutionZipper[F]` (runtime progress tracking). It exports fields from both: `id`, `atomId`, `generator`, `obsControl`, `resources`, `done`, `focus`, `pending`.

**`ExecutionZipper[F]`** (`ExecutionZipper.scala`) is a zipper over execution groups within a single step. It tracks `pending`, `focus` (current), `done`, and `rolledback` (for restart). `withNextExecution` advances to the next execution group; returns `None` when the step is complete.

**`Execution[F]`** (`Execution.scala`) — a list of `Action[F]` executing in parallel.

**`Action[F]`** (`Action.scala`) — individual effectful computation with `gen: Stream[F, Result]`, `kind: ActionType`, and `state: Action.State`.

**`Result`** is a sealed trait: `OK`, `OKStopped`, `OKAborted`, `Partial`, `Paused`, `Error`.

### Execution Flow

1. A step is loaded into `SequenceState.loadedStep` via `withLoadedStepGen`
2. `Engine.executeLoadedStep` runs all actions in the current execution group in parallel via `parJoin`
3. Action results map to `SystemEvent`s (Completed, Failed, Paused, etc.)
4. When all actions in an execution group complete, `nextExecution` advances the `ExecutionZipper`
5. When the zipper is exhausted (step complete), the engine calls `loadNextStep` to fetch the next step from ODB
6. If no more steps, `SequenceComplete` is emitted

### Step Definition

**`StepGen[F]`** (`StepGen.scala`) — sealed trait with instrument-specific subclasses (`GmosNorth`, `GmosSouth`, `Flamingos2`). Contains atom ID, step ID, data ID, resources, instrument config, telescope config, breakpoint, and a `StepActionsGen[F]` for generating the action pipeline.

**`StepActionsGen[F]`** defines the action pipeline for each step:

```
preStep → preConfig → [parallel config actions] → postConfig → preObserve → [observe actions] → postObserve → postStep
```

## Controller Pattern

Every subsystem follows a consistent 4-file pattern:

| File                        | Purpose                                |
| --------------------------- | -------------------------------------- |
| `*Controller.scala`         | Trait defining the subsystem API       |
| `*ControllerEpics.scala`    | Real implementation via EPICS channels |
| `*ControllerSim.scala`      | Simulated implementation for testing   |
| `*ControllerDisabled.scala` | No-op implementation                   |
| `*Epics.scala`              | Raw EPICS channel access wrapper       |

**Active instruments**: GMOS North/South, Flamingos-2
**WIP instruments**: GNIRS, GSAOI, GPI, Ghost, NIRI, NIFS (commented out)
**Subsystems**: TCS (North/South), GCAL, Altair, GeMS, GWS

### Wiring

**`Systems[F]`** (`Systems.scala`) aggregates all controllers, keyword readers, ODB proxy, and DHS client. Built via `Systems.build(site, httpClient)` which selects real/sim/disabled implementations based on `ControlStrategy`.

**`ObserveEngine[F]`** (`ObserveEngine.scala`) is the top-level facade, built via `ObserveEngine.build(site, systems, conf, environment)`.

## Integration Points

| System    | Interface                                                 | Protocol                                         |
| --------- | --------------------------------------------------------- | ------------------------------------------------ |
| **EPICS** | `EpicsCommand[F]`, per-subsystem `*Epics[F]`              | EPICS channels via `CaService`/`CaCommandSender` |
| **ODB**   | `OdbProxy[F]` (read), `OdbCommands[F]` (lifecycle events) | GraphQL queries/mutations/subscriptions          |
| **DHS**   | `DhsClient[F]`                                            | HTTP (image creation, FITS keywords)             |
| **GDS**   | `GdsClient[F]`                                            | HTTP (keyword management for GPI/Ghost)          |

Java enums in `src/main/java/` define EPICS channel value types (`BinaryOnOff`, `DetectorState`, `ParkState`, etc.).

GraphQL definitions live in `src/clue/scala/observe/common/` — uses `@GraphQL` annotation with `sbt-clue` codegen.

## Domain Events

**`SeqEvent`** (`SeqEvent.scala`) — all domain-level events: `LoadSequence`, `SetConditions`, `SequenceStart`, `NewAtomLoaded`, `AtomCompleted`, `NoMoreAtoms`, etc.

## Test Infrastructure

- **Framework**: MUnit (`CatsEffectSuite`, `DisciplineSuite`) + ScalaCheck
- **`TestCommon`** — simulated `Systems.dummy[IO]`, engine builders, `advanceN(n)` helper for stepping through events, fixtures for `SequenceData`
- **Mocks**: `TestOdbProxy`, `TestTcsEpics`, `TestEpicsCommand`
- **Key test suites**:
  - `EngineSpec` — core engine event processing
  - `StepSuite` — `ExecutionZipper` advancement and step completion
  - `SequenceSuite` — `SequenceState` transitions
  - `PackageSuite`, `ExecutionSpec`, `HandleSpec` — primitives
  - `ObserveEngineSuite` — end-to-end sequence execution
  - `SeqTranslateSuite`, `GmosSpec`, `GcalSuite` — translation and instruments
  - `TcsControllerSuite`, `TcsNorth/SouthSuite` — telescope control
