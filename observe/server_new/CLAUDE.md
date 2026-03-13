# CLAUDE.md — observe/server_new

This module is the **observation execution engine** for the Gemini Observatory. It is a library (no main entry point) consumed by `observe/web/server`, which provides HTTP routes and application bootstrap.

## Build & Test

```bash
# Compile
sbt -J-Xmx6g observe_server_new/compile

# Run all tests
sbt -J-Xmx6g observe_server_new/test

# Run a specific test suite
sbt -J-Xmx6g "observe_server_new/testOnly *ObserveEngineSuite*"

# Run a single test
sbt -J-Xmx6g "observe_server_new/testOnly *ObserveEngineSuite* -- *testname*"
```

## Engine Architecture

### Core Primitives (`engine/` package)

- **`Handle[F, S, E, O]`** (`Handle.scala`) — Opaque type for `StateT[F, S, (O, Stream[F, E])]`. A state transition that produces both a result and a stream of output events. Implements `MonadCancelThrow`.
- **`Engine[F]`** (`Engine.scala`) — Main event loop. Processes `Event[F]` via `handleUserEvent`/`handleSystemEvent`, using `Handle` to transition `EngineState[F]` and emit event streams.
- **`Event[F]`** (`Event.scala`) — Sealed trait: `EventUser` (Start, Pause, Breakpoints, Poll, etc.) and `EventSystem` (Completed, Failed, Paused, StepComplete, etc.).
- **`EngineHandle[F, A]`** — Type alias: `Handle[F, EngineState[F], Event[F], A]`.

The event loop runs as an `fs2.Stream` pipeline: events arrive via input/stream queues, each processed by `run`, state threaded via `evalMapAccumulate`, output events fed back into the stream queue.

### Global State

**`EngineState[F]`** (`EngineState.scala`):

```
queues:     ExecutionQueues          — Map[QueueId, ExecutionQueue]
selected:   Selected[F]             — Currently loaded sequence per instrument
conditions: Conditions               — Observing conditions
operator:   Option[Operator]
```

**`SequenceData[F]`** wraps a `SequenceGen[F]` (ODB-derived definition) and a `Sequence.State[F]` (runtime execution state).

State is navigated via Monocle optics: `EngineState.atSequence(obsId)`, `EngineState.instrumentLoaded(instrument)`, etc.

## Execution Model (Zipper-based)

Nested state tracked with focus/pending/done zippers:

```
Sequence[F]           — observation id + list of steps + breakpoints
  └─ EngineStep[F]    — step id + list of parallel action groups
      └─ Execution[F] — currently executing parallel actions
          └─ Action[F] — effectful computation (gen: Stream[F, Result]) + state + kind
```

`Sequence.Zipper[F]` and `EngineStep.Zipper[F]` track progress. `Sequence.State[F]` is either `Zipper` (active) or `Final` (complete).

`Result` is a sealed trait: `OK`, `OKStopped`, `OKAborted`, `Partial`, `Paused`, `Error`.

### Atom-Based Flow

1. An atom is loaded from ODB (via `onAtomComplete` or `onAtomReload`)
2. Steps within the atom execute sequentially
3. After the last step, the engine requests the next atom from ODB
4. ODB edit subscriptions can trigger mid-sequence atom reloads

## Step Generation Pipeline

**`SeqTranslate[F]`** translates ODB observation data into `SequenceGen[F]` instances with concrete actions.

**`StepActionsGen[F]`** defines the action pipeline for each step:

```
preStep → preConfig → [parallel config actions] → postConfig → preObserve → [observe actions] → postObserve → postStep
```

**`SequenceGen[F]`** has instrument-specific subclasses: `GmosNorth`, `GmosSouth`, `Flamingos2`. Each contains `AtomGen[F]` with `StepGen[F, D]` instances.

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
- **`TestCommon`** — simulated `Systems.dummy[IO]`, engine builders, `advanceN(n)` helper for stepping through events, fixtures for `SequenceGen`/`SequenceData`
- **Mocks**: `TestOdbProxy`, `TestTcsEpics`, `TestEpicsCommand`
- **Coverage**: engine core, sequence translation, TCS config encoding, GMOS, GCAL, keywords, execution queues, settle time calculations
