# CLAUDE.md — observe/server

This module is the **observation execution engine** for the Gemini Observatory. It is a library (no main entry point) consumed by `observe/web/server`, which provides HTTP routes and application bootstrap.

Build and test commands are the standard ones from the root CLAUDE.md with the `observe_server` project.

## Step-Based Execution Model

The engine uses a **single-step-at-a-time** execution model. Only one step is loaded into memory at any time — there is no full-sequence zipper.

### Execution Flow

1. A step is loaded into `SequenceState.loadedStep` via `withLoadedStepGen`
2. `Engine.executeLoadedStep` runs all actions in the current execution group in parallel via `parJoin`
3. Action results map to `SystemEvent`s (Completed, Failed, Paused, etc.)
4. When all actions in an execution group complete, `nextExecution` advances the `ExecutionZipper`
5. When the zipper is exhausted (step complete), the engine calls `loadNextStep` to fetch the next step from ODB
6. If no more steps, `SequenceComplete` is emitted

### Step Definition

**`StepActionsGen[F]`** defines the action pipeline for each step:

```
preStep → preConfig → [parallel config actions] → postConfig → preObserve → [observe actions] → postObserve → postStep
```

### Wiring

**`Systems[F]`** (`Systems.scala`) aggregates all controllers, keyword readers, ODB proxy, and DHS client. Built via `Systems.build(site, httpClient)` which selects real/sim/disabled implementations based on `ControlStrategy`.

**`ObserveEngine[F]`** (`ObserveEngine.scala`) is the top-level facade, built via `ObserveEngine.build(site, systems, conf, environment)`.
