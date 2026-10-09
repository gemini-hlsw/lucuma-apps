# Seqexec → Observe name conversion

Observe started as a port of [seqexec](https://github.com/gemini-hlsw/seqexec). PR #1667
("Name disambiguation and unification in Observe") renamed the sequence, step and exposure
actions so that every name states its level and no verb is used at more than one level.
Code brought over from seqexec still uses the old names. Apply this table when porting.

To rename automatically, run `/seqexec-rename <files or directory>`.

## Vocabulary

Three levels, each with its own verbs. Never reuse a verb across levels.

| Level | Meaning | Verbs |
|---|---|---|
| Sequence | the loaded observation's step list | **start**, **hold** (finish the current step, then wait; was "pause" / "user stop") |
| Step | the currently loaded step | **rewind**, **interrupt** (step is being cut short by any exposure action or a rewind; was "internal stop") |
| Exposure | the exposure phase of a step | **stop**, **abort**, **pause**, **resume**, plus `…Gracefully` / `…Immediately` variants (was "observe" / "observation" / "obs") |

"Observation" stays for the ODB entity (`obsId`, `loadObservation`, `ObservationRequests`).
"Sequence" is used for executing its steps.

## Conversion table

Match is case-sensitive and whole-identifier; adjust casing for the surrounding convention
(`PascalCase` types and enum cases, `camelCase` members, string literals as shown).

### Model (`seqexec.model` → `observe.model`)

| Seqexec | Observe |
|---|---|
| `SequenceState` | `SequenceStatus` |
| `SequenceState.Running(userStop, internalStop)` | `SequenceStatus.Running(sequenceHoldRequested, stepInterruptRequested, waitingUserPrompt, waitingNextStep, starting)` |
| `userStop: Boolean` | `sequenceHoldRequested: IsSequenceHoldRequested` |
| `internalStop: Boolean` | `stepInterruptRequested: IsStepInterruptRequested` |
| `userStopRequested` | `isSequenceHoldRequested` |
| `internalStopRequested` | `isStepInterruptRequested` |
| `isStopRequested` (either flag, Observe-only) | removed; use the specific predicate |
| `OperationLevel.Observation` | `OperationLevel.Exposure` |
| `Operations.PauseObservation` | `Operations.PauseExposure` |
| `Operations.StopObservation` | `Operations.StopExposure` |
| `Operations.AbortObservation` | `Operations.AbortExposure` |
| `Operations.ResumeObservation` | `Operations.ResumeExposure` |
| `Operations.PauseGracefullyObservation` | `Operations.PauseExposureGracefully` |
| `Operations.StopGracefullyObservation` | `Operations.StopExposureGracefully` |
| `Operations.PauseImmediatelyObservation` | `Operations.PauseExposureImmediately` |
| `Operations.StopImmediatelyObservation` | `Operations.StopExposureImmediately` |
| `SeqexecEvent.SequencePaused` | `ClientEvent.SequenceHeld` |

### Engine (`seqexec.engine` → `observe.server.engine`)

| Seqexec | Observe |
|---|---|
| `Sequence.State.userStopSet(v)` | `SequenceState.setSequenceHoldRequested(IsSequenceHoldRequested(v))` |
| `Sequence.State.internalStopSet(v)` | `SequenceState.setStepInterruptRequested(IsStepInterruptRequested(v))` |
| `Sequence.State.userStopRequested` | `SequenceState.isSequenceHoldRequested` |
| `UserEvent.Pause` | `UserEvent.RequestSequenceHold` |
| `UserEvent.CancelPause` | `UserEvent.CancelSequenceHoldRequest` |
| `Event.pause(...)` | `Event.requestSequenceHold(...)` |
| `Event.cancelPause(...)` | `Event.cancelSequenceHoldRequest(...)` |
| `SystemEvent.SequencePaused` | `SystemEvent.SequenceHeld` |
| `Event.sequencePaused(...)` | `Event.sequenceHeld(...)` |
| `Engine.pause` | `Engine.requestSequenceHold` |
| `Engine.cancelPause` | `Engine.cancelSequenceHoldRequest` |
| `Engine.start` | `Engine.startSequence` |

### Server (`SeqexecEngine` → `ObserveEngine`, `SeqTranslate`, instrument controllers)

| Seqexec | Observe |
|---|---|
| `SeqexecEngine` | `ObserveEngine` |
| `start(...)` | `startSequence(...)` |
| `startFrom(...)` | `startSequenceFrom(...)` |
| `requestPause(...)` | `requestSequenceHold(...)` |
| `requestCancelPause(...)` | `cancelSequenceHoldRequest(...)` |
| `stopObserve` | `stopExposure` |
| `abortObserve` | `abortExposure` |
| `pauseObserve` | `pauseExposure` |
| `resumeObserve` | `resumeExposure` |
| `resumePaused` (instrument controllers) | `resumeExposure` |
| `PauseObserveCmd` | `PauseExposureCmd` |
| `stopObserveDelay` (sims) | `stopExposureDelay` |
| log `"Pause requested"` (sequence) | `"Sequence hold requested"` |
| log `"Continue requested"` (sequence) | `"Sequence hold cancelled"` |
| log `"Stop/Abort/Pause/Continue requested"` (exposure) | `"Exposure stop/abort/pause/resume requested"` |

### HTTP routes (`SeqexecCommandRoutes` → `ObserveCommandRoutes`)

| Seqexec | Observe |
|---|---|
| `"start"` | `"startSequence"` |
| `"startFrom"` | `"startSequenceFrom"` |
| `"pause"` | `"sequenceHold"` |
| `"cancelpause"` | `"cancelSequenceHold"` |
| `"stop"` | `"stopExposure"` |
| `"stopGracefully"` | `"stopExposureGracefully"` |
| `"abort"` | `"abortExposure"` |
| `"pauseObs"` | `"pauseExposure"` |
| `"pauseObsGracefully"` | `"pauseExposureGracefully"` |
| `"resumeObs"` | `"resumeExposure"` |

### Client (`seqexec.web.client` → `observe.ui`)

| Seqexec | Observe |
|---|---|
| `TabOperations` | `ObservationRequests` |
| `runRequested` | `startSequence` |
| `startFromRequested` | `startSequenceFrom` |
| `pauseRequested` | `sequenceHold` |
| `cancelPauseRequested` | `cancelSequenceHold` |
| `stopRequested` | `stopExposure` |
| `abortRequested` | `abortExposure` |
| `resumeRequested` | `resumeExposure` |
| (none; exposure pause shared `pauseRequested`) | `pauseExposure` |
| `resourceRunRequested` | `subsystemRun` |
| `RequestRun` | `SequenceApi.startSequence` |
| `RequestRunFrom` | `SequenceApi.startSequenceFrom` |
| `RequestPause` | `SequenceApi.requestSequenceHold` |
| `RequestCancelPause` | `SequenceApi.cancelSequenceHoldRequest` |
| `RequestStop` / `RequestGracefulStop` | `SequenceApi.stopExposure` / `stopExposureGracefully` |
| `RequestAbort` | `SequenceApi.abortExposure` |
| `RequestObsPause` / `RequestGracefulObsPause` | `SequenceApi.pauseExposure` / `pauseExposureGracefully` |
| `RequestObsResume` | `SequenceApi.resumeExposure` |
| tooltip "Pause the sequence after the current step completes" | "Hold sequence after current step" |
| tooltip "Cancel process to pause the sequence" | "Cancel sequence hold" |
| button text "Pause" (sequence toolbar) | "Hold" |
| CSS `pauseButton` / `cancelPauseButton` | `observe-sequence-hold-button` / `observe-cancel-sequence-hold-button` |

## Not renamed

- `obsPause` and `stepStop` ODB mutations: ODB vocabulary.
- `Audio.SequencePaused` and its clips: that is what the recording says.
- `Result.Paused`, `SystemEvent.Paused`, `actionResume`: engine action-level pause, unrelated
  to both sequence hold and exposure pause.
- `NsCycle` / `NsNod` operation levels.
