# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Lucuma Apps is a monorepo for the Gemini Observatory's astronomical observation software. It contains three main applications:

- **Explore** — Observation planning web app (Scala.js SPA)
- **Observe** — Observation execution system (Scala.js frontend + http4s backend)
- **Navigate** — Telescope navigation system (JVM server with EPICS integration)

Plus two shared libraries:

- **schemas** — Cross-compiled (JVM + JS) GraphQL schema definitions and domain model types
- **ui** — Shared UI component library (Scala.js only)

## Build System

Scala 3.9.0 on sbt 2.0.8. Frontend bundled with Vite. JS dependencies managed with pnpm. Java 17
or later (the build targets 25).

sbt 2 takes **one `;`-separated argument**, not several words. `sbt clean compile test` is a
syntax error; `sbt "clean; compile; testFull"` is the equivalent. Folding `++` into the same
string matters too: `sbt '++ 3' foo --bar` passes the aggregated project keys to `foo`.

Two renamed keys catch people out:

- `test` is now incremental (the old `testQuick`) and its success is cached by content hash,
  surviving `clean`. **`testFull` is the old always-run-everything `test`.**
- `scalafixAll` is gone. Run `scalafix` and `Test/scalafix` separately.

### Essential Commands

```bash
# Compile everything
sbt compile

# Compile only JVM or JS subprojects
sbt "rootJVM/Test/compile"
sbt "rootJS/Test/compile"

# Run all tests
sbt "rootJVM/testFull"
sbt "rootJS/testFull"

# Run tests for a specific subproject
sbt explore_model/testFull

# Run a specific test suite
sbt "testOnly *MySuite*"

# Run a single test within a suite (MUnit filter)
sbt "testOnly *MySuite* -- *testname*"

# Format code
sbt "scalafmtAll; scalafmtSbt"

# Run scalafix
sbt "scalafix; Test/scalafix"
```

Heap size comes from `.jvmopts` (gitignored), not from `-J-Xmx...` on the command line: sbt 2
keeps a background server, and a later invocation hands its command to a server that already
booted. For the same reason a forked process inherits the _server's_ environment, not your
shell — after changing anything the app reads from the environment, `shutdown` the server.

**In a git worktree, pass `--server`.** The `sbt` wrapper switches to the thin client for sbt 2
builds, and that client attaches to whichever server is already running — usually the main
checkout's. Symptom: paths in the log point at another directory. `project/target/active.json`
names the socket in use.

### Running locally

See `explore/CLAUDE.md` and `observe/CLAUDE.md` for the dev-server commands of each app.

### CI Lint Checks

```bash
# Headers, formatting and scalafix. CI runs these for both matrix legs,
# so check with rootJS as well as rootJVM.
sbt "project rootJVM; ++ 3.9.0; headerCheckAll; scalafmtCheckAll; project /; \
  scalafmtSbtCheck; lucumaScalafmtCheck; lucumaScalafixCheck; lucumaSlackNotifyCheck"
sbt "project rootJVM; ++ 3.9.0; scalafix --check; Test/scalafix --check"

# CSS linting
sbt "++ 3.9.0; ui_css/lucumaCss"
pnpm exec stylelint explore/common/src/main/webapp/sass
pnpm exec stylelint observe/web/client/src/main/webapp/styles
pnpm exec stylelint ui/lib/src/main/resources/lucuma-css

# Prettier
pnpm exec prettier --check .
```

Scalafmt and Scalafix are enforced in CI (see `.github/workflows/ci.yml`) for both
the `rootJS` and `rootJVM` matrix legs. `LucumaWorkflowSyntaxPlugin` rewrites every `sbt` line
in the generated workflow, so run `sbt githubWorkflowGenerate` and commit the result rather
than hand-editing it.

### Build Layout

sbt 2 puts every module's output under `target/out/<platform>/scala-<ver>/<artifact>/` at the
repo root, keyed by **artifact name**, not module directory. `lucumaCss` writes there too.

Scala.js **linker** output is deliberately kept inside each project (see
`jsLinkerOutputInProject` in `build.sbt`): Node and Vite resolve npm imports against the
location of the linked file, and pnpm does not hoist, so output at the repo root cannot find
the package's own `node_modules`.

Two more sbt 2 traps this build works around:

- **Bare settings apply to every subproject.** A top-level `foo := ...` is a _common_ setting
  now, so root-only tasks are scoped (`LocalRootProject / lintCheck := ...`).
- **Every task is cached to disk.** On a hit sbt returns the cached value without running the
  body, so anything with side effects needs `Def.uncached { ... }`.

## Architecture and Patterns

### GraphQL Code Generation (Clue)

The `sbt-clue` plugin generates Scala code from GraphQL:

- Schemas: `schemas/lib/src/clue/resources/lucuma/schemas/*.graphql`
- Query definitions: `src/clue/scala/` directories (e.g., `explore/app/src/clue/scala/queries/`)
- Queries use `@GraphQL` annotation on traits extending `GraphQLOperation[Schema]`
- The `document` field contains raw GraphQL, often composing fragments via string interpolation
- Special `// gql:` comments inject imports into generated code

### State Management (Crystal Views)

The `crystal` library provides reactive state via `View[A]`:

- `View[A]` is an optic-aware reactive state holder that propagates changes to React components
- `view.zoom(lens)` creates a narrower View focused on a subfield
- `view.mod(f)` applies a pure modification, `view.set(a)` replaces the value
- `.async` converts between sync/async effect contexts
- `Pot[A]` represents async data states: `Pending`, `Ready(a)`, `Error(t)`

### Monocle Optics (Pervasive)

Every case class companion object defines lenses using `Focus[CaseClass](_.fieldName)`. Lenses are composed with `andThen` and used with `View.zoom` for state management. `GenPrism` handles sealed trait hierarchies.

### AlignerF — Synchronized Undo + Remote Updates

`AlignerF` (in `explore/common`) synchronizes local model state, GraphQL mutation input deltas, and an undo stack. When you `zoom` into an AlignerF, it drills into both the model and the delta structure in parallel, so model edits automatically generate corresponding API mutation inputs.

### UndoContext / UndoSetter

Full undo/redo system built on `View` and `UndoStacks`. `UndoSetter` provides a View-like interface that automatically records undo history via `Restorer` values.

### Reusability (React Performance)

`Reusability` typeclass instances are required for React components. Centrally defined in `explore/common/src/main/scala/explore/model/reusability.scala`. Usually `Reusability.byEq` for types with `Eq`.

### Cross-Compilation

Multiple modules use `crossProject(JVMPlatform, JSPlatform)` to share code between browser and server. Domain types and decoders in `schemas/model` and `schemas/lib` are cross-compiled.

### Dependency Injection

React `Context` provides services to components. Both Explore and Observe define an `AppContext` with service dependencies.

## Coding Conventions

- **Scala 3 syntax**: Significant indentation (braceless), `given`/`using`, `derives`, `extension`, `enum`
- **Typeclass derivation**: `derives Eq, Show, Decoder` on case classes and enums
- **Effect system**: Tagless final with `F[_]` type parameter, cats-effect `IO` as runtime
- **Imports**: Scala 3 wildcard `*`, `cats.syntax.all.*` is standard
- **Copyright header**: Every file starts with `// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)`
- **Scalafmt**: maxColumn = 100, align.preset = most, Scala 3 dialect
- **No mutable state**: All state via `View`, `Ref`, or `IO`
- **Components**: `ScalaFnComponent` (functional React components only). React is used via scalajs-react and `lucuma.react.common.*`, with monadic hooks.
- **Comments**: Don't put comments directly on method or class parameters, those belong to the method or class documentation or scaladoc.
- **UI library**: PrimeReact via lucuma-react bindings
