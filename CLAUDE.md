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

Scala 3.8.2 on sbt 1.12.5. Frontend bundled with Vite. JS dependencies managed with pnpm 10.30.3. Java 17 (temurin).

### Essential Commands

```bash
# Compile everything (needs extra memory)
sbt -J-Xmx6g compile

# Compile only JVM or JS subprojects
sbt -J-Xmx6g 'project rootJVM' compile
sbt -J-Xmx6g 'project rootJS' compile

# Run all tests
sbt -J-Xmx6g test

# Run tests for a specific subproject
sbt -J-Xmx6g explore_model/test

# Run a specific test suite
sbt -J-Xmx6g "testOnly *MySuite*"

# Run a single test within a suite (MUnit filter)
sbt -J-Xmx6g "testOnly *MySuite* -- *testname*"

# For JS tests, link first then test
sbt -J-Xmx6g 'project rootJS' Test/scalaJSLinkerResult
sbt -J-Xmx6g 'project rootJS' test

# Format code
sbt scalafmtAll

# Run scalafix
sbt scalafixAll
```

### Running Explore Locally

```bash
# Terminal 1: continuous Scala.js compilation
sbt -J-Xmx6g '~explore_app/fastLinkJS'

# Terminal 2: install deps + start Vite dev server
pnpm install --frozen-lockfile --filter explore --prefer-offline
cd explore && pnpm exec vite
# Serves at https://local.lucuma.xyz:8080
```

### Running Observe Locally

```bash
# Backend
sbt observe_web_server/reStart

# Frontend (separate terminal)
sbt '~observe_web_client/fastLinkJS'
cd observe/web/client && pnpm exec vite
```

### CI Lint Checks

```bash
# Header check
sbt -J-Xmx6g 'project rootJVM' '++ 3' headerCheckAll

# CSS linting
sbt -J-Xmx6g '++ 3.8.2' ui_css/lucumaCss
pnpm exec stylelint explore/common/src/main/webapp/sass
pnpm exec stylelint observe/web/client/src/main/webapp/styles
pnpm exec stylelint ui/lib/src/main/resources/lucuma-css

# Prettier
pnpm exec prettier --check .
```

Note: Scalafmt and Scalafix CI checks are currently disabled in the build.

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
- **UI library**: PrimeReact via lucuma-react bindings
