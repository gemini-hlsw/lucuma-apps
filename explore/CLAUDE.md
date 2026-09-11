# Explore

## Running Explore Locally

```bash
# Terminal 1: continuous Scala.js compilation
sbt -J-Xmx6g '~explore_app/fastLinkJS'

# Terminal 2: install deps + start Vite dev server
pnpm install --frozen-lockfile --filter explore --prefer-offline
cd explore && pnpm exec vite
# Serves at https://local.lucuma.xyz:8080
```

## The observation subquery

`ObservationSubquery` is the single observation subquery, used by the bulk summary
query (first paint), the `observationEdit` subscription, and the create/clone
mutations. It selects only the lightweight `BasicConfiguration` for `observingMode`;
the full `ObservingMode` is hydrated separately via `ObservingModeByTypeSubquery`
(see `hydrateObservingMode` in `OdbObservationApiImpl`).
