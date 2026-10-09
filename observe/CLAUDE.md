# Observe

## Running Observe Locally

```bash
# Backend
sbt observe_web_server/reStart

# Frontend (separate terminal)
sbt '~observe_web_client/fastLinkJS'
cd observe/web/client && pnpm exec vite
```

## Porting code from seqexec

Observe's sequence/step/exposure vocabulary differs from seqexec's. Before adapting code
brought over from the seqexec repo, apply `observe/docs/seqexec-name-conversion.md`, or run
`/seqexec-rename <files>`.
