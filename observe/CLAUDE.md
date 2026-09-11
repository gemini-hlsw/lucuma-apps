# Observe

## Running Observe Locally

```bash
# Backend
sbt observe_web_server/reStart

# Frontend (separate terminal)
sbt '~observe_web_client/fastLinkJS'
cd observe/web/client && pnpm exec vite
```
