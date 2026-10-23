- Place these files on `~/observe` in each server.
- Set the `SITE`, `DOCKERHUB_TOKEN` and `LOCAL_LOG_DIR` variables in `config.sh`.

The deployed version is not part of `config.sh`. The only way to change it is `observe update`,
which deploys the tag last promoted to the server's environment (`staging` or `production`, taken
from `SITE`). It reads that tag from the GitHub deployment records that the promote script writes,
so the server needs access to api.github.com. `observe update --force-version <tag>` deploys another
tag instead, after a warning and a confirmation prompt: anything other than the promoted version may
be incompatible with the other systems of the environment. Tags are `YYYYMMDD-<sha8>` as published
by CI (e.g. `20260916-4b3faa6d`). The current tag is stored in `~/observe/deployed-version` (written
only by `update`) and shown by `observe version`. `observe start` and `observe restart` reuse it.

The per-subsystem control modes (`observe-engine.subsystem-control-modes`) live in
`~/observe/conf/subsystems.conf`, which is now written by the Observe UI (Subsystems dialog) for the
subsystems of the running site. The UI can also restart the server for the new modes to take
effect, when nothing is running: the server exits cleanly and the container's
`--restart unless-stopped` policy brings it back (the server exits with code 3 on such a restart).

You can use the commands in `install.sh` to copy the latest version of the scripts:

```
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe/deploy/scripts/install.sh >install.sh
chmod +x install.sh
./install.sh
```
