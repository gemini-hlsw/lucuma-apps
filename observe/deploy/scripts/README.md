- Place these files on `~/observe` in each server.
- Set the `SITE`, `DOCKERHUB_TOKEN` and `LOCAL_LOG_DIR` variables in `config.sh`.

The deployed version is not part of `config.sh`. The only way to change it is
`observe update <tag>`; the tag is mandatory and there is no `latest` fallback. Tags are
`YYYYMMDD-<sha8>` as published by CI (e.g. `20260916-4b3faa6d`); the promote script prints the
exact command to run. The current tag is stored in `~/observe/deployed-version` (written only by
`update`) and shown by `observe version`. `observe start` and `observe restart` reuse it.

You can use the commands in `install.sh` to copy the latest version of the scripts:

```
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe/deploy/scripts/install.sh >install.sh
chmod +x install.sh
./install.sh
```
