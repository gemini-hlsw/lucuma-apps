#!/bin/bash

pnpm install --frozen-lockfile --filter lucuma-schemas

./fetchODBSchema.mjs $@
