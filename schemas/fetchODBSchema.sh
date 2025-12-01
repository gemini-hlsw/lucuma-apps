#!/bin/bash

pnpm install --frozen-lockfile --filter lucuma-schemas --prefer-offline

./fetchODBSchema.mjs $@
