#! /bin/bash

# WARNING: This will apply CURRENT LOCAL migrations
 unset NODE_OPTIONS
 hasura migrate apply --endpoint https://prefs-dev.gpp.gemini.edu --database-name default
 hasura metadata apply --endpoint https://prefs-dev.gpp.gemini.edu
 hasura metadata reload --endpoint https://prefs-dev.gpp.gemini.edu
