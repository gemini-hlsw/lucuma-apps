#!/bin/bash

gq https://user-prefs-master.herokuapp.com/v1/graphql --introspect > app/src/clue/resources/UserPreferencesDB.graphql
