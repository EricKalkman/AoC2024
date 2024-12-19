#!/bin/bash

if [[ -z $SCHEME_EXEC ]]; then
  SCHEME_EXEC='scheme --libdirs . --script '
fi

$SCHEME_EXEC runner.scm
