#!/bin/bash

if [[ -z $SCHEME_EXEC ]]; then
  SCHEME_EXEC='chez --libdirs . --script '
fi

$SCHEME_EXEC runner.scm
