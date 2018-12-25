#!/bin/sh

which yesod
if test $? -ne 0; then
    stack build yesod-bin
fi

stack clean
stack exec -- yesod devel
