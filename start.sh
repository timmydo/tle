#!/bin/sh
# Start TLE (Timmy's Lisp Environment)
export CL_SOURCE_REGISTRY="$(pwd)/vendor//:"
exec sbcl --non-interactive --eval '(asdf:load-system :tle)' --eval '(tle:main)'
