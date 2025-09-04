#!/bin/sh
# Start TLE (Timmy's Lisp Environment)
export CL_SOURCE_REGISTRY="$(pwd)//:"
exec sbcl --no-userinit --non-interactive --eval '(sb-int:set-floating-point-modes :traps nil)' --eval "(require \"asdf\")" --eval '(asdf:load-system :tle)' --eval '(tle:main)'
