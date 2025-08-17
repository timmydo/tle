#!/bin/sh
# Start TLE (Timmy's Lisp Environment)
exec sbcl --non-interactive --eval '(ql:quickload :tle)' --eval '(tle:main)'
