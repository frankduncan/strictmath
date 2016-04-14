#!/bin/bash

sbcl \
  --eval "(asdf:load-system :docgen)" \
  --eval "(asdf:load-system :strictmath)" \
  --eval "(format t \"----~%\")" \
  --eval "(format t \"~A\" (docgen:export-package :strictmath))" \
  --eval "(quit)" 2> /dev/null | sed -n '/^----$/,$p' | tail -n +2 > wiki/Home.md
