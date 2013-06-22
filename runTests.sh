#!/bin/bash

emacs --batch \
    --eval "(progn
(add-to-list 'load-path \"/Users/aki/source/org-mode/lisp/\")
(find-file \"literate.org\")
(org-babel-tangle)
(load-file \"octorgopress.el\")
(load-file \"tests.el\")
(with-temp-buffer
  (ert t (current-buffer))
  (prin1 (buffer-substring-no-properties (point-min) (point-max)))))"
