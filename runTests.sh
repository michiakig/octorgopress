#!/bin/bash

emacs -Q --batch \
    --eval "(progn
(load-file \"~/.emacs.d/init.el\")
(find-file \"literate.org\")
(org-babel-tangle)
(load-file \"octorgopress.el\")
(load-file \"tests.el\")
(with-temp-buffer
  (ert t (current-buffer))
  (prin1 (buffer-substring-no-properties (point-min) (point-max)))))"
