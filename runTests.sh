#!/bin/bash

emacs --batch \
    --eval "(progn
(require 'package)
(add-to-list 'package-archives
             '(\"marmalade\" . \"http://marmalade-repo.org/packages/\") t)
(package-initialize)
(find-file \"literate.org\")
(org-babel-tangle)
(load-file \"octorgopress.el\")
(load-file \"tests.el\")
(with-temp-buffer
  (ert t (current-buffer))
  (prin1 (buffer-substring-no-properties (point-min) (point-max)))))"
