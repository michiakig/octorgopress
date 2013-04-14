#!/bin/bash

emacs -Q --batch \
    --eval "(progn
(load-file \"~/.emacs.d/init.el\")
(find-file \"blorg/2013-04-06-Literate-blogging-with-Org-mode-and-Octopress.org\")
(org-babel-tangle)
(load-file \"../octorgopress.el\")
(load-file \"../tests.el\")
(with-temp-buffer
  (ert t (current-buffer))
  (prin1 (buffer-substring-no-properties (point-min) (point-max)))))"
