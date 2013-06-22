 #!/bin/bash

# tangles octorgopress.el
./runTests.sh

OCTOPRESS=/Users/aki/source/octopress

if [ -d publish ]; then
    rm -rf publish
fi
if [ ! -d "$OCTOPRESS" ]; then
    echo ""
    echo "!!! Set env var OCTOPRESS to path to Octopress source."
    echo "!!! Bailing..."
    exit 1
fi
git clone $OCTOPRESS publish
pushd publish
git checkout 7dfba9a26e21b970f74aa663a86d407ae8fd5958
rake install
cp -r ../customizations/ .
cp ../posterous/* ./source/_posts

emacs --batch \
    --eval "(progn
(add-to-list 'load-path \"/Users/aki/source/org-mode/lisp/\")
(load-file \"../octorgopress.el\")
(setq org-publish-project-alist
      (make-org-publish-project-alist
       \"blorg\"
       \"../blorg\"
       \".\"))
(org-publish-project \"blorg\" t))"

rake generate
popd
