 #!/bin/bash

# tangles octorgopress.el
./runTests.sh

if [ -d publish ]; then
    rm -rf publish
fi
if [ ! -d /Users/aki/source/octopress ]; then
    echo "missing octopress source, bailing"
    exit 1
fi
git clone /Users/aki/source/octopress publish
pushd publish
git checkout 7dfba9a26e21b970f74aa663a86d407ae8fd5958
rake install
cp -r ../customizations/ .
cp ../posterous/* ./source/_posts

emacs -Q --batch \
    --eval "(progn
(load-file \"~/.emacs.d/init.el\")
(load-file \"../octorgopress.el\")
(setq org-publish-project-alist
      (make-org-publish-project-alist
       \"blorg\"
       \"../blorg\"
       \".\"))
(org-publish-project \"blorg\" t))"

rake generate
popd
