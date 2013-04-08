 #!/bin/bash

if [ ! -d publish ]; then
    git clone /Users/aki/source/octopress publish
fi
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
