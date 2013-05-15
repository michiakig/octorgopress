Org mode backend for Octopress. See `literate.org` for detailed explanation

Right now requires an older, development version of Org-mode v8 (NOT
the release version).

# usage

1. Clone Org-mode source from: http://orgmode.org/worg/org-faq.html#keeping-current-with-Org-mode-development
2. Checkout the following commit: 6caddbca052399f3f46c94e49c983b2ebceebc81
3. Clone Octopress
4. Set env var $OCTOPRESS to path to Octopress
5. Install Org-mode following: http://orgmode.org/manual/Installation.html#Installation. Specifically be sure to run `make autoloads`, and add Org-mode to your load-path
6. Clone this repo
7. Run the script `runTests.sh`, which builds `octorgopress.el` and runs the tests
8. Load `octorgopress.el`
9. (require 'ox) in your `.emacs` or in a scratch buffer

Finally ready to start a new post:

1. From the root of this repo, in Emacs, call `new-post` interactively, providing `blorg` for the directory, and the name of your post
2. You should now be able to insert the Org mode export options template as usual, and write your blog post.
3. To build your blog, run `build.sh` which will generate it in `publish`
