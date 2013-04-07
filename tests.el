
;; Some helpers:

(defun to-octopress (s)
  "Given a string, in Org syntax, convert to Octopress Markdown and
return"
  (with-temp-buffer
    (insert s)
    (org-export-as 'octopress)))

(defmacro as-octopress (&rest body)
  "Execute body in Org-mode buffer, then export as Octopress and
return string"
  `(with-temp-buffer
     ,@body
     (org-export-as 'octopress)))

(defun eq/trail-newlines (x y)
  "Returns t if two strings are equal modulo trailing newlines"
  (let ((xx (replace-regexp-in-string "\n+$" "" x))
        (yy (replace-regexp-in-string "\n+$" "" y)))
    (string= xx yy)))

;; Test cases themselves:

(ert-deftest octopress-headline ()
  "Test exporting Org headlines as Markdown"
  (let ((*org-octopress-yaml-front-matter* nil))
    (should (eq/trail-newlines (to-octopress "* Headline 1") "# Headline 1"))
    (should (eq/trail-newlines (to-octopress "* Headline 1\n\n** Headline 2")
                               "# Headline 1\n\n## Headline 2"))))

(ert-deftest octopress-link ()
  "Test exporting Org links as Markdown"
  (let ((*org-octopress-yaml-front-matter* nil))
    (should (eq/trail-newlines
             (as-octopress
              (org-insert-link nil "http://www.example.org" "Example"))
             "[Example](http://www.example.org)"))))

(ert-deftest octopress-emphasize ()
  "Test exporting Org bold and italic"
  (let ((*org-octopress-yaml-front-matter* nil))
    (should (eq/trail-newlines (to-octopress "*bold!*") "**bold!**"))
    (should (eq/trail-newlines (to-octopress "/italic!/") "*italic!*"))
    (should (eq/trail-newlines (to-octopress "*/both/*") "***both***"))
    (should (eq/trail-newlines (to-octopress "/*both*/") "***both***"))))

(ert-deftest octopress-paragraphs ()
  "Test exporting multiple paragraphs"
  (let ((*org-octopress-yaml-front-matter* nil))
    (should (eq/trail-newlines (to-octopress "foo bar baz\n\nqux flarp\n\nzoot zot")
                               "foo bar baz\n\nqux flarp\n\nzoot zot"))))

(ert-deftest octopress-anon-src-block ()
  "Test exporting source blocks without name or language specified"
  (let ((*org-octopress-yaml-front-matter* nil))
    (should (eq/trail-newlines (to-octopress

"#+begin_src
int main() {
   printf(\"Hello, World.\\n\");
}
#+end_src
")

"```
int main() {
   printf(\"Hello, World.\\n\");
}
```"
))))

(ert-deftest octopress-src-block ()
  "Test exporting code blocks with name and language"
  (let ((*org-octopress-yaml-front-matter* nil))
    (should (eq/trail-newlines (to-octopress

"#+name: Hello World in C
#+begin_src C
int main() {
   printf(\"Hello, World.\\n\");
}
#+end_src
")

"``` C Hello World in C
int main() {
   printf(\"Hello, World.\\n\");
}
```"))))
