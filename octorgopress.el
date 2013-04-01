;; Markdown backend for Org-mode
;; Depends on latest (bleeding development branch as of Mar 31) of Org

(require 'ox)

(org-export-define-backend 'markdown
  '(
    ;; (bold . org-ascii-bold)
    ;; (center-block . org-ascii-center-block)
    ;; (clock . org-ascii-clock)
    ;; (code . org-ascii-code)
    ;; (comment . (lambda (&rest args) ""))
    ;; (comment-block . (lambda (&rest args) ""))
    ;; (drawer . org-ascii-drawer)
    ;; (dynamic-block . org-ascii-dynamic-block)
    ;; (entity . org-ascii-entity)
    ;; (example-block . org-ascii-example-block)
    ;; (export-block . org-ascii-export-block)
    ;; (export-snippet . org-ascii-export-snippet)
    ;; (fixed-width . org-ascii-fixed-width)
    ;; (footnote-definition . org-ascii-footnote-definition)
    ;; (footnote-reference . org-ascii-footnote-reference)
    (headline . org-markdown-headline)
    ;; (horizontal-rule . org-ascii-horizontal-rule)
    ;; (inline-src-block . org-ascii-inline-src-block)
    ;; (inlinetask . org-ascii-inlinetask)
    ;; (inner-template . org-ascii-inner-template)
    ;; (italic . org-ascii-italic)
    ;; (item . org-ascii-item)
    ;; (keyword . org-ascii-keyword)
    ;; (latex-environment . org-ascii-latex-environment)
    ;; (latex-fragment . org-ascii-latex-fragment)
    ;; (line-break . org-ascii-line-break)
    (link . org-markdown-link)
    (paragraph . org-markdown-paragraph)
    ;; (plain-list . org-ascii-plain-list)
    ;; (plain-text . org-ascii-plain-text)
    ;; (planning . org-ascii-planning)
    ;; (quote-block . org-ascii-quote-block)
    ;; (quote-section . org-ascii-quote-section)
    ;; (radio-target . org-ascii-radio-target)
    (section . org-markdown-section)
    ;; (special-block . org-ascii-special-block)
    (src-block . org-markdown-src-block)
    ;; (statistics-cookie . org-ascii-statistics-cookie)
    ;; (strike-through . org-ascii-strike-through)
    ;; (subscript . org-ascii-subscript)
    ;; (superscript . org-ascii-superscript)
    ;; (table . org-ascii-table)
    ;; (table-cell . org-ascii-table-cell)
    ;; (table-row . org-ascii-table-row)
    ;; (target . org-ascii-target)
    ;; (template . org-ascii-template)
    ;; (timestamp . org-ascii-timestamp)
    ;; (underline . org-ascii-underline)
    ;; (verbatim . org-ascii-verbatim)
    ;; (verse-block . org-ascii-verse-block)
))

(defun org-markdown-src-block (src-block contents info)
  "Transcode a #+begin_src block from Org to Github style backtick code blocks"
  (let ((lang (org-element-property :language src-block))
        (value (org-element-property :value src-block))
        (name (or (org-element-property :name src-block) "")))
    (concat
     (format "``` %s %s\n" lang name)
     value
     "```\n"
     contents)))

(defun repeat (x n)
  (let (acc)
    (dotimes (_ n acc)
      (push x acc))))

(defun org-markdown-headline (headline contents info)
  (let ((value (org-element-property :raw-value headline))
        (level (org-element-property :level headline)))
    (concat (apply 'concat (repeat "#" level))
            " "
            value
            "\n"
            contents)))

(defun org-markdown-link (link contents info)
  (let ((path (org-element-property :raw-link link)))
    (format "[%s](%s)" contents path)))

(defun org-markdown-paragraph (paragraph contents info)
  contents)

(defun org-markdown-section (section contents info)
  contents)

(defun org-markdown-export-as-markdown
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (if async
      (org-export-async-start
          (lambda (output)
            (with-current-buffer (get-buffer-create "*Org Markdown Export*")
              (erase-buffer)
              (insert output)
              (goto-char (point-min))
              (org-export-add-to-stack (current-buffer) 'markdown)))
        `(org-export-as 'markdown ,subtreep ,visible-only ,body-only ',ext-plist))
    (let ((outbuf (org-export-to-buffer 'markdown "*Org Markdown Export*"
                                        subtreep visible-only body-only ext-plist)))
      (with-current-buffer outbuf (LaTeX-mode))
      (when org-export-show-temporary-export-buffer
        (switch-to-buffer-other-window outbuf)))))

(defun org-markdown-publish-to-markdown (plist filename pub-dir)
  (org-publish-org-to 'markdown filename ".markdown" plist pub-dir))
