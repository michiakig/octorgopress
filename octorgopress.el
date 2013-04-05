;; Markdown backend for Org-mode
;; Depends on latest (bleeding development branch, maybe v8.x) of Org
;; uses generic export: http://orgmode.org/worg/dev/org-export-reference.html

(require 'ox)

(org-export-define-backend 'markdown
  '(
    (bold . org-markdown-bold)
    (headline . org-markdown-headline)
    (italic . org-markdown-italic)
    (link . org-markdown-link)
    (paragraph . org-markdown-paragraph)
    (section . org-markdown-section)
    (src-block . org-markdown-src-block)
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

(defun org-markdown-italic (elt contents info)
  "Transcode italic text to Markdown equiv of <em>"
  (format "*%s*" contents))

(defun org-markdown-bold (text contents info)
  "Transcode bold text to Markdown equiv of <strong>"
  (format "**%s**" contents))

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
