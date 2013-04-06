;; Octopress backend for Org-mode
;; Depends on latest (bleeding development branch, maybe v8.x) of Org
;; uses generic export: http://orgmode.org/worg/dev/org-export-reference.html

(require 'ox)

(org-export-define-backend 'octopress
  '(
    (bold . org-octopress-bold)
    (headline . org-octopress-headline)
    (italic . org-octopress-italic)
    (link . org-octopress-link)
    (paragraph . org-octopress-paragraph)
    (section . org-octopress-section)
    (src-block . org-octopress-src-block)
    (template . org-octopress-template)
))

(defun org-octopress-template (contents info)
  "Accepts the final transcoded string and a plist of export options,
returns final string with YAML frontmatter as preamble"
  (let ((title (car (plist-get info :title)))
        (date (car (plist-get info :date)))
        (time "")
        (frontmatter
"---
layout: post
title: %s
date: %s %s
comments: true
external-url:
categories:
---
"))
    (concat (format frontmatter title date time)
            contents)))

(defun org-octopress-src-block (src-block contents info)
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

(defun org-octopress-headline (headline contents info)
  (let ((value (org-element-property :raw-value headline))
        (level (org-element-property :level headline)))
    (concat (apply 'concat (repeat "#" level))
            " "
            value
            "\n"
            contents)))

(defun org-octopress-link (link contents info)
  (let ((path (org-element-property :raw-link link)))
    (format "[%s](%s)" contents path)))

(defun org-octopress-paragraph (paragraph contents info)
  contents)

(defun org-octopress-section (section contents info)
  contents)

(defun org-octopress-italic (elt contents info)
  "Transcode italic text to Octopress equiv of <em>"
  (format "*%s*" contents))

(defun org-octopress-bold (text contents info)
  "Transcode bold text to Octopress equiv of <strong>"
  (format "**%s**" contents))

(defun org-octopress-export-as-octopress
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (if async
      (org-export-async-start
          (lambda (output)
            (with-current-buffer (get-buffer-create "*Org Octopress Export*")
              (erase-buffer)
              (insert output)
              (goto-char (point-min))
              (org-export-add-to-stack (current-buffer) 'octopress)))
        `(org-export-as 'octopress ,subtreep ,visible-only ,body-only ',ext-plist))
    (let ((outbuf (org-export-to-buffer 'octopress "*Org Octopress Export*"
                                        subtreep visible-only body-only ext-plist)))
      (with-current-buffer outbuf (LaTeX-mode))
      (when org-export-show-temporary-export-buffer
        (switch-to-buffer-other-window outbuf)))))

(defun org-octopress-publish-to-octopress (plist filename pub-dir)
  (org-publish-org-to 'octopress filename ".md" plist pub-dir))
