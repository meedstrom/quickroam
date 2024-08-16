;;; quickroam.el --- Fast versions of org-roam commands -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Edström
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;; Author: Martin Edström <meedstrom91@gmail.com>
;; Created: 2024-04-09
;; Version: 1.2
;; Keywords: org, hypermedia
;; Package-Requires: ((emacs "28.1") (org-roam "2.2.2") (pcre2el "1.12"))
;; URL: https://github.com/meedstrom/quickroam

;;; Commentary:

;; Replaces these commands:
;;
;; `org-roam-node-find' -- use `quickroam-find'
;; `org-roam-node-insert' -- use `quickroam-insert'
;;
;; Bonus: if you find that `org-roam-db-autosave-mode' slows saving files, you
;; can disable it, since the commands work anyway.
;;
;; Setup:
;;
;;     (with-eval-after-load 'org (quickroam-mode))
;;
;; Requires ripgrep installed on your computer.

;;; Code:

(require 'subr-x)
(require 'pcre2el)
(require 'org-roam)

(defcustom quickroam-extra-rg-args
  '("--glob" "**/*.org"
    "--glob" "!**/logseq/**"  ;; ignore Logseq's backup files
    "--glob" "!**/*.sync-conflict-*") ;; ignore Syncthing dups
  "Extra arguments to ripgrep - useful for filtering paths.
Read about glob syntax in the Ripgrep guide:

  https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md

Rather than add arguments here, it's probably easier to rely on
an .ignore or .gitignore file in `org-roam-directory'.

On an exotic system such as Windows, you probably have to edit
these paths.

These arguments are NOT passed directly to a shell, so there's no
need to shell-escape characters.  If you have a filename with a
space, it's fine (I think).  Do not pass several arguments in a
single string, it will not work."
  :type '(repeat string)
  :group 'org-roam)


;;; Plumbing

(defvar quickroam-cache (make-hash-table :size 4000 :test #'equal)
  "Table of org-roam node titles, with associated data in plists.
To peek on the contents, try \\[quickroam-peek].

Contrary to what you might expect, we don't use the org-ID as the
database key because keying on the title instead allows
`completing-read' to use this variable as-is.  If two nodes have
the same title, one gets dropped, but that matches the behavior
of upstream `org-roam-node-find'!

Another way to think of it: since this cache is only used for
`quickroam-find'/`quickroam-insert', it is fine to drop
same-titled nodes.  If it were used to populate `org-roam-db' or
`org-id-locations', we'd have a serious problem.")

(defun quickroam-peek ()
  "For debugging: peek on some rows of `quickroam-cache'."
  (interactive)
  (let ((rows (hash-table-values quickroam-cache)))
    (dotimes (_ 5)
      (print (nth (random (length rows)) rows)))))

;; 4-fold perf boost over `shell-command', if you call rg once per file.  But
;; it's still even faster to find a regexp that lets you call rg only once for
;; the entire directory.
(defun quickroam--program-output (program &rest args)
  "Like `shell-command-to-string', but skip the shell intermediary.

Arguments PROGRAM and ARGS as in `call-process'.  Each argument
is a string usually without spaces, and needs not
backslash-escape characters such as asterisks.  On the other
hand, you get no shell magic such as globs or envvars."
  (with-temp-buffer
    (apply #'call-process program nil t nil args)
    (buffer-string)))

(defconst quickroam-file-level-re
  (rxt-elisp-to-pcre
   (rx bol ":ID:" (+ " ") (group (+ nonl))
       (*? "\n" (* nonl))
       "\n#+title:" (+ " ") (group (+ nonl))))
  "Regexp to match file-level nodes.")

(defconst quickroam-subtree-re
  (rxt-elisp-to-pcre
   (rx bol (+? "*") (+ space) (group (+? nonl))   ; * heading
       (? (+ space) ":" (+ nonl) ":") (* space)   ; (* heading...) :tags:
       (? "\n" (*? space) (not "*") (* nonl))     ; CLOSED/SCHEDULED
       "\n" (*? space) ":PROPERTIES:"
       (*? "\n" (*? space) ":" (* nonl))
       "\n" (*? space) ":ID:" (+ space) (group (+ nonl))
       (*? "\n" (*? space) ":" (* nonl))
       "\n" (*? space) ":END:"))
  "Regexp to match subtree nodes.")

(defun quickroam-seek-file-level-nodes ()
  "Scan `org-roam-directory' for file-level nodes."
  (let* ((default-directory org-roam-directory)
         (rg-result (apply #'quickroam--program-output "rg"
                           `("--multiline"
                             "--ignore-case"
                             "--line-number"
                             "--max-count" "1"
                             "--only-matching"
                             "--replace" "	$1	$2"
                             ,@quickroam-extra-rg-args
                             ,quickroam-file-level-re))))
    (dolist (line (split-string rg-result "\n" t))
      (let* ((groups (split-string line "\t"))
             (file:lnum (split-string (pop groups) ":"))
             ($1 (pop groups))
             ($2 (pop groups)))
        (puthash $2 (list :title $2
                          :id $1
                          :file (car file:lnum)
                          :line-number (string-to-number (cadr file:lnum)))
                 quickroam-cache)))))

;; Today this function looks almost identical to the above
;; `quickroam-seek-file-level-nodes', but if you extend them to do more,
;; they're very likely to diverge.  This has actually happened!  The early
;; commits in org-node show an advanced version of quickroam:
;; https://github.com/meedstrom/org-node/blob/c2d43155d9fd4e96a97df28d0411eb95ce2bfa1a/org-node-cache.el
(defun quickroam-seek-subtree-nodes ()
  "Scan `org-roam-directory' for subtree nodes."
  (let* ((default-directory org-roam-directory)
         (rg-result (apply #'quickroam--program-output "rg"
                           `("--multiline"
                             "--ignore-case"
                             "--line-number"
                             "--only-matching"
                             "--replace" "	$1	$2"
                             ,@quickroam-extra-rg-args
                             ,quickroam-subtree-re))))
    (dolist (line (split-string rg-result "\n" t))
      (let* ((groups (split-string line "\t"))
             (file:lnum (split-string (pop groups) ":"))
             ($1 (pop groups))
             ($2 (pop groups)))
        (puthash $1 (list :title $1
                          :id $2
                          :file (car file:lnum)
                          :line-number (string-to-number (cadr file:lnum)))
                 quickroam-cache)))))

(defun quickroam-reset ()
  "Wipe and rebuild the cache."
  (interactive)
  (unless (executable-find "rg")
    (error "Install ripgrep to use quickroam"))
  (clrhash quickroam-cache)
  (quickroam-seek-file-level-nodes)
  (quickroam-seek-subtree-nodes))

(defun quickroam-reset-soon (&rest _)
  "Reset cache after 1 second if inside an org-roam file now.

It's a simplistic trick to work on :before `delete-file', 
usually works and doesn't need to always work anyway."
  (when (org-roam-file-p)
    (run-with-timer 1 nil #'quickroam-reset)))


;;; Porcelain

;;;###autoload
(defun quickroam-find ()
  "Fast substitute for `org-roam-node-find'."
  (interactive)
  (require 'org-roam)
  (when (or (hash-table-empty-p quickroam-cache)
            (not quickroam-mode))
    (quickroam-reset))
  (let* ((title (completing-read "Node: " quickroam-cache nil nil nil 'org-roam-node-history))
         (node (gethash title quickroam-cache)))
    (if node
        (progn
          (find-file
           (expand-file-name (plist-get node :file) org-roam-directory))
          (widen)
          (goto-char 1)
          (forward-line (1- (plist-get node :line-number))))
      (org-roam-capture-
       :node (org-roam-node-create :title title)
       :props '(:finalize find-file)))))

;;;###autoload
(defun quickroam-insert ()
  "Fast substitute for `org-roam-node-insert'."
  (interactive nil org-mode)
  (require 'org-roam)
  (when (or (hash-table-empty-p quickroam-cache)
            (not quickroam-mode))
    (quickroam-reset))
  (let* ((beg nil)
         (end nil)
         (region-text (when (region-active-p)
                        (setq beg (region-beginning))
                        (setq end (region-end))
                        (org-link-display-format
                         (buffer-substring-no-properties beg end))))
         (title (completing-read "Node: " quickroam-cache nil nil nil 'org-roam-node-history))
         (node (gethash title quickroam-cache))
         (id (plist-get node :id))
         (link-desc (or region-text title)))
    (if node
        (atomic-change-group
          (if region-text
              (delete-region beg end)
            ;; Try to strip the todo keyword, whatever counts as todo syntax
            ;; in the target file.  Fail silently because it matters not much.
            (ignore-errors
              (org-roam-with-file
                  (expand-file-name (plist-get node :file) org-roam-directory)
                  nil
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char 1)
                    (forward-line (1- (plist-get node :line-number)))
                    (setq link-desc (nth 4 (org-heading-components))))))))
          (insert (org-link-make-string (concat "id:" id) link-desc))
          (run-hook-with-args 'org-roam-post-node-insert-hook id link-desc))
      (atomic-change-group
        (org-roam-capture-
         :node (org-roam-node-create :title title)
         :props (append
                 (when region-text
                   (list :region (cons (set-marker (make-marker) beg)
                                       (set-marker (make-marker) end))))
                 (list :link-description link-desc
                       :finalize 'insert-link)))))))

;;;###autoload
(define-obsolete-function-alias #'quickroam-enable-cache #'quickroam-mode
  "2024-08-16")

;;;###autoload
(define-obsolete-function-alias #'quickroam-enable #'quickroam-mode
  "2024-04-13")

;;;###autoload
(define-minor-mode quickroam-mode
  "Instruct on-save hooks and such things to update the cache.
Using a cache lets `quickroam-find' and `quickroam-insert' open
the minibuffer slightly faster -- a difference on the order of
going from 100ms to 20ms, but it is optional."
  :group 'org-roam
  :global t
  (if quickroam-mode
      (progn
        (add-hook 'after-save-hook #'quickroam-reset-soon)
        (advice-add #'delete-file :before #'quickroam-reset-soon)
        (advice-add #'rename-file :before #'quickroam-reset-soon))
    (remove-hook 'after-save-hook #'quickroam-reset-soon)
    (advice-remove #'delete-file #'quickroam-reset-soon)
    (advice-remove #'rename-file #'quickroam-reset-soon)))

(provide 'quickroam)

;;; quickroam.el ends here
