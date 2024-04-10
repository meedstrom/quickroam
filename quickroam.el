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
;; Version: 0.4.3-pre
;; Keywords: outlines, hypermedia
;; Package-Requires: ((emacs "29.1") (org-roam "2.2.2") (pcre2el "1.12"))
;; URL: https://github.com/meedstrom/quickroam

;; This file is not part of GNU Emacs.

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
;;     (add-hook 'org-mode-hook #'quickroam-enable)
;;
;; Requires ripgrep.

;;; Code:

(require 'org-roam)
(require 'pcre2el)

(defcustom quickroam-extra-rg-args
  '("--glob" "**/*.org"
    "--glob" "!**/logseq/**"
    "--glob" "!**/*.sync-conflict-*")
  "Extra arguments to ripgrep - useful for filtering paths.
If you are on an exotic system such as Windows, you probably have
to edit these paths.

Read about syntax at:

  https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md#manual-filtering-globs

These arguments are NOT passed directly to a shell, so there's no
need to shell-escape characters.  If you have a filename with a
space, it's fine (I think).  But do not pass several arguments in
one string."
  :type '(repeat string)
  :group 'org-roam)


;;; Plumbing

(defun quickroam--program-output (program &rest args)
  "Like `shell-command-to-string', but skip the shell intermediary.

Arguments PROGRAM and ARGS as in `call-process'.  Each argument
is a string usually without spaces, and needs not
backslash-escape characters such as asterisks.  On the other
hand, you get no shell magic such as globs or envvars."
  (with-temp-buffer
    (apply #'call-process program nil t nil args)
    (buffer-string)))

(defvar quickroam-cache
  (make-hash-table :size 4000 :test #'equal)
  "Table of org-roam IDs with associated data in plists.
To peek on the contents, try \\[quickroam--print-random-rows].")

(defconst quickroam-file-level-re
  (rxt-elisp-to-pcre
   (rx bol ":ID:" (+ " ") (group (+ nonl))
       (*? "\n" (* nonl))
       "\n#+title:" (+ " ") (group (+ nonl))))
  "Regexp to match file-level nodes.")

(defun quickroam-scan-file-level ()
  "Scan `org-roam-directory' for file-level nodes."
  (let* ((default-directory org-roam-directory)
         (result (apply #'quickroam--program-output "rg"
                        `("--multiline"
                          "--ignore-case"
                          "--line-number"
                          "--only-matching"
                          "--replace" "$1	$2"
                          ,@quickroam-extra-rg-args
                          ,quickroam-file-level-re))))
    (dolist (line (string-split result "\n" t))
      (let* ((groups (string-split line "\t"))
             ($1 (pop groups))
             ($2 (pop groups))
             (blob (string-split $1 ":"))
             (file (pop blob))
             (line-number (string-to-number (pop blob)))
             (id (string-join blob))) ;; Maybe some ppl have colons inside ID
        (puthash id (list :title $2
                          :id id
                          :file file
                          :line-number line-number)
                 quickroam-cache)))))

;; Whoa boy so hairy!  Glad for `rx'.
(defconst quickroam-subtree-re
  (rxt-elisp-to-pcre
   (rx bol (+? "*") (+ space) (group (+? nonl))
       (? (+ space) ":" (+ nonl) ":") (* space)   ; :tags:
       (? "\n" (*? space) (not "*") (* nonl))     ; CLOSED/SCHEDULED
       "\n" (*? space) ":PROPERTIES:"
       (*? "\n" (* nonl))
       "\n" (*? space) ":ID:" (+ space) (group (+ nonl))
       (*? "\n" (* nonl))
       "\n" (*? space) ":END:"))
  "Regexp to match subtree nodes.")

(defun quickroam-scan-subtrees ()
  "Scan `org-roam-directory' for subtree nodes."
  (let* ((default-directory org-roam-directory)
         (result (apply #'quickroam--program-output "rg"
                        `("--multiline"
                          "--ignore-case"
                          "--line-number"
                          "--only-matching"
                          "--replace" "$1	$2"
                          ,@quickroam-extra-rg-args
                          ,quickroam-subtree-re))))
    (dolist (line (string-split result "\n" t))
      (let* ((groups (string-split line "\t"))
             ($1 (pop groups))
             ($2 (pop groups))
             (blob (string-split $1 ":"))
             (file (pop blob))
             (line-number (string-to-number (pop blob)))
             (title (string-join blob))) ;; Title may contain colons
        (puthash $2 (list :title title
                          :id $2
                          :file file
                          :line-number line-number)
                 quickroam-cache)))))

(defun quickroam--print-random-rows ()
  "For debugging."
  (interactive)
  (require 'seq)
  (let ((rows (cl-loop for v being the hash-values of quickroam-cache
                       collect v)))
    (dotimes (_ 5)
      (print (seq-random-elt rows)))))

(defun quickroam-reset (&optional interactive)
  "Wipe and rebuild the cache."
  (interactive "p")
  (unless (executable-find "rg")
    (error "Install ripgrep to use quickroam"))
  (let ((then (current-time)))
    (clrhash quickroam-cache)
    (quickroam-scan-subtrees)
    (quickroam-scan-file-level)
    (funcall (if interactive #'message #'format)
             "Rebuilt quickroam cache in %.3f seconds"
             (float-time (time-since then)))))

(defun quickroam-reset-soon (&rest _)
  "Call `quickroam-reset' after 1 second if inside an org-roam file now."
  (when (org-roam-file-p)
    (run-with-timer 1 nil #'quickroam-reset)))


;;; Porcelain

;;;###autoload
(defun quickroam-find ()
  "Fast substitute for `org-roam-node-find'."
  (interactive)
  (when (hash-table-empty-p quickroam-cache)
    (quickroam-reset))
  ;; Based on design from `org-roam-node-find'
  (let* ((coll (cl-loop for qr-node being the hash-values of quickroam-cache
                        collect (cons (plist-get qr-node :title)
                                      (plist-get qr-node :id))))
         (title (completing-read "Node: " coll nil nil nil 'org-roam-node-history))
         (id (cdr (assoc title coll))))
    (unless (and id
                 (when-let ((qr-node (gethash id quickroam-cache)))
                   (find-file (expand-file-name
                               (plist-get qr-node :file) org-roam-directory))
                   (goto-line (plist-get qr-node :line-number))
                   t))
      (org-roam-capture-
       :node (org-roam-node-create :title title)
       :props '(:finalize find-file)))))

;;;###autoload
(defun quickroam-insert ()
  "Fast substitute for `org-roam-node-insert'."
  (interactive)
  (when (hash-table-empty-p quickroam-cache)
    (quickroam-reset))
  ;; Based on design from `org-roam-node-insert'
  (atomic-change-group
    (let* (region-text
           beg end
           (_ (when (region-active-p)
                (setq beg (set-marker (make-marker) (region-beginning)))
                (setq end (set-marker (make-marker) (region-end)))
                (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
           (coll (cl-loop for qr-node being the hash-values of quickroam-cache
                          collect (cons (plist-get qr-node :title)
                                        (plist-get qr-node :id))))
           (title (completing-read "Node: " coll nil nil nil 'org-roam-node-history))
           (id (or (cdr (assoc title coll))))
           (node (unless id
                   (let ((node (org-roam-node-create :title title)))
                     (setq id (org-roam-node-id node))
                     node)))
           (description (or region-text title)))
      (if id
          (progn
            (when region-text
              (delete-region beg end)
              (set-marker beg nil)
              (set-marker end nil))
            (insert (org-link-make-string
                     (concat "id:" id)
                     description))
            (run-hook-with-args 'org-roam-post-node-insert-hook
                                id
                                description))
        (org-roam-capture-
         :node node
         :props (append
                 (when (and beg end)
                   (list :region (cons beg end)))
                 (list :link-description description
                       :finalize 'insert-link)))))))

;;;###autoload
(defun quickroam-enable ()
  "Designed for `org-mode-hook'."
  (quickroam-mode)
  (remove-hook 'org-mode-hook #'quickroam-enable))

;;;###autoload
(define-minor-mode quickroam-mode
  "Setup on-save hooks etc to keep the cache updated.
This permits `quickroam-find' and `quickroam-insert' to know about new files
immediately."
  :global t
  (if quickroam-mode
      (progn
        (add-hook 'after-save-hook #'quickroam-reset-soon)
        (advice-add #'delete-file :before #'quickroam-reset-soon)
        (advice-add #'rename-file :before #'quickroam-reset-soon))
    (advice-remove #'rename-file #'quickroam-reset-soon)
    (advice-remove #'delete-file #'quickroam-reset-soon)
    (remove-hook 'after-save-hook #'quickroam-reset-soon)))

;; DEPRECATED 2024-04-10
(defun quickroam-aftersave ()
  (message "`quickroam-aftersave' deprecated, turn on `quickroam-mode' instead"))

(provide 'quickroam)
;;; quickroam.el ends here
