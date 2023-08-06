;;; consult-ag.el --- The silver searcher integration using Consult -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Kanon Kakuno

;; Author: Kanon Kakuno <yadex205@outlook.jp> and contributors
;; Homepage: https://github.com/yadex205/consult-ag
;; Package-Requires: ((emacs "27.1") (consult "0.32"))
;; SPDX-License-Identifier: MIT
;; Version: 0.2.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; consult-ag provides interfaces for using `ag` (The Silver Searcher).
;; To use this, turn on `consult-ag` in your init-file or interactively.

;;; Code:

(eval-when-compile ; IDK but required for byte-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'consult)

(defun consult-ag--make-builder (paths)
  "Build command line given INPUT."
  (let ((cmd (split-string-and-unquote "stdbuf -oL ag --vimgrep")))
    (lambda (input)
     (pcase-let* ((`(,arg . ,opts)
                   (consult--command-split input))
                  (`(,re . ,hl)
                   (funcall consult--regexp-compiler arg 'extended nil)))
    (cons `(,@cmd ,@opts ,(consult--join-regexps re 'extended) ,@paths) hl)))))

(defun consult-ag--format (async builder)
  "Return ASYNC function highlighting ag match results.
BUILDER is the command line builder function."
  (let (highlight)
    (lambda (action)
      (cond
       ((stringp action)
        (setq highlight (cdr (funcall builder action)))
        (funcall async action))
       ((consp action)
        (let ((file "") (file-len 0) result)
          (save-match-data
            (dolist (str action)
              (when (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)$" str))
                ;; We share the file name across candidates to reduce
                ;; the amount of allocated memory.
                (unless (and (= file-len (- (match-end 1) (match-beginning 1)))
                             (eq t (compare-strings
                                    file 0 file-len
                                    str (match-beginning 1) (match-end 1) nil)))
                  (setq file (match-string 1 str)
                        file-len (length file)))
                (let* ((line (match-string 2 str))
                       (column (match-string 3 str))
                       (content (match-string 4 str))
                       (line-len (length line))
                       (sep ":"))
                  (when (length> content consult-grep-max-columns)
                    (setq content (substring content 0 consult-grep-max-columns)))
                  (when highlight
                    (funcall highlight content))
                  (setq str (concat file sep line sep content))
                  ;; Store file name in order to avoid allocations in `consult--prefix-group'
                  (add-text-properties 0 file-len `(face consult-file consult--prefix-group ,file) str)
                  (put-text-property (1+ file-len) (+ 1 file-len line-len) 'face 'consult-line-number str)
                  (push (propertize str 'filename file 'row line 'column column) result))))
          (funcall async (nreverse result))))
       (t (funcall async action))))))

(defun consult-ag--grep-position (cand &optional find-file)
  "Return the candidate position marker for CAND.
FIND-FILE is the file open function, defaulting to `find-file`."
  (when cand
    (let ((file (get-text-property 0 'filename cand))
          (row (string-to-number (get-text-property 0 'row cand)))
          (column (- (string-to-number (get-text-property 0 'column cand)) 1)))
      (consult--marker-from-line-column (funcall (or find-file #'find-file) file) row column))))

(defun consult-ag--grep-state ()
  "Not documented."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (action cand)
      (unless cand
        (funcall open nil))
      (funcall jump action (consult-ag--grep-position cand open)))))

;;;###autoload
(defun consult-ag (&optional dir initial)
  "Consult ag for query in TARGET file(s) with INITIAL input."
  (interactive)
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Consult ag: " dir))
               (default-directory dir)
               (builder (consult-ag--make-builder paths)))
    (consult--read (consult--async-command builder
                     (consult-ag--format builder)
                     :file-handler t)
                   :prompt prompt
                   :lookup #'consult--lookup-member
                   :state (consult-ag--grep-state)
                   :initial (consult--async-split-initial initial)
                   :add-history (consult--async-split-thingatpt 'symbol)
                   :require-match t
                   :category 'consult-grep
                   :group #'consult--prefix-group
                   :history '(:input consult--grep-history)
                   :sort nil)))

(provide 'consult-ag)

;;; consult-ag.el ends here
