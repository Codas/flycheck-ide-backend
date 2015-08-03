;;; flycheck-stack.el --- flycheck-mode checker for stack-mode -*- lexical-binding: t -*-

;; Copyright (C) 2015 by Arne Link

;; Author:    Arne Link <link.arne@gmail.com>
;; URL:       https://github.com/Codas/flycheck-stack
;; Version:   0.0.1
;; Package-Requires: ((cl-lib "0.5") (flycheck "0.21") (emacs "24"))
;; Keywords:  haskell, tools, convenience
;; Stability: experimental

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `flycheck-mode' checker for haskell files via `stack-mode'.
;;
;; Provides fast and asynchronous sytax checks using stack-mode.
;; Add `flycheck-stack' to `flycheck-mode' syntax checkers.
;;
;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;;; Code:

(require 'cl-lib)
(require 'flycheck)
(require 'stack-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checker definitions
(defvar flycheck-stack-last-errors nil)

(defun flycheck-stack-start (checker callback)
  "Start a GHCi load with CHECKER.
CALLBACK is the status callback passed by Flycheck."
  (let ((flycheck-stack-got-progress nil)
        (buffer (current-buffer))
        (project-dir (stack-mode-dir)))
    (flycheck-stack-mode-load
     (lambda (_ reply)
       (cond
        ((equal "ResponseUpdateSession" (cdr (assoc 'tag reply)))
         (let* ((update-content (cdr (assoc 'contents reply)))
                (update-content-tag (cdr (assoc 'tag update-content))))
           (progn
             (when (equal "UpdateStatusProgress" update-content-tag)
               (setq flycheck-stack-got-progress 1))
             :continue)))
        ((equal "ResponseGetSourceErrors" (cdr (assoc 'tag reply)))
          (progn
            (condition-case err
                (let ((errors (mapcar
                               (lambda (item) (flycheck-stack-parse-error
                                               item checker buffer project-dir))
                               (cdr (assoc 'contents reply)))))
                  (if flycheck-stack-got-progress
                      (progn
                        (setq flycheck-stack-last-errors errors)
                        (setq flycheck-stack-got-progress nil)
                        (funcall callback 'finished (delq nil errors)))
                    (progn
                      (funcall callback 'finished (delq nil flycheck-stack-last-errors)))))
              (error (funcall callback 'errored (error-message-string err))))
            :done))
          (t :continue))
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Parser
(defun flycheck-stack-parse-error (item checker buffer project-dir)
  "Parse a haskell error ITEM from CHECKER in BUFFER into a `flycheck-error'.
Return the corresponding `flycheck-error'."
  (let* ((msg (cdr (assoc 'errorMsg item)))
         (kind (cdr (assoc 'errorKind item)))
         (span (cdr (assoc 'errorSpan item)))
         (span-contents (cdr (assoc 'contents span)))
         (fp (cdr (assoc 'spanFilePath span-contents)))
         (sl (cdr (assoc 'spanFromLine span-contents)))
         (sc (cdr (assoc 'spanFromColumn span-contents)))
         ;; (el (cdr (assoc 'spanToLine span-contents)))
         ;; (ec (cdr (assoc 'spanToColumn span-contents)))
         (level (cond ((string= kind "KindError") 'error)
                      ((string= kind "KindWarning") 'warning)
                      ((string= kind "KindInfo") 'info)
                      (t 'error))))
    (progn
      (flycheck-error-new-at
       sl
       sc
       level
       (unless (string-empty-p msg) msg)
       :checker checker
       :buffer buffer
       :filename (concat project-dir fp)
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;; These are mostly copied from stack mode, but since not custom callbacks
;; can be supplied for the error checking function after loading the current
;; file, these have to be copied and slightly modified
(defun flycheck-stack-mode-load (callback)
  "Load the current buffer's file."
  (let ((filename (buffer-file-name)))
    (with-current-buffer (stack-mode-buffer)
      (flycheck-stack-mode-update-file
       (file-relative-name filename default-directory) callback))))

(defun flycheck-stack-mode-update-file (filepath callback)
  "Load the given filepath."
  (with-current-buffer (stack-mode-buffer)
    (stack-mode-enqueue
     `((tag . "RequestUpdateSession")
       (contents . []))
     nil
     (lambda (_ reply)
       (cond
        ((assoc 'progress reply)
         (let ((msg (cdr (assoc 'parsedMsg (assoc 'progress reply))))
               (step (cdr (assoc 'step (assoc 'progress reply))))
               (steps (cdr (assoc 'numSteps (assoc 'progress reply)))))
           (message "%s %s"
                    (propertize msg 'face 'bold)
                    (propertize (format "(%d of %d)" step steps)
                                'face 'font-lock-comment-face)))
         :continue)
        (t
         (stack-mode-enqueue
          `((tag . "RequestGetSourceErrors")
            (contents . []))
          nil
          callback)
         :done))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup

(flycheck-define-generic-checker 'haskell-stack
  "A syntax and type checker for Haskell using stack-mode."
  :start 'flycheck-stack-start
  :modes '(haskell-mode)
  :predicate (lambda () (and
                         stack-mode
                         (flycheck-buffer-saved-p)
                         (stack-mode-process)
                         (process-live-p (stack-mode-process))))
  :next-checkers '((warning . haskell-hlint)))

;;;###autoload
(defun flycheck-stack-setup ()
  "Setup Flycheck stack.
Add `haskell-stack' checker to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'haskell-stack))

(provide 'flycheck-stack)
;;; company-stack.el ends here
