;;; flycheck-ide-backend.el --- flycheck-mode checker for ide-backend-mode -*- lexical-binding: t -*-

;; Copyright (C) 2015 by Arne Link

;; Author:    Arne Link <link.arne@gmail.com>
;; URL:       https://github.com/Codas/flycheck-ide-backend
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

;; `flycheck-mode' checker for haskell files via `ide-backend-mode'.
;;
;; Provides fast and asynchronous sytax checks using ide-backend-mode.
;; Add `flycheck-ide-backend' to `flycheck-mode' syntax checkers.
;;
;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;;; Code:

(require 'cl-lib)
(require 'flycheck)
(require 'ide-backend-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checker definitions
(defun flycheck-ide-backend-start (checker callback)
  "Start a GHCi load with CHECKER.
CALLBACK is the status callback passed by Flycheck."
  (let ((buffer (current-buffer))
        (project-dir (ide-backend-mode-dir)))
    (flycheck-ide-backend-mode-load
     (lambda (_ reply)
       (condition-case err
           (let ((errors (mapcar
                          (lambda (item) (flycheck-ide-backend-parse-error
                                          item checker buffer project-dir))
                          (cdr (assoc 'errors reply)))))
             (funcall callback 'finished (delq nil errors)))
         (error (funcall callback 'errored (error-message-string err))))
       :done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Parser
(defun flycheck-ide-backend-parse-error (item checker buffer project-dir)
  "Parse a haskell error ITEM from CHECKER in BUFFER into a `flycheck-error'.
Return the corresponding `flycheck-error'."
  (let* ((msg (cdr (assoc 'msg item)))
         (kind (cdr (assoc 'kind item)))
         (span (cdr (assoc 'span item)))
         (fp (cdr (assoc 'filePath span)))
         (sl (cdr (assoc 'fromLine span)))
         (sc (cdr (assoc 'fromColumn span)))
         ;; (el (cdr (assoc 'toLine span)))
         ;; (ec (cdr (assoc 'toColumn span)))
         (level (cond ((string= kind "error") 'error)
                      ((string= kind "warning") 'warning)
                      ((string= kind "info") 'info)
                      (t 'error))))
    (flycheck-error-new-at
     sl
     sc
     level
     (unless (string-empty-p msg) msg)
     :checker checker
     :buffer buffer
     :filename (concat project-dir fp)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;; These are mostly copied from ide-backend mode, but since not custom callbacks
;; can be supplied for the error checking function after loading the current
;; file, these have to be copied and slightly modified
(defun flycheck-ide-backend-mode-load (callback)
  "Load the current buffer's file."
  (let ((filename (buffer-file-name)))
    (with-current-buffer (ide-backend-mode-buffer)
      (flycheck-ide-backend-mode-update-file
       (file-relative-name filename default-directory) callback))))

(defun flycheck-ide-backend-mode-update-file (filepath callback)
  "Load the given filepath."
  (with-current-buffer (ide-backend-mode-buffer)
    (ide-backend-mode-enqueue
     `((request . "updateSession")
       (update . (,(ide-backend-mode-list->hashtable
                    `((update . "updateSourceFileFromFile")
                      (filePath . ,filepath))))))
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
         (ide-backend-mode-enqueue
          `((request . "getSourceErrors"))
          nil
          callback)
         :done))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup

(flycheck-define-generic-checker 'haskell-ide-backend
  "A syntax and type checker for Haskell using ide-backend-mode."
  :start 'flycheck-ide-backend-start
  :modes '(haskell-mode)
  :predicate (lambda () ide-backend-mode)
  :next-checkers '((warning . haskell-hlint)))

;;;###autoload
(defun flycheck-ide-backend-setup ()
  "Setup Flycheck ide-backend.
Add `haskell-ide-backend' checker to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'haskell-ide-backend))

(provide 'flycheck-ide-backend)
;;; company-ide-backend.el ends here
