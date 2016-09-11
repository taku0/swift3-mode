;;; swift3-mode-test-indent.el --- Test for swift3-mode: indentation  -*- lexical-binding: t -*-

;; Copyright (C) 2016 taku0

;; Authors: taku0 (http://github.com/taku0)
;;
;; Version: 2.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: languages swift
;; URL: https://github.com/taku0/swift3-mode

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test for swift3-mode: indentation.
;; Execute swift3-mode:run-test:indent interactively or in batch mode.

;;; Code:

(require 'swift3-mode)
(require 'swift3-mode-indent)

(defvar swift3-mode:test:basedir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun swift3-mode:setup-error-buffer ()
  "Initialize and switch to the error buffer.

Return the error-buffer"
  (switch-to-buffer (get-buffer-create "*swift3-mode-test-indent*"))
  (fundamental-mode)
  (setq view-read-only nil)
  (erase-buffer)
  (current-buffer))

(defun swift3-mode:run-test:indent ()
  "Run indentation test for swift3-mode."
  (interactive)
  (let ((error-buffer
         (if noninteractive nil (swift3-mode:setup-error-buffer)))
        (current-line 0)
        (error-count 0))
    (setq default-directory
          (concat (file-name-as-directory swift3-mode:test:basedir)
                  "swift-files"))

    (dolist (swift-file (file-expand-wildcards "*.swift"))
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (insert-file-contents-literally swift-file)
        (swift3-mode)
        (setq current-line 0)
        (while (not (eobp))
          (setq current-line (1+ current-line))
          (unless (looking-at ".*//.*swift3-mode:test:keep-indent")
            (when (looking-at ".*//.*swift3-mode:test:eval\\(.*\\)")
              (eval-region (match-beginning 1) (match-end 1)))
            (unless
                (swift3-mode:test-current-line-indent
                 swift-file current-line error-buffer)
              (setq error-count (1+ error-count))))
          (forward-line))))
    (when (= error-count 0)
      (swift3-mode:print-message error-buffer "no regressions\n"))
    (when (not noninteractive)
      (compilation-mode))))

(defun swift3-mode:test-current-line-indent
    (swift-file current-line error-buffer)
  "Run indentation test for swift3-mode on current line.

SWIFT-FILE is the filename of the current test case.
CURRENT-LINE is the current line number.
ERROR-BUFFER is the buffer to output errors."
  (back-to-indentation)
  (let ((original-indent (current-column))
        computed-indent
        (known-bug (looking-at ".*//.*swift3-mode:test:known-bug")))
    (delete-horizontal-space)
    (when (= original-indent 0)
      (indent-line-to 1))

    (swift3-mode:indent-line)
    (back-to-indentation)
    (setq computed-indent (current-column))
    (indent-line-to original-indent)

    (when (/= original-indent computed-indent)
      (swift3-mode:show-error
       error-buffer swift-file current-line
       (if known-bug "warning" "error")
       (concat
        (if known-bug "(knwon bug) " "")
        "expected "
        (prin1-to-string original-indent)
        " but "
        (prin1-to-string computed-indent))))

    (when (and (= original-indent computed-indent) known-bug)
      (swift3-mode:show-error
       error-buffer swift-file current-line
       "info"
       "known-bug is fixed somehow"))

    (= original-indent computed-indent)))

(defun swift3-mode:show-error (error-buffer file line level message)
  "Show an error message to the ERROR-BUFFER or stdout.

If the Emacs is in the batch mode, the message is printed to the stdout.
Otherwise, the message is appended to the ERROR-BUFFER.

FILE is the filename of the test case.
LINE is the line number of the error.
LEVEL is the error level (e.g. error, warning).
MESSAGE is the error message."
  (let ((formatted
         (concat
          "swift3-mode-test:"
          file
          ":"
          (prin1-to-string line)
          ": "
          level
          ": "
          message
          "\n")))
    (swift3-mode:print-message error-buffer formatted)))

(defun swift3-mode:print-message (error-buffer message)
  "Print a message to the ERROR-BUFFER or stdout.

If the Emacs is in the batch mode, MESSAGE is printed to the stdout.
Otherwise, MESSAGE is appended to the ERROR-BUFFER."
  (if noninteractive
      (princ message)
    (with-current-buffer error-buffer
      (insert-and-inherit message))))

(provide 'swift3-mode-test-indent)

;;; swift3-mode-test-indent.el ends here
