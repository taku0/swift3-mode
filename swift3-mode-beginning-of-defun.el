;;; swift3-mode-beginning-of-defun.el --- Major-mode for Apple's Swift programming language, beginning/end-of-defun. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 taku0

;; Authors: taku0 (http://github.com/taku0)
;;
;; Version: 2.1.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: languages swift

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

;; beginning-of-defun and end-of-defun

;;; Code:

(require 'swift3-mode-lexer)
(require 'swift3-mode-indent)

(defun swift3-mode:beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun."
  (interactive)
  (setq arg (or arg 1))
  (let (result
        pos)
    (if (<= 0 arg)
        (while (< 0 arg)
          (setq result (swift3-mode:beginning-of-defun-1
                        #'swift3-mode:backward-token-or-list))
          (setq arg (1- arg)))
      (while (< arg 0)
        (setq pos (point))

        (swift3-mode:beginning-of-statement)

        (when (<= (point) pos)
          (while (not
                  (memq
                   (swift3-mode:token:type (swift3-mode:forward-token-or-list))
                   '({} outside-of-buffer)))))

        (setq result (swift3-mode:beginning-of-defun-1
                      (lambda ()
                        (prog1 (swift3-mode:forward-token-or-list)
                          (forward-comment (point-max))))))
        (setq arg (1+ arg))))
    result))

(defun swift3-mode:beginning-of-defun-1 (next-token-function)
  (catch 'swift3-mode:found-defun
    (while (not (eq (swift3-mode:token:type (funcall next-token-function))
                    'outside-of-buffer))
      (when (save-excursion (swift3-mode:is-point-before-body-of-defun))
        (swift3-mode:beginning-of-statement)
        (throw 'swift3-mode:found-defun t)))
    nil))


(defun swift3-mode:is-point-before-body-of-defun ()
  (and
   (= (char-after) ?{)
   (progn
     ;; Skips implicit ;
     (forward-comment (- (point)))
     (let* ((defun-keywords '("class" "struct" "protocol" "enum" "extension"
                              "func" "operator" "var" "get" "set" "willSet"
                              "didSet" "deinit" "subscript"))
            (previous-token (swift3-mode:backward-token-or-list))
            (previous-type (swift3-mode:token:type previous-token))
            (previous-text (swift3-mode:token:text previous-token)))
       (while (and
               (not (eq previous-type 'outside-of-buffer))
               (not (memq previous-type swift3-mode:statement-parent-tokens))
               (not (member previous-text swift3-mode:statement-parent-tokens))
               (not (member previous-text defun-keywords))
               (not (and (equal previous-text "init")
                         (save-excursion
                           ;; Excludes self.init() {}
                           (not
                            (equal
                             (swift3-mode:token:text (swift3-mode:backward-token))
                             "."))))))
         (setq previous-token (swift3-mode:backward-token-or-list))
         (setq previous-type (swift3-mode:token:type previous-token))
         (setq previous-text (swift3-mode:token:text previous-token)))
       (unless (bobp)
         (swift3-mode:forward-token-simple))
       (or (equal previous-text "init")
           (member previous-text defun-keywords))))))

(defun swift3-mode:beginning-of-statement ()
  "Move backward to the beginning of a statement or some kind of expression.

Intended for internal use."
  (let ((parent (swift3-mode:backward-sexps-until
                 swift3-mode:statement-parent-tokens)))
    (forward-comment (point-max))
    (swift3-mode:goto-non-comment-bol)
    (when (< (point) (swift3-mode:token:end parent))
      (goto-char (swift3-mode:token:end parent)))
    (swift3-mode:skip-whitespaces)))


(defun swift3-mode:end-of-defun (&optional arg)
  "Move forward to the end of a defun."
  (interactive)
  (setq arg (or arg 1))
  (let (result)
    (if (<= 0 arg)
        (while (< 0 arg)
          (setq result (swift3-mode:end-of-defun-1
                        #'swift3-mode:forward-token-or-list
                        ))
          (setq arg (1- arg)))
      (while (< arg 0)
        (setq result (swift3-mode:end-of-defun-1
                      (lambda ()
                        (prog1 (swift3-mode:backward-token-or-list)
                          (forward-comment (- (point)))))))
        (setq arg (1+ arg))))
    result))

(defun swift3-mode:end-of-defun-1 (next-token-function)
  (catch 'swift3-mode:found-defun
    (while (not (eq (swift3-mode:token:type (funcall next-token-function))
                    'outside-of-buffer))
      (when (and (= (char-before) ?})
                 (save-excursion
                   (backward-list)
                   (swift3-mode:is-point-before-body-of-defun)))
        (throw 'swift3-mode:found-defun t)))
    nil))


(provide 'swift3-mode-beginning-of-defun)

;;; swift3-mode-beginning-of-defun.el ends here
