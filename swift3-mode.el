;;; swift3-mode.el --- Major-mode for Apple's Swift programming language. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 taku0, Chris Barrett, Bozhidar Batsov, Arthur Evstifeev

;; Authors: taku0 (http://github.com/taku0)
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;;
;; Version: 2.1.1
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

;; Major-mode for Apple's Swift programming language.

;;; Code:

(require 'swift3-mode-lexer)
(require 'swift3-mode-indent)
(require 'swift3-mode-font-lock)
(require 'swift3-mode-beginning-of-defun)
(require 'swift3-mode-repl)

;;;###autoload
(defgroup swift3 nil
  "Major-mode for Apple's Swift programming language."
  :group 'languages
  :prefix "swift-mode:")

;;; Keymap

(defvar swift3-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map (kbd "M-j") #'swift3-mode:indent-new-comment-line)
    (define-key map (kbd "C-M-j") #'swift3-mode:indent-new-comment-line)
    (define-key map (kbd "C-c C-z") 'swift-mode:run-repl)
    (define-key map (kbd "C-c C-f") 'swift-mode:send-buffer)
    (define-key map (kbd "C-c C-r") 'swift-mode:send-region)
    (easy-menu-define swift-menu map "Swift3 Mode menu"
      `("Swift3"
        :help "Swift-specific Features"
        ["Run REPL" swift-mode-run-repl
         :help "Run Swift REPL"]
        ["Send buffer to REPL" swift-mode-send-buffer
         :help "Send the current buffer's contents to the REPL"]
        ["Send region to REPL" swift-mode-send-region
         :help "Send currently selected region to the REPL"]))
    map)
  "Swift3 mode key map.")

;;; `foward-sexp-function'

(defun swift3-mode:forward-sexp (&optional arg)
  (setq arg (or arg 1))
  (if (< 0 arg)
      (while (< 0 arg)
        (while (eq
                (swift3-mode:token:type (swift3-mode:forward-token-or-list))
                'implicit-\;))
        (setq arg (1- arg))))
  (while (< arg 0)
    (while (eq
            (swift3-mode:token:type (swift3-mode:backward-token-or-list))
            'implicit-\;))
    (setq arg (1+ arg))))

;; Imenu

(defun swift3-mode:mk-regex-for-def (keyword)
  "Make a regex matching the identifier introduced by KEYWORD."
  (concat "\\<" (regexp-quote keyword) "\\>"
          "\\s *"
          "\\("
          "\\(?:" "\\sw" "\\|" "\\s_" "\\)" "+"
          "\\)"))

(defconst swift3-mode:imenu-generic-expression
  (list
   (list "Functions" (swift3-mode:mk-regex-for-def "func") 1)
   (list "Classes"   (swift3-mode:mk-regex-for-def "class") 1)
   (list "Enums"     (swift3-mode:mk-regex-for-def "enum") 1)
   (list "Protocols" (swift3-mode:mk-regex-for-def "protocol") 1)
   (list "Structs"   (swift3-mode:mk-regex-for-def "struct") 1)
   (list "Extensions"   (swift3-mode:mk-regex-for-def "extension") 1)
   (list "Constants" (swift3-mode:mk-regex-for-def "let") 1)
   (list "Variables" (swift3-mode:mk-regex-for-def "var") 1))
  "Value for `imenu-generic-expression' in `swift3-mode'.")

;;;###autoload
(define-derived-mode swift3-mode prog-mode "Swift"
  "Major mode for editing Swift code.

\\{swift3-mode-map}"
  :syntax-table swift3-mode:syntax-table
  :group 'swift3

  (setq font-lock-defaults '(swift3-mode:font-lock-keywords))

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  ;; ":" is for Playground Rich Comments Markup Syntax:
  ;; https://developer.apple.com/library/prerelease/ios/documentation/Xcode/Reference/xcode_markup_formatting_ref/PlaygroundRichComments.html
  (setq-local comment-start-skip
              (concat
               "\\s *"
               "\\(?:"
               ;; Single-line comment
               "//+" ":?" "\\|"
               ;; Multi-line comment
               "/\\*+" ":?" "\\|"
               ;; Middle of multi-line-comment
               "\\*+ "
               "\\)"
               "\\s *"))
  (setq-local adaptive-fill-regexp comment-start-skip)
  (setq-local comment-multi-line t)

  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'swift3-mode:indent-line)

  (setq-local forward-sexp-function #'swift3-mode:forward-sexp)

  (setq-local electric-indent-chars
              (append "{}()[]:;,." electric-indent-chars))

  (add-hook 'post-self-insert-hook #'swift3-mode:post-self-insert nil t)

  (setq-local imenu-generic-expression swift3-mode:imenu-generic-expression)

  (setq-local beginning-of-defun-function #'swift3-mode:beginning-of-defun)
  (setq-local end-of-defun-function #'swift3-mode:end-of-defun)
  (message "swift3-mode has been merged into swift-mode. Please uninstall swift3-mode and install swift-mode."))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift3-mode))

(provide 'swift3-mode)

;;; swift3-mode.el ends here
