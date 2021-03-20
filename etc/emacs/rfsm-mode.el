;;; rfsm-mode.el --- simple major mode for editing RFSM code (.fsm). -*- coding: utf-8; lexical-binding: t; -*-
;; Inspired by http://ergoemacs.org/emacs/elisp_syntax_coloring.html

(require 'cc-langs)

(setq rfsm-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("type" "function" "constant" "fsm" "model" "in" "out" "inout" "states" "vars" "trans" "itrans" "on" "when" "with" "where" "input" "output" "shared"))
            (x-types '("int" "bool" "float" "event" "array"))
            (x-constants '("true" "false"))
            (x-functions '("periodic" "sporadic" "value_changes" "return" ))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words))
            (x-constants-regexp (regexp-opt x-constants 'words))
            (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          ;;(,x-events-regexp . font-lock-builtin-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;; (defvar rfsm-mode-syntax-table nil "Syntax table for `rfsm-mode'.")

;; (setq rfsm-mode-syntax-table
;;       (let ( (synTable (make-syntax-table)))
;;         ;; VHDL style comment: “-- …”
;;         (modify-syntax-entry ?- "<" synTable)
;;         (modify-syntax-entry ?\n ">" synTable)
;;         synTable))

(defvar rfsm-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?- ". 12b" table)
    table)
  "Syntax table for `rfsm-mode'.")

(setq rfsm-font-lock-keywords
    (append rfsm-font-lock-keywords
    '(("->\\||" . font-lock-function-name-face))))

(define-derived-mode rfsm-mode c-mode "rfsm mode"
  "Major mode for editing RFSM programs"
  :syntax-table rfsm-syntax-table
  (setq comment-start "-- ")
  (setq comment-end "")
  (setq font-lock-defaults '((rfsm-font-lock-keywords))))

;(set-syntax-table rfsm-mode-syntax-table)

(provide 'rfsm-mode)
