;;; evil-quick-sender.el --- Use "s" key to eval expression  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Liu233w

;; Author: Liu233w <wwwlsmcom@outlook.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
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

;; 在 normal 状态下使用 s 来求值表达式，在不同的 mode 中会调用不同的函数。比如，
;; 在 emacs-lisp-mode 中会调用 eval-last-sexp，在 org-mode 中会调用
;; org-ctrl-c-ctrl-c 等等。您可以自己定制要激活的命令。

;;; Code:

(defvar evil-quick-sender--normal-map
  (make-hash-table)
  "在 normal 中执行的命令")

(defvar evil-quick-sender--visual-map
  (make-hash-table)
  "在 visual 中执行的命令")

(defun evil-quick-sender ()
  (interactive)
  (let* ((table (cond
                 ((evil-normal-state-p) evil-quick-sender--normal-map)
                 ((evil-visual-state-p) evil-quick-sender--visual-map)
                 (t nil)))
         (func (and table (gethash major-mode table))))
    (when func
      (call-interactively func))))

;;;###autoload
(defun evil-quick-sender-add-command (mode cmd state)
  "在 mode 中按下 s 将执行 cmd，state 有 normal 和 visual 两种。"
  (cond
   ((eql state 'normal)
    (puthash mode cmd evil-quick-sender--normal-map))
   ((eql state 'visual)
    (puthash mode cmd evil-quick-sender--visual-map))
   (t
    (error
     "Only normal or visual state can use `evil-quick-sender-add-command'."))))

(evil-global-set-key 'normal "q" #'evil-quick-sender)
(evil-global-set-key 'visual "q" #'evil-quick-sender)
(evil-global-set-key 'normal "Q" #'evil-record-macro)

(provide 'evil-quick-sender)
;;; evil-quick-sender.el ends here
