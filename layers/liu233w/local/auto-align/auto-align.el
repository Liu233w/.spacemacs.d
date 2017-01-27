;;; auto-align.el --- 在键入分号或右大括号时自动调用内置的 align 函数进行格式化  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Liu233w

;; Author: Liu233w <wwwlsmcom@outlook.com>
;; Keywords: convenience, tools

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

;; 在键入分号或右大括号时自动调用内置的 align 函数进行格式化。

;;; Code:

;;;###autoload
(defun auto-align-semi ()
  "Format current line using `align' when press ';'"
  (interactive)
  (command-execute #'c-electric-semi&comma)
  (align (line-beginning-position 0)
         (line-beginning-position 2)))

;;;###autoload
(defun auto-align-brace ()
  "Format the region using `align' when press '}'"
  (interactive)
  (command-execute #'c-electric-brace)
  (let ((end-position (point))
        (begin-position (scan-lists (point) -1 0)))
    (align begin-position
           end-position)))

;;;###autoload
(defun auto-align-setup ()
  "启动 auto-align"
  (interactive)
  ;;当插入分号时自动对当前行排版
  (local-set-key (kbd ";")
                 'auto-align-semi)
  (local-set-key (kbd "}")
                 'auto-align-brace))

(provide 'auto-align)
;;; auto-align.el ends here
