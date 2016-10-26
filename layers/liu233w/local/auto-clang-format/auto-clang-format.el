;;; auto-clang-format.el --- 在键入分号或右大括号的时候自动调用 clang-format 进行排版  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Liu233w

;; Author: Liu233w <wwwlsmcom@outlook.com>
;; Keywords: languages, convenience

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

;; 在键入分号或右大括号的时候自动调用 clang-format 进行排版

;;; Code:

(require 'clang-format)

(defvar acf-clang-format-style
  "{BasedOnStyle: LLVM, IndentWidth: 4, BreakBeforeBraces: Allman,
    AllowShortFunctionsOnASingleLine: false}"
  "默认的style，在找不到默认的style文件时提供")

(defun acf--get-clang-format-config (source)
  "source是源文件地址，为nil时表示没有地址。如果在源文件目录或其上级目录中可以找到
clang-format配置文件，返回\"file\"，否则返回`acf-clang-format-style'的内容。"
  (if (and source (or (locate-dominating-file source ".clang-format")
                      (locate-dominating-file source "_clang-format")))
      "file"
    acf-clang-format-style))

;;;###autoload
(defun acf-semi-clang-format ()
  "format by clang-format when enter ';'"
  (interactive)
  (command-execute #'c-electric-semi&comma)
  (clang-format-region (line-beginning-position 0)
                       (line-beginning-position 2)
                       (acf--get-clang-format-config
                        (buffer-file-name))))

;;;###autoload
(defun acf-brace-clang-format ()
  "format by clang-format when enter '}'"
  (interactive)
  (command-execute #'c-electric-brace)
  (let ((end-position (point))
        begin-position (scan-lists (point) -1 0))
    (clang-format-region begin-position
                         end-position
                         (acf--get-clang-format-config
                          (buffer-file-name)))))

(defvar acf-clang-format-executable
  "clang-format"
  "clang-format 可执行文件的名字。")

;;;###autoload
(defun acf-enable-auto-clang-format ()
  "启动 auto-clang-format"
  (when (executable-find acf-clang-format-executable)
    ;;使用clang-format作为默认排版工具
    (local-set-key (kbd "C-M-\\")
                   #'(lambda (beg end)
                       (interactive "r")
                       (clang-format-region beg end
                                            (acf--get-clang-format-config
                                             (buffer-file-name)))))
    ;;当插入分号时自动对当前行排版
    (local-set-key (kbd ";")
                   'acf-semi-clang-format)
    (local-set-key (kbd "}")
                   'acf-brace-clang-format)))

(provide 'auto-clang-format)
;;; auto-clang-format.el ends here
