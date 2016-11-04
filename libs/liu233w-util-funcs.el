;;; liu233w-util-funcs.el --- 一些零散的函数  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Liu233w

;; Author: Liu233w  <wwwlsmcom@outlook.com>
;; Keywords: lisp

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

;; 这里保存一些零散的函数，这些函数不是用来增强emacs功能的，而是用来辅助编写elisp的

;;; Code:

;;;###autoload
(defmacro liu233w|bind-keys (binding-list &rest func-and-args)
  "从列表自动生成多个键绑定命令
语法为：
\(liu233w|bind-keys ((\"mn\" 'func1) (\"mp\" 'func2))
                       define-key evil-visual-state-map)
键绑定会自动添加，不会自动调用kbd。这个宏会生成多个键绑定函数的调用，
每次都使用binding-list中的一项（去掉括号）放在函数调用的最后。
除了binding-list以外，请使用和直接调用键绑定函数时相同的语法"
  `(progn
     ,@(mapcar (lambda (item)
                   (append func-and-args item))
               binding-list)))

;;; from evil-plist-delete
;;;###autoload
(defun plist-delete (plist prop)
  "Delete by side effect the property PROP from PLIST.
If PROP is the first property in PLIST, there is no way
to remove it by side-effect; therefore, write
\(setq foo (evil-plist-delete foo :prop)) to be sure of
changing the value of `foo'."
  (let ((tail plist) elt head)
    (while tail
      (setq elt (car tail))
      (cond
       ((eq elt prop)
        (setq tail (cdr (cdr tail)))
        (if head
            (setcdr (cdr head) tail)
          (setq plist tail)))
       (t
        (setq head tail
              tail (cdr (cdr tail))))))
    plist))

;;;###autoload
(defmacro run-the-form (form)
  "form 必须返回一个列表。对form 求值一次，将得到的列表做为代码放进progn 中。

比如：
\(run-the-form
 \(mapcar \(lambda \(a) `\(+ ,a ,a)) '\(1 2)))
会展开成：
\(progn
  \(+ 1 1)
  \(+ 2 2))"
  (let ((res (eval form)))
    `(progn
       ,@res)))

;;;###autoload
(defmacro code-list (list &rest body)
  "list 是一个有两项的列表，对第二项求值一次得到一个列表，然后将第一项做为符号
分别绑定到列表的每一项中，返回一个以progn 打头的代码块。

比如：
\(code-list \(a '\(1 2))
  \(+ a a))
会展开成：
\(progn \(progn \(+ 1 1)) \(progn \(+ 2 2)))"
  (declare (indent defun))
  (let* ((lst (eval (second list)))
         (symb (first list))
         (form (mapcar
                (lambda (lst-symb)
                    (cl-subst lst-symb symb (cons 'progn body)))
                lst)))
    `(progn ,@form)))

(provide 'liu233w-util-funcs)
;;; liu233w-util-funcs.el ends here
