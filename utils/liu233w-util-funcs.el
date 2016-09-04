;;; 这里保存一些零散的函数，这些函数不是用来增强emacs功能的，而是用来辅助编写elisp的

(defmacro liu233w|bind-keys (binding-list &rest func-and-args)
  "从列表自动生成多个键绑定命令
语法为：
\(liu233w|bind-keys ((\"mn\" 'func1) (\"mp\" 'func2))
                       define-key evil-visual-state-map)
键绑定会自动添加，不会自动调用kbd。这个宏会生成多个键绑定函数的调用，
每次都使用binding-list中的一项（去掉括号）放在函数调用的最后。
除了binding-list以外，请使用和直接调用键绑定函数时相同的语法"
  `(progn
     ,@(mapcar #'(lambda (item)
                   (append func-and-args item))
               binding-list)))

;;; from evil-plist-delete
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

(provide 'liu233w-util-funcs)
