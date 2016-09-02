;;; 这里保存一些零散的函数，这些函数不是用来增强emacs功能的，而是用来辅助编写elisp的

(defmacro liu233w|binding-keys (binding-list &rest func-and-args)
  "从列表自动生成多个键绑定命令
语法为：
\(liu233w|binding-keys ((\"mn\" 'func1) (\"mp\" 'func2))
                       define-key evil-visual-state-map)
键绑定会自动添加，不会自动调用kbd。这个宏会生成多个键绑定函数的调用，
每次都使用binding-list中的一项（去掉括号）放在函数调用的最后。
除了binding-list以外，请使用和直接调用键绑定函数时相同的语法"
  `(progn
     ,@(mapcar #'(lambda (item)
                   (append func-and-args item))
               binding-list)))

(provide 'liu233w-util-funcs)
