
;;;### (autoloads nil "liu233w-util-funcs" "liu233w-util-funcs.el"
;;;;;;  (22615 39446 0 0))
;;; Generated autoloads from liu233w-util-funcs.el

(autoload 'liu233w|bind-keys "liu233w-util-funcs" "\
从列表自动生成多个键绑定命令。

语法为：
\(liu233w|bind-keys (define-key evil-visual-state-map)
  \"mn\" 'func1
  \"mp\" 'func2)

键绑定会自动添加，不会自动调用 kbd。这个宏会生成多个键绑定函数的调
用，每次都使用 binding-list 中的两项放在函数调用的最后。除了
binding-list 以外，请使用和直接调用键绑定函数时相同的语法

\(fn FUNC-AND-ARG-LIST &rest BINDINGS)" nil t)

(function-put 'liu233w|bind-keys 'lisp-indent-function '1)

(autoload 'plist-delete "liu233w-util-funcs" "\
Delete by side effect the property PROP from PLIST.
If PROP is the first property in PLIST, there is no way
to remove it by side-effect; therefore, write
\(setq foo (evil-plist-delete foo :prop)) to be sure of
changing the value of `foo'.

\(fn PLIST PROP)" nil nil)

(autoload 'run-the-form "liu233w-util-funcs" "\
form 必须返回一个列表。对 form 求值一次，将得到的列表做为代码放进 progn 中。

比如：
\(run-the-form
 (mapcar (lambda (a) `(+ ,a ,a)) '(1 2)))
会展开成：
\(progn
  (+ 1 1)
  (+ 2 2))

\(fn FORM)" nil t)

(autoload 'code-list "liu233w-util-funcs" "\
list 是一个有两项的列表，在编译时对第二项求值一次得到一个列表，
然后将第一项做为符号分别绑定到列表的每一项中，返回一个以 progn 打
头的代码块。

比如：
\(code-list (a '(1 2))
  (+ a a))
会展开成：
\(progn (progn (+ 1 1)) (progn (+ 2 2)))

\(fn LIST &rest BODY)" nil t)

(function-put 'code-list 'lisp-indent-function 'defun)

(autoload 'liu233w/get-command-with-evil-state "liu233w-util-funcs" "\
将函数包装成调用 `liu233w//command-with-evil-state' 的模式

\(fn FUNC)" nil nil)

;;;***

;;;### (autoloads nil "evil-quick-sender" "evil-quick-sender.el"
;;;;;;  (22581 22031 0 0))
;;; Generated autoloads from evil-quick-sender.el

(autoload 'evil-quick-sender-add-command "evil-quick-sender" "\
在 mode 中按下 s 将执行 cmd，state 有 normal 和 visual 两种。

\(fn MODE CMD STATE)" nil nil)

;;;***


;;;### (autoloads nil "multiple-micro-state" "multiple-micro-state.el"
;;;;;;  (22581 22031 0 0))
;;; Generated autoloads from multiple-micro-state.el

(autoload 'mms|define-multiple-micro-state "multiple-micro-state" "\
使用`spacemacs|define-micro-state'来生成 micro-state，同时对每
个命令生成一个函数，调用函数会执行相应的命令并进入 micro-state。

参数列表详见`spacemacs|define-micro-state'

对于 micro-state 的`:doc'参数如果传入 auto，则根据键绑定和命令名
自动生成 doc。额外的参数`:with-full-arguments'默认为 nil，如果置
为 t，则生成的函数将拥有和原函数同样的 interactive 和参数列表，但
是在宏展开的时候原函数的定义必须完全加载完成；如果为 nil，生成的
函数则不接受参数，只有一个无参数的 interactive，会使用
`command-execute'来执行原来的函数。

\(fn NAME &rest PROPS)" nil t)

;;;***

(provide 'autoload-dotspacemacs-libs)
