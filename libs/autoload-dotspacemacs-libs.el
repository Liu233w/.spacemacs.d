
;;;### (autoloads nil "liu233w-util-funcs" "liu233w-util-funcs.el"
;;;;;;  (22556 14416 0 0))
;;; Generated autoloads from liu233w-util-funcs.el

(autoload 'liu233w|bind-keys "liu233w-util-funcs" "\
从列表自动生成多个键绑定命令
语法为：
\(liu233w|bind-keys ((\"mn\" 'func1) (\"mp\" 'func2))
                       define-key evil-visual-state-map)
键绑定会自动添加，不会自动调用kbd。这个宏会生成多个键绑定函数的调用，
每次都使用binding-list中的一项（去掉括号）放在函数调用的最后。
除了binding-list以外，请使用和直接调用键绑定函数时相同的语法

\(fn BINDING-LIST &rest FUNC-AND-ARGS)" nil t)

(autoload 'plist-delete "liu233w-util-funcs" "\
Delete by side effect the property PROP from PLIST.
If PROP is the first property in PLIST, there is no way
to remove it by side-effect; therefore, write
\(setq foo (evil-plist-delete foo :prop)) to be sure of
changing the value of `foo'.

\(fn PLIST PROP)" nil nil)

(autoload 'run-the-form "liu233w-util-funcs" "\
form 必须返回一个列表。对form 求值一次，将得到的列表做为代码放进progn 中。

比如：
\(run-the-form
 (mapcar (lambda (a) `(+ ,a ,a)) '(1 2)))
会展开成：
\(progn
  (+ 1 1)
  (+ 2 2))

\(fn FORM)" nil t)

(autoload 'code-list "liu233w-util-funcs" "\
list 是一个有两项的列表，对第二项求值一次得到一个列表，然后将第一项做为符号
分别绑定到列表的每一项中，返回一个以progn 打头的代码块。

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
;;;;;;  (22556 13848 0 0))
;;; Generated autoloads from evil-quick-sender.el

(autoload 'evil-quick-sender-add-command "evil-quick-sender" "\
在mode中按下s将执行cmd，state有normal和visual两种。

\(fn MODE CMD STATE)" nil nil)

;;;***

;;;### (autoloads nil "multiple-micro-state" "multiple-micro-state.el"
;;;;;;  (22556 10491 0 0))
;;; Generated autoloads from multiple-micro-state.el

(autoload 'mms|define-multiple-micro-state "multiple-micro-state" "\
使用`spacemacs|define-micro-state'来生成micro-state，同时对每个命令生成一个函数，
调用函数会执行相应的命令并进入micro-state。

参数列表详见`spacemacs|define-micro-state'
对于micro-state的`:doc'参数如果传入auto，则根据键绑定和命令名自动生成doc。
额外的参数`:with-full-arguments'默认为nil，如果置为t，则生成的函数将拥有和
原函数同样的interactive和参数列表，但是在宏展开的时候原函数的定义必须完全加载
完成；如果为nil，生成的函数则不接受参数，只有一个无参数的interactive，会使用
`command-execute'来执行原来的函数。

\(fn NAME &rest PROPS)" nil t)

;;;***


(provide 'autoload-dotspacemacs-libs)
