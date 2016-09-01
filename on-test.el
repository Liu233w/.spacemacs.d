;;; 包含正在测试中的代码，还不能正式开始使用

;;; 用来生成多重micro-state的代码
(defun liu233w//double-list (lst)
  "从形如 (1a 1b 2a 2b ...) 的列表生成形如 ((1a 1b) (2a 2b) ...) 的列表。
请保证列表中含有偶数个元素！"
  (let ((nlst nil))
    (loop while lst
          do (push (list (first lst) (second lst)) nlst)
          (setf lst (cddr lst)))
    (nreverse nlst)))

(defun liu233w//generate-document-string (bindings)
  "根据参数生成用于 `liu233w|define-multiple-micro-state' 的文档字符串。
参数bindings是一个列表，每一项都是一个含两项的列表，用于生成文档，
生成的文档形如： `键绑定' 命令名 `键绑定' 命令名…… "
  (let* ((first-elt (car bindings))
         (rest-elt (cdr bindings))
         (str (format "`%s' %S" (first first-elt) (second first-elt))))
    (dolist (i rest-elt)
      (setf str (format "%s `%s' %S" str (first i) (second i))))
    str))

(defmacro liu233w|define-multiple-micro-state (name on-enter-form &rest bindings)
  "一次生成多个micro-state，每个都有一个自己的名字。
名字形如`spacemacs/name-命令名-micro-state'，每个micro-state的执行列表和文档
都是相同的，但是各自在启动的时候会执行各自的命令名。
on-enter-form指定传递给命令的参数，形如 (arg1 arg2) 的参数会变成
`spacemacs|define-micro-state'的 :on-enter (命令名 arg1 arg2)"
  (let* ((forms nil)
         (bind-list (liu233w//double-list bindings))
         (doc (liu233w//generate-document-string bind-list)))
    (dolist (i bind-list)
      (push `(spacemacs|define-micro-state
                 ,(intern (format "%S-%S" name (second i)))
               :use-minibuffer t
               :doc ,doc
               :on-enter (,(second i) ,@on-enter-form)
               :bindings
               ,@bind-list)
            forms))
    `(progn
       ,@forms)))

(liu233w|define-multiple-micro-state
 liu233w/view (0)
 "d" evil-scroll-down
 "u" evil-scroll-up
 "f" evil-scroll-page-down
 "b" evil-scroll-page-up)

;; 问题：没法指定scroll的参数了
(evil-global-set-key 'normal (kbd "C-d") 'spacemacs/liu233w/view-evil-scroll-down-micro-state)

;; 一个micro-state ，用来快速翻页
(mms|define-multiple-micro-state liu233w/view
  :use-minibuffer t
  :doc auto
  :bindings
  ("d" evil-scroll-down)
  ("u" evil-scroll-up)
  ("f" evil-scroll-page-down)
  ("b" evil-scroll-page-up))


(defmacro liu233w|binding-keys (binding-list &rest func-and-args)
  "从列表自动生成多个键绑定命令
语法为：
(liu233w|binding-keys (("mn" func1) ("mp" func2))
                      define-key evil-visual-state-map)
键绑定会自动添加"
  `(progn
     ,@(mapcar
        #'())))
