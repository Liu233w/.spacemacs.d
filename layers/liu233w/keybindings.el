;; 光标在行首时注释此行
;; 说明见 http://cmdblock.blog.51cto.com/415170/557978/
(defun liu233w/qiang-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is
selected and current line is not blank and we are not at the end
of the line, then comment current line. Replaces default
behaviour of comment-dwim, when it inserts comment at the end of
the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'liu233w/qiang-comment-dwim-line)

;; 使用 F10 在当前 buffer 和 shell 之间来回切换
(defun liu233w/toggle-shell ()
  "For emacs 25：如果当前 buffer 不是 shell，则打开一个新的
window 并切换到 shell。否则关闭当前的 window。"
  (interactive)
  (if (string-equal (buffer-name) "*shell*")
      (delete-window)
    (shell)))
(global-set-key (kbd "<f10>") 'liu233w/toggle-shell)

;; 在插入模式下使用 C-O 来在光标前插入一个换行符
(define-key evil-insert-state-map (kbd "C-S-o") 'open-line)
;; 在普通模式下在下一行增加一个空行，但光标不移动也不进入插入模式
(define-key evil-normal-state-map (kbd "C-S-o")
  (lambda ()
    (interactive)
    (save-excursion
      (evil-open-below 1)
      (evil-normal-state))))

;; don't need it anymore
;; (evil-leader/set-key "oe" 'eclim-project-mode)

;; 使用 occur 搜索
(evil-leader/set-key "so" 'occur)

;; 在插入模式下使用 C-d 删除光标后的内容（<delete>）
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)

;; from zilongshanren
(global-set-key (kbd "C-S-y") 'helm-show-kill-ring)

;; ;; set C-h to backspace while C-H is help
;; (define-key key-translation-map (kbd "C-S-h") (kbd "C-h"))
;; (define-key key-translation-map (kbd "C-h") (kbd "DEL"))

;; (with-eval-after-load 'helm
;;   ;; 在 helm 的 find-files 模式中使用 C-u 跳到上一级目录，C-h 如上仍然是退格
;;   ;; C-<return>可以做 TAB 一样的功能，配合 spacemacs 中的 C-j、C-k 上下移动光标，
;;   ;; 可以左手一直按住 ctrl，右手负责移动
;;   (define-key helm-find-files-map (kbd "C-u") 'helm-find-files-up-one-level)
;;   (define-key helm-find-files-map (kbd "C-<return>") 'helm-execute-persistent-action)
;; )

(evil-ex-define-cmd "quit" 'liu233w/ex-kill-buffer-and-close)
(evil-ex-define-cmd "wq" 'liu233w/ex-save-kill-buffer-and-close)

;; 在 visual state 之下按 v 后会进入 expand-region 模式
(define-key evil-visual-state-map (kbd "v") 'er/expand-region)

(global-set-key (kbd "C-c DEL") 'hungry-delete-backward)

;; 一个 micro-state，用来快速翻页
(with-eval-after-load 'evil
  (mms|define-multiple-micro-state
   liu233w/view
   :use-minibuffer t
   :doc "`d' scroll-down `u' scroll-up `f' scroll-page-down `b' scroll-page-up"
   :with-full-arguments t
   :bindings
   ("d" evil-scroll-down)
   ("u" evil-scroll-up)
   ("f" evil-scroll-page-down)
   ("b" evil-scroll-page-up))

  (liu233w|bind-keys (evil-global-set-key 'normal)
    (kbd "C-f") 'liu233w/view:evil-scroll-page-down-then-enter-micro-state
    (kbd "C-b") 'liu233w/view:evil-scroll-page-up-then-enter-micro-state
    (kbd "C-u") 'liu233w/view:evil-scroll-up-then-enter-micro-state
    (kbd "C-d") 'liu233w/view:evil-scroll-down-then-enter-micro-state
    )

  (mms|define-multiple-micro-state
   liu233w/page
   :use-minibuffer t
   :doc "`\[' backword-page `\]' forward-page"
   :with-full-arguments t
   :bindings
   ("[" backward-page)
   ("]" forward-page))

  (dolist (state '(normal visual))
    (liu233w|bind-keys (evil-global-set-key state)
      (kbd "g [") #'liu233w/page:backward-page-then-enter-micro-state
      (kbd "g ]") #'liu233w/page:forward-page-then-enter-micro-state))

  (mms|define-multiple-micro-state
   liu233w/other
   :use-minibuffer t
   :doc auto
   :with-full-arguments t
   :bindings
   ("f" scroll-other-window)
   ("b" scroll-other-window-down))

  (dolist (state '(normal visual insert))
    (liu233w|bind-keys (evil-global-set-key state)
      (kbd "C-S-f") #'liu233w/other:scroll-other-window-then-enter-micro-state
      (kbd "C-S-b") #'liu233w/other:scroll-other-window-down-then-enter-micro-state))
  )

;;; 求值一个表达式并打印结果
(define-key emacs-lisp-mode-map (kbd "C-x j")
  (liu233w/get-command-with-evil-state #'eval-print-last-sexp))

;;; 求值表达式并用结果替换原来的表达式
;;; http://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun liu233w/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c j") (liu233w/get-command-with-evil-state
                               #'liu233w/eval-and-replace))

;;; 用来快速定位 elisp 代码的快捷键
;;; from https://github.com/emacs-china/hello-emacs/blob/master/Emacs_Redux/%E5%BF%AB%E9%80%9F%E5%AE%9A%E4%BD%8DEmacs_Lisp%E6%BA%90%E4%BB%A3%E7%A0%81.org
(dolist (state '(insert normal))
  (liu233w|bind-keys (evil-global-set-key state)
    (kbd "C-h C-l") #'find-library
    (kbd "C-h C-v") #'find-variable
    (kbd "C-h C-f") #'find-function))
