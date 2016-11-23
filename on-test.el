;;; 包含正在测试中的代码，还不能正式开始使用

;;; 用来快速定位 elisp 代码的快捷键
;;; from https://github.com/emacs-china/hello-emacs/blob/master/Emacs_Redux/%E5%BF%AB%E9%80%9F%E5%AE%9A%E4%BD%8DEmacs_Lisp%E6%BA%90%E4%BB%A3%E7%A0%81.org
(dolist (state '(insert normal))
  (evil-global-set-key state (kbd "C-h C-l") #'find-library)
  (evil-global-set-key state (kbd "C-h C-v") #'find-variable)
  (evil-global-set-key state (kbd "C-h C-f") #'find-function))
