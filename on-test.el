;;; 包含正在测试中的代码，还不能正式开始使用

;;; 便于我写python讲义的函数
(defun liu233w/py-acm-insert-src-block ()
  (interactive)
  (save-excursion
    (insert "#+begin_src python :exports both
#+end_src")))
(evil-ex-define-cmd "pys" #'liu233w/py-acm-insert-src-block)
