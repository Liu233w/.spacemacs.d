;;; 设置编码
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(when (spacemacs/system-is-mswindows)
  (set-language-environment "chinese-gbk")
  (set-default 'process-coding-system-alist
               '(("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos)
                 ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos)))
  (defun liu233w//python-encode-in-org-babel-execute (func body params)
    "org-babel 执行代码时不会自动编码文件，这里通过动态作用域覆盖默认选项来编码文件。"
    ;; 此问题的详细信息请参考： https://github.com/Liu233w/.spacemacs.d/issues/6
    (let ((coding-system-for-write 'utf-8))
      (funcall func body params)))
  (advice-add #'org-babel-execute:python :around
              #'liu233w//python-encode-in-org-babel-execute))

;;设置窗口大小
(liu233w/reset-frame-size)
;;在这里学到： https://github.com/syl20bnr/spacemacs/issues/4365 @raawaa
;;可以使用这个hook来在加载frame之后调用制定的函数，frame将作为参数被传递给
;;这个函数
(add-hook 'after-make-frame-functions 'liu233w/reset-frame-size)

(display-time-mode 1)
(setq display-time-24hr-format t)
;;显示时间的格式
(setq display-time-format "%H:%M")

;;双窗口模式显示gdb
(setq gdb-many-windows nil)

;; when save a buffer, the directory is not exsits, it will ask you to create the directory
;; from zilongshanren
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;;Don’t ask me when close emacs with process is running
;;from zilongshanren
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

;;Don’t ask me when kill process buffer
;;from zilongshanren
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; 设置用户名和邮箱地址
(setq user-mail-address "wwwlsmcom@outlook.com")
(setq user-full-name "Liu233w")

;;; from http://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close
(defun liu233w/bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (delete-window (get-buffer-window buf)))
                      buffer)))
(add-hook 'compilation-finish-functions
          #'liu233w/bury-compile-buffer-if-successful
          t)
