;; 设置窗口大小
(liu233w/reset-frame-size)
;; 在这里学到：https://github.com/syl20bnr/spacemacs/issues/4365 @raawaa
;; 可以使用这个 hook 来在加载 frame 之后调用制定的函数，frame 将作为参数被传递给
;; 这个函数
(add-hook 'after-make-frame-functions 'liu233w/reset-frame-size)

(display-time-mode 1)
(setq display-time-24hr-format t)
;;显示时间的格式
(setq display-time-format "%H:%M")

;; 双窗口模式显示 gdb
(setq gdb-many-windows nil)

;; when save a buffer, the directory is not exsits, it will ask you to create
;; the directory from zilongshanren
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; Don’t ask me when close emacs with process is running
;; from zilongshanren
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

;; Don’t ask me when kill process buffer
;; from zilongshanren
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

(with-eval-after-load 'evil
  (defun override-evil-delete-marks (marks &optional force)
    "覆盖原函数以修复 bug。

在 https://bitbucket.org/lyro/evil/pull-requests/72/bugfix/diff
被合并之前会一直留在这里。"
    (cond
     ;; delete local marks except 0-9
     (force
      (setq evil-markers-alist
            (cl-delete-if (lambda (m)
                            (not (and (>= (car m) ?0) (<= (car m) ?9))))
                          evil-markers-alist)))
     (t
      (let ((i 0)
            (n (length marks))
            delmarks)
        (while (< i n)
          (cond
           ;; skip spaces
           ((= (aref marks i) ?\ ) (cl-incf i))
           ;; ranges of marks
           ((and (< (+ i 2) n)
                 (= (aref marks (1+ i)) ?-)
                 (or (and (>= (aref marks i) ?a)
                          (<= (aref marks i) ?z)
                          (>= (aref marks (+ 2 i)) ?a)
                          (<= (aref marks (+ 2 i)) ?z))
                     (and (>= (aref marks i) ?A)
                          (<= (aref marks i) ?Z)
                          (>= (aref marks (+ 2 i)) ?A)
                          (<= (aref marks (+ 2 i)) ?Z))))
            (let ((m (aref marks i)))
              (while (<= m (aref marks (+ 2 i)))
                (push m delmarks)
                (cl-incf m)))
            (cl-incf i 2))
           ;; single marks
           (t
            (push (aref marks i) delmarks)
            (cl-incf i))))
        ;; now remove all marks
        (setq evil-markers-alist
              (cl-delete-if (lambda (m) (member (car m) delmarks))
                            evil-markers-alist))
        (set-default 'evil-markers-alist
                     (cl-delete-if (lambda (m) (member (car m) delmarks))
                                   (default-value 'evil-markers-alist)))))))
  (advice-add #'evil-delete-marks :override
              #'override-evil-delete-marks)
  )

;;; from http://endlessparentheses.com/improving-page-navigation.html
(defun liu233w/ad-focus-top-of-page (&rest _)
  "Recenter to page start."
  (when (called-interactively-p 'any)
    (recenter 5)))
;; Requires Emacs 24.5
(advice-add #'backward-page :after
            #'liu233w/ad-focus-top-of-page)
(advice-add #'forward-page  :after
            #'liu233w/ad-focus-top-of-page)

;;; 设置 custom layout
(with-eval-after-load 'persp-mode
  (spacemacs|define-custom-layout "@personal-config"
    :binding "p"
    :body
    (lexical-let* ((layer-name '("liu233w" "org-config"))
                   (file-list '("~/.spacemacs.d/init.el"
                                "~/.spacemacs.d/layers/%s/packages.el"
                                "~/.spacemacs.d/layers/%s/config.el"
                                "~/.spacemacs.d/layers/%s/keybindings.el"
                                "~/.spacemacs.d/layers/%s/funcs.el"))
                   (results (-table-flat
                             (lambda (file layer) (format file layer))
                             file-list
                             layer-name)))
      (dolist (file results)
        (condition-case nil ; 如果文件不存在，不抛出异常
            (find-file-existing file)
          (error nil)))
      results)))
