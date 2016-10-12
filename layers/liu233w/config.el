(require 'cl-lib)

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

;;add auto format paste code
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;; when save a buffer, the directory is not exsits, it will ask you to create the directory
;; from zilongshanren
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; http://emacs.stackexchange.com/questions/13970/fixing-double-capitals-as-i-type
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

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

(menu-bar-mode t)

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

;;c++缩进
(add-hook 'c++-mode-hook
          '(lambda ()
             (interactive)
             (setq default-tab-width 4)
             (setq-default indent-tabs-mode nil)
             (setq c-basic-offset 4)
             ))

;;When enter the shell buffer, evil state will be switched to emacs-state,
;;C-z can switch between emacs-mode and normal-mode
(add-hook 'shell-mode-hook '(lambda ()
                              (evil-normal-state)
                              (evil-emacs-state)))

;;在dired中使用enter时只使用同一个缓冲区
(put 'dired-find-alternate-file 'disabled nil)
;; 延迟加载
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; (add-hook 'c++-mode-hook '(lambda ()
;;                             (semantic-add-system-include
;;                              "c:/Software/LLVM/include/")))

;; 打开大文件的时候关闭linum，否则速度太慢
;; from https://github.com/zilongshanren/spacemacs-private/blob/develop/layers/zilongshanren-better-defaults/config.el#L132
(defun spacemacs/check-large-file ()
  (when (> (buffer-size) 300000)
    (linum-mode -1)))

(add-hook 'find-file-hook 'spacemacs/check-large-file)

;; 设置用户名和邮箱地址
(setq user-mail-address "wwwlsmcom@outlook.com")
(setq user-full-name "Liu233w")

;; 设置windows底下的company-clang
;; 系统中必须要有 mingw64 ，请自行更改其目录位置
;; (when (spacemacs/system-is-mswindows)
;;   (with-eval-after-load 'company-clang
;;     (require 'nadvice)
;;     (defconst liu233w//company-clang-additional-clang-args-before
;;       (replace-regexp-in-string "\n" " "
;;                                 "--target=x86_64-w64-windows-gnu
;; -I c:/Software/MinGW64/mingw64/include/
;; -I c:/Software/MinGW64/mingw64/lib/gcc/x86_64-w64-mingw32/6.2.0/include
;; -I c:/Software/MinGW64/mingw64/lib/gcc/x86_64-w64-mingw32/6.2.0/include/c++
;; -I c:/Software/MinGW64/mingw64/lib/gcc/x86_64-w64-mingw32/6.2.0/include/c++/backward
;; -I c:/Software/MinGW64/mingw64/lib/gcc/x86_64-w64-mingw32/6.2.0/include/c++/mingw32
;; -I c:/Software/MinGW64/mingw64/lib/gcc/x86_64-w64-mingw32/6.2.0/include-fixed"))
;;     (defconst liu233w//company-clang-additional-clang-args-after
;;       "-lstdc++ -lsupc++")
;;     (advice-add 'company-clang--build-complete-args
;;                 :filter-return
;;                 #'(lambda (args)
;;                     (append
;;                      (list liu233w//company-clang-additional-clang-args-before)
;;                      args
;;                      (list liu233w//company-clang-additional-clang-args-after))))))

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

(with-eval-after-load 'emacs-lisp-mode
  (defun liu233w/eval-buffer-with-message ()
    (interactive)
    (command-execute #'eval-buffer)
    (message "Eval finished"))
  (with-eval-after-load 'emacs-lisp-mode
      (spacemacs/set-leader-keys-for-major-mode
        'emacs-lisp-mode
        "e b" #'liu233w/eval-buffer-with-message))
  ;;
  ;; emacs-lisp-mode下的quick-sender
  (require 'evil-quick-sender)
  (defun liu233w/evil-quick-sender-eval-last-sexp ()
    "在normal state 下eval 光标后面的点")
  (evil-quick-sender-add-command
   'emacs-lisp-mode
   (evil-quick-sender-as-state-send #'eval-last-sexp)
   'normal)
  (evil-quick-sender-add-command 'emacs-lisp-mode 'eval-region 'visual))
