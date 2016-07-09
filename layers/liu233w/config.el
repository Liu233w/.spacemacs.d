(require 'cl-lib)

;; For my language code setting (UTF-8)
(set-language-environment "chinese-GBK")
(prefer-coding-system 'utf-8)

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
