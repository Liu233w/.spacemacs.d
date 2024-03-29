;;; packages.el --- liu233w layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <wwwlsmcom@DESKTOP-1V4DLJV>
;; URL: https://github.com/liu233w/.spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `liu233w-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `liu233w/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `liu233w/pre-init-PACKAGE' and/or
;;   `liu233w/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst liu233w-packages
  '(
    ;; multiple-cursors
    evil-mc
    smart-compile
    slime
    paredit
    evil-cleverparens
    ;; ace-mc
    (ace-evil-mc :location
                 (recipe
                  :fetcher github
                  :repo liu233w/ace-evil-mc))
    evil-vimish-fold
    ahk-mode
    fcitx
    evil-visual-mark-mode
    visual-regexp-steroids
    tiny
    web-mode
    pangu-spacing
    number-lock
    flycheck-package
    ;; js-comint
    skewer-mode
    js2-mode
    pug-mode
    quickrun
    pyim-greatdict
    pyim-basedict
    pyim
    evil-find-char-pinyin
    (python :location built-in)
    (dubcaps-mode :location local)
    (cc-mode :location built-in)
    (auto-clang-format :location local)
    (dired :location built-in)
    nlinum
    (emacs-lisp :location built-in)
    (word-count-for-chinese :location local)
    (run-current-file :location local)
    try
    aggressive-indent
    focus
    vline
    electric-operator
    tern
    helm-pages
    (org-slides-mode :location local)
    (multi-keys :location
                (recipe
                 :fetcher github
                 :repo "Liu233w/multi-keys.el"))
    (elisp-format :location
                  (recipe
                   :fetcher github
                   :repo "Yuki-Inoue/elisp-format"))
    vue-mode
    sml-mode
    )
  "The list of Lisp packages required by the liu233w layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun liu233w/init-smart-compile ()
  (use-package smart-compile
    :defer t
    :init (spacemacs/set-leader-keys "cs" 'smart-compile)))

(defun liu233w/pre-init-evil-mc ()
  (spacemacs|use-package-add-hook evil-mc
    :post-init
    (add-hook 'prog-mode-hook 'evil-mc-mode)
    (add-hook 'text-mode-hook 'evil-mc-mode)

    ;; 快捷键设置：在 visual 模式之下绑定到 m 键
    (mms|define-multiple-micro-state
     liu233w/emc
     :doc auto
     :use-minibuffer t
     :bindings
     ("n" evil-mc-make-and-goto-next-match)
     ("j" evil-mc-skip-and-goto-next-match)
     ("p" evil-mc-make-and-goto-prev-match)
     ("k" evil-mc-skip-and-goto-prev-match))
    (liu233w|bind-keys (define-key evil-visual-state-map)
      (kbd "mn") #'liu233w/emc:evil-mc-make-and-goto-next-match-then-enter-micro-state
      (kbd "mp") #'liu233w/emc:evil-mc-make-and-goto-prev-match-then-enter-micro-state)
    (define-key evil-visual-state-map (kbd "ma")
      'evil-mc-make-all-cursors))

  (with-eval-after-load 'evil-mc
    ;; 设置在 evil-mc 之下可以执行的命令，主要是删除操作
    (setq
     evil-mc-custom-known-commands
     '((paredit-backward-delete . ((:default . evil-mc-execute-default-call-with-count)))
       (paredit-doublequote . ((:default . evil-mc-execute-default-call)))
       (paredit-open-round  . ((:default . evil-mc-execute-default-call)))
       (paredit-close-round  . ((:default . evil-mc-execute-default-call)))
       (paredit-open-square  . ((:default . evil-mc-execute-default-call)))
       (paredit-close-square  . ((:default . evil-mc-execute-default-call)))
       (paredit-forward-slurp-sexp . ((:default . evil-mc-execute-default-call-with-count)))
       (paredit-backward-slurp-sexp . ((:default . evil-mc-execute-default-call-with-count)))
       (paredit-forward-barf-sexp . ((:default . evil-mc-execute-default-call-with-count)))
       (paredit-backward-barf-sexp . ((:default . evil-mc-execute-default-call-with-count)))
       (hungry-delete-backward . ((:default . evil-mc-execute-default-call-with-count)))
       (org-delete-backward-char . ((:default . evil-mc-execute-default-call-with-count)))
       )))
  )

;; (defun liu233w/init-multiple-cursors ()
;;   (use-package multiple-cursors
;;     :defer t
;;     :init
;;     (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;     (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;     (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;     (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;     ;; 未完成，目前会在添加新 cursors 的时候询问是否在多个 cursor 上执行此命令
;;     (spacemacs|define-micro-state liu233w/mc-mark-next
;;       :bindings
;;       ("n" mc/mark-next-like-this))

;;     (define-key evil-visual-state-map (kbd "mn")
;;       'spacemacs/liu233w/mc-mark-next-micro-state)
;;     (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this)
;;     :config
;;     (setq mc/list-file "~/.emacs.d/.cache/.mc-lists.el")
;;     ;; multiple-cursors 中执行下列指令时不询问
;;     ;; see https://github.com/syl20bnr/spacemacs/issues/2669
;;     (setq mc/cmds-to-run-for-all
;;           '(
;;             electric-newline-and-maybe-indent
;;             evil-backward-char
;;             evil-delete-char
;;             evil-escape-emacs-state
;;             evil-escape-insert-state
;;             evil-exit-emacs-state
;;             evil-forward-char
;;             evil-insert
;;             evil-next-line
;;             evil-normal-state
;;             evil-previous-line
;;             forward-sentence
;;             kill-sentence
;;             org-self-insert-command
;;             sp-backward-delete-char
;;             sp-delete-char
;;             sp-remove-active-pair-overlay
;;             evil-force-normal-state
;;             ))

;;     (setq mc/cmds-to-run-once
;;           '(
;;             helm-M-x
;;             spacemacs/default-pop-shell
;;             ))

;;     ))

(defun liu233w/pre-init-slime ()
  (spacemacs|use-package-add-hook slime
    :post-config
    ;; 快捷键设置
    (evil-quick-sender-add-command
     'lisp-mode
     (liu233w/get-command-with-evil-state #'slime-eval-last-expression)
     'normal)
    (evil-quick-sender-add-command 'lisp-mode #'slime-eval-region 'visual)
    (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
      "ge" #'slime-edit-definition
      "d" #'slime-documentation)

    ;; 设置默认的 lisp 程序
    (setq inferior-lisp-program
          (cond
           ((spacemacs/system-is-mswindows) "wx86cl64")
           ((spacemacs/system-is-linux) "sbcl")
           (t "sbcl")
           )))
  )

(defun liu233w/init-paredit ()
  (use-package paredit
    :defer t
    :init
    (autoload 'enable-paredit-mode "paredit"
      "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
    (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
    :config
    (setq paredit-lighter "(P)")
    ;; Stop SLIME's REPL from grabbing DEL,
    ;; which is annoying when backspacing over a '('
    (defun override-slime-repl-bindings-with-paredit ()
      (define-key slime-repl-mode-map
        (read-kbd-macro paredit-backward-delete-key) nil))
    (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
    ))

(defun liu233w/post-init-evil-cleverparens ()
  "用来代替 paredit"
  (spacemacs/toggle-evil-cleverparens-on)
  (dolist (i '(emacs-lisp-mode-hook
               eval-expression-minibuffer-setup-hook
               ielm-mode-hook
               lisp-mode-hook
               lisp-interaction-mode-hook
               scheme-mode-hook
               slime-repl-mode-hook
               ))
    (add-hook i (lambda ()
                  (hungry-delete-mode -1)
                  (evil-cleverparens-mode 1))))
  )

(defun liu233w/pre-init-evil-cleverparens ()
  ;;首先赋值参数，防止本 package 覆盖 s 键位
  (spacemacs|use-package-add-hook evil-cleverparens
    :post-init
    (when (configuration-layer/layer-usedp 'evil-snipe)
      (setq evil-snipe-auto-disable-substitute t))))

;; (defun liu233w/init-ace-mc ()
;;   (use-package ace-mc
;;     :defer t
;;     :init
;;     (define-key evil-visual-state-map (kbd "mm") 'ace-mc-add-multiple-cursors)
;;     (define-key evil-visual-state-map (kbd "ms") 'ace-mc-add-single-cursor)))

(defun liu233w/init-ace-evil-mc ()
  (use-package ace-evil-mc
    :defer t
    :init
    (define-key evil-visual-state-map (kbd "mm") 'ace-evil-mc-add-multiple-cursors)
    (define-key evil-visual-state-map (kbd "ms") 'ace-evil-mc-add-single-cursor)))

(defun liu233w/init-evil-vimish-fold ()
  ;;from zilongshanren
  (use-package evil-vimish-fold
    :init
    (vimish-fold-global-mode 1)
    :config
    (define-key evil-normal-state-map (kbd "zf") 'vimish-fold)
    (define-key evil-visual-state-map (kbd "zf") 'vimish-fold)
    (define-key evil-normal-state-map (kbd "zd") 'vimish-fold-delete)
    (define-key evil-normal-state-map (kbd "za") 'vimish-fold-toggle)))

(defun liu233w/init-ahk-mode ()
  (use-package ahk-mode
    :defer t
    :config
    (add-hook 'ahk-mode-hook
              (lambda () (nlinum-mode t)))))

(defun liu233w/post-init-fcitx ()
  "可以在输入快捷键时自动切换输入法"
  (with-eval-after-load 'fcitx
    (setq w32-pass-lwindow-to-system nil)
    (setq w32-lwindow-modifier 'super)
    (setq fcitx-active-evil-states '(insert emacs hybrid)) ; if you use hybrid mode
    (fcitx-aggressive-setup)
    (fcitx-shell-command-turn-on)
    (fcitx-eval-expression-turn-on)
    (fcitx-prefix-keys-add "M-m") ; M-m is common in Spacemacs
    ))

(defun liu233w/pre-init-evil-visual-mark-mode ()
  ;; learn from https://gist.github.com/TheBB/367096660b203952c162
  (spacemacs|use-package-add-hook evil-visual-mark-mode
    :post-init
    (evil-visual-mark-mode))
  (define-advice evil-delete-marks (:after (&rest _))
    "在删除 marks 之后重置 evil-visual-mark-mode 来自动删除之前的标记"
    (evil-visual-mark-render)))

(defun liu233w/init-visual-regexp-steroids ()
  "可视化地使用替换模式"
  (use-package visual-regexp-steroids
    :defer t
    :init
    (define-key global-map (kbd "C-c r") 'vr/replace)
    (define-key global-map (kbd "C-c q") 'vr/query-replace)
    ;; if you use multiple-cursors, this is for you:
    (define-key global-map (kbd "C-c m") 'vr/mc-mark)
    ;; to use visual-regexp-steroids's isearch instead of the built-in regexp
    ;; isearch, also include the following lines:
    (define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
    (define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s

    (spacemacs/declare-prefix "xv" "visual-regexp")
    (spacemacs/set-leader-keys
      "xvr" 'vr/replace
      "xvq" 'vr/query-replace
      "xvm" 'vr/mc-mark)))

(defun liu233w/init-tiny ()
  "tiny 自动扩展范围
网址：http://oremacs.com/2014/12/26/the-little-package-that-could/"
  (use-package tiny
    :defer t
    :init
    (defun liu233w/tiny-expand-with-undo ()
      "带 undo 的 tiny-expand。

在 insert-state 时直接调用进行扩展时可以 undo 回扩展之前的状态而
不会直接清除扩展语句；在 normal-state 中将光标放在扩展语句的最后
一个字母上调用此函数即可扩展；其他 state 下与 tiny-expand 一致。"
      (interactive)
      (cond
       ((evil-insert-state-p)
        (evil-normal-state)
        (evil-append 0)
        (tiny-expand))
       ((evil-normal-state-p)
        (evil-append 0)
        (tiny-expand)
        (evil-normal-state))
       (t
        (tiny-expand))))
    ;;
    (global-set-key (kbd "C-c C-;") 'liu233w/tiny-expand-with-undo)
    (spacemacs/set-leader-keys "oe" 'liu233w/tiny-expand-with-undo)
    ))

(defun liu233w/pre-init-web-mode ()
  "web-mode 会与 smartparents 冲突，表现为在 HTML 模板中输入两个
`{'的时候如果接着输入空格，会在右侧出现三个大括号，这里是解决方法：
http://web-mode.org"
  ;; (defun my-web-mode-hook ()
  ;;   (setq web-mode-enable-auto-pairing nil))

  ;; (add-hook 'web-mode-hook  'my-web-mode-hook)

  ;; (defun sp-web-mode-is-code-context (id action context)
  ;;   (and (eq action 'insert)
  ;;        (not (or (get-text-property (point) 'part-side)
  ;;                 (get-text-property (point) 'block-side)))))

  ;; (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context)))
  (spacemacs|use-package-add-hook web-mode
    :post-config
    (add-hook 'web-mode-hook (lambda ()
                               (turn-off-smartparens-mode)
                               (setq web-mode-enable-auto-pairing t))))
  )

(defun liu233w/init-pangu-spacing ()
  "覆盖 Chinese-layer 中的设置，并提供一些新功能。

默认关闭 pangu-spacing，只有在 buffer 比较小的时候才启动，如果是
启动之后再关闭的话就开的太慢了。"
  (use-package pangu-spacing
    :config
    (global-pangu-spacing-mode -1)
    (spacemacs|hide-lighter pangu-spacing-mode)
    ;; Always insert `real' space in org-mode.
    (add-hook 'org-mode-hook
              '(lambda ()
                 (set (make-local-variable
                       'pangu-spacing-real-insert-separtor) t)))

    (defun liu233w/enable-pangu-spacing-when-buffer-not-large ()
      "when buffer is not large, turn on it"
      (when (< (buffer-size) *large-buffer-threshold*)
        (pangu-spacing-mode 1)))

    (dolist (i '(prog-mode-hook text-mode-hook))
      (add-hook i 'liu233w/enable-pangu-spacing-when-buffer-not-large))

    (defun liu233w/pangu-spacing-space-the-buffer ()
      "调用`pangu-spacing-mode'为当前 buffer 分词，不会修改原有的设置。"
      (interactive)
      (let ((pangu-spacing-real-insert-separtor t))
        (pangu-spacing-modify-buffer)))
    (evil-ex-define-cmd "pangu" #'liu233w/pangu-spacing-space-the-buffer)
    ))

(defun liu233w/init-number-lock ()
  "启动 number-lock，详见 https://github.com/Liu233w/number-lock.el"
  (use-package number-lock
    :init
    (spacemacs/set-leader-keys
      "otn" #'number-lock-toggle-number-lock)
    :config
    (require 'number-lock))
  )

(defun liu233w/init-flycheck-package ()
  "可以检查 package 的编码是否符合规范。"
  (use-package flycheck-package
    :defer t
    :init
    (eval-after-load 'flycheck
      '(flycheck-package-setup))))

(defun liu233w/pre-init-skewer-mode ()
  "交互式 js, 不是 node"
  (spacemacs|use-package-add-hook skewer-mode
    :post-config
    (evil-quick-sender-add-command
     'js2-mode
     (liu233w/get-command-with-evil-state #'skewer-eval-last-expression)
     'normal)
    (evil-quick-sender-add-command
     'js2-mode #'spacemacs/skewer-eval-region 'visual)))

;; (defun liu233w/init-js-comint ()
;;   "交互式运行 Node.js"
;;   (use-package js-comint
;;     :defer t
;;     :init
;;     (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "repl")
;;     (spacemacs/set-leader-keys-for-major-mode 'js2-mode
;;       "si" 'run-js
;;       "ss" 'js-send-last-sexp
;;       "sS" 'js-send-last-sexp-and-go
;;       "sb" 'js-send-buffer
;;       "sB" 'js-send-buffer-and-go
;;       "sr" 'js-send-region
;;       "sR" 'js-send-region-and-go)

;;     (evil-quick-sender-add-command
;;      'js2-mode
;;      (liu233w/get-command-with-evil-state #'js-send-last-sexp)
;;      'normal)
;;     (evil-quick-sender-add-command 'js2-mode #'js-send-region 'visual)))

;;; from zilongshanren https://github.com/zilongshanren/spacemacs-private/blob/develop/layers/zilongshanren-programming/packages.el
(defun liu233w/pre-init-js2-mode ()
  (spacemacs|use-package-add-hook js2-mode
    :post-config
    (setq company-backends-js2-mode '((company-dabbrev-code
                                       :with company-keywords company-etags)
                                      company-files company-dabbrev))

    ;; these mode related variables must be in eval-after-load
    ;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-js2-mode.el
    (setq-default js2-allow-rhino-new-expr-initializer nil)
    (setq-default js2-auto-indent-p nil)
    (setq-default js2-enter-indents-newline nil)
    (setq-default js2-global-externs
                  '("module" "require" "buster" "sinon" "assert" "refute"
                    "setTimeout" "clearTimeout" "setInterval" "clearInterval"
                    "location" "__dirname" "console" "JSON"))
    (setq-default js2-idle-timer-delay 0.2)
    (setq-default js2-mirror-mode nil)
    (setq-default js2-strict-inconsistent-return-warning nil)
    (setq-default js2-include-rhino-externs nil)
    (setq-default js2-include-gears-externs nil)
    (setq-default js2-include-node-externs t)
    (setq-default js2-concat-multiline-strings 'eol)
    (setq-default js2-rebind-eol-bol-keys nil)
    (setq-default js2-auto-indent-p t)

    (setq-default js2-bounce-indent nil)
    (setq-default js-indent-level 2)
    (setq-default js2-basic-offset 2)
    (setq-default js-switch-indent-offset 2)
    ;; Let flycheck handle parse errors
    (setq-default js2-mode-show-parse-errors nil)
    (setq-default js2-mode-show-strict-warnings nil)
    (setq-default js2-highlight-external-variables t)
    (setq-default js2-strict-trailing-comma-warning nil)

    (eval-after-load 'tern-mode
      '(spacemacs|hide-lighter tern-mode))

    (evilified-state-evilify js2-error-buffer-mode js2-error-buffer-mode-map))
  )

(defun liu233w/post-init-pug-mode ()
  (with-eval-after-load 'pug-mode
    (add-hook 'pug-mode-hook (lambda () (smartparens-mode 1)))))

(defun liu233w/init-quickrun ()
  "快速运行当前的 buffer"
  ;; 缺陷：无法与运行的程序交互
  (use-package "quickrun"
    :defer t
    :init
    (spacemacs/set-leader-keys
      "o q" #'quickrun
      ;; "o k" #'liu233w/quickrun-kill-process-and-window
      )
    :config
    ;; ;; 默认使用 shell 来打开，从而允许交互
    ;; (setf quickrun/run-in-shell t)

    ;; (defun liu233w/quickrun-kill-process-and-window ()
    ;;   "关闭 quickrun 打开的 buffer"
    ;;   (interactive)
    ;;   (let* ((buf (get-buffer "*eshell-quickrun*"))
    ;;          (proc (get-buffer-process buf)))
    ;;     (when proc
    ;;       (quickrun/kill-process proc))
    ;;     (bury-buffer buf)
    ;;     (delete-window (get-buffer-window buf))))
    ;; 在运行前询问是否保存
    (advice-add #'quickrun :before #'save-some-buffers)))

(defun liu233w/init-chinese-pyim-greatdict ()
  (use-package chinese-pyim-greatdict
    :defer t
    :ensure nil))
(defun liu233w/init-chinese-pyim-basedict ()
  (use-package chinese-pyim-basedict
    :defer t
    :ensure nil))

(defun liu233w/pre-init-chinese-pyim ()
  (spacemacs|use-package-add-hook chinese-pyim
    :post-init
    (defun liu233w/switch-to-pyim-and-convert ()
      (interactive)
      (set-input-method "chinese-pyim")
      (command-execute #'pyim-convert-code-at-point))
    (evil-global-set-key 'insert
                         (kbd "C-;") #'liu233w/switch-to-pyim-and-convert)
    (evil-global-set-key 'normal
                         (kbd "C-;") (liu233w/get-command-with-evil-state
                                      #'liu233w/switch-to-pyim-and-convert))

    (spacemacs|hide-lighter multi-keys-mode))

  (with-eval-after-load 'chinese-pyim
    ;; (require 'chinese-pyim-greatdict)
    ;; (chinese-pyim-greatdict-enable)
    (require 'chinese-pyim-basedict)
    (chinese-pyim-basedict-enable)

    ;; 选词框样式
    (if (spacemacs/system-is-mswindows)
        (setq-default pyim-page-tooltip nil)
      (setq-default pyim-page-tooltip 'popup)
      (when (spacemacs/system-is-linux)
        (setq-default x-gtk-use-system-tooltips t)))

    ;; 根据上下文来确定当前是否使用中文
    (setq-default pyim-english-input-switch-functions
                  '(pyim-probe-dynamic-english
                    pyim-probe-isearch-mode
                    pyim-probe-program-mode
                    pyim-probe-org-structure-template
                    ))
    (setq-default pyim-punctuation-half-width-functions
                  '(pyim-probe-punctuation-line-beginning
                    pyim-probe-punctuation-after-punctuation))

    ;; 进入 helm 的时候自动关闭当前的输入法，退出时自动恢复到之前的状态 =======
    (defvar liu233w//helm-pyim-switch--last-im
      nil
      "最后切换的输入法")
    (make-variable-buffer-local 'liu233w//helm-pyim-switch--last-im)

    (defun liu233w/helm-pyim-enter ()
      (setq liu233w//helm-pyim-switch--last-im current-input-method)
      (set-input-method nil))
    (defun liu233w/helm-pyim-exit ()
      (set-input-method liu233w//helm-pyim-switch--last-im)
      (setf liu233w//helm-pyim-switch--last-im nil))

    (add-hook 'helm-minibuffer-set-up-hook #'liu233w/helm-pyim-enter)
    (add-hook 'helm-exit-minibuffer-hook #'liu233w/helm-pyim-exit)
    ;; ========================================================================
    ))

(defun liu233w/init-evil-find-char-pinyin ()
  "可以使 evil 的 f、t 操作支持拼音的首字母，而且支持 evil-snipe"
  (use-package evil-find-char-pinyin
    :defer t
    :init
    (with-eval-after-load 'evil-snipe
      (evil-find-char-pinyin-mode +1)
      (evil-find-char-pinyin-toggle-snipe-integration t))
    :config
    (spacemacs|hide-lighter evil-find-char-pinyin-mode)
    ))

(defun liu233w/pre-init-python ()
  (spacemacs|use-package-add-hook python
    :post-config
    ;; fix: https://github.com/gregsexton/ob-ipython/issues/28
    (setq python-shell-completion-native-enable nil)

    (defun liu233w/python-shell-send-line ()
      "send current line to python shell"
      (interactive)
      (python-shell-send-region (line-beginning-position) (line-end-position)))
    (evil-quick-sender-add-command 'python-mode
                                   #'liu233w/python-shell-send-line
                                   'normal)
    (evil-quick-sender-add-command 'python-mode
                                   #'python-shell-send-region
                                   'visual))
  )

(defun liu233w/init-dubcaps-mode ()
  "Convert word in DOuble CApitals to Single Capitals."
  (use-package dubcaps-mode
    :defer t
    :commands (dubcaps-mode)
    ))

(defun liu233w/pre-init-cc-mode ()
  (spacemacs|use-package-add-hook cc-mode
    :post-init
    (add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
    :post-config
    ;;c++缩进
    (add-hook 'c++-mode-hook
              (lambda ()
                (interactive)
                (setq default-tab-width 4)
                (setq-default indent-tabs-mode nil)
                (setq c-basic-offset 4)
                )))
  ;; (add-hook 'c++-mode-hook (lambda ()
  ;;                             (semantic-add-system-include
  ;;                              "c:/Software/LLVM/include/")))

  ;; 设置 windows 底下的 company-clang
  ;; 系统中必须要有 mingw64，请自行更改其目录位置
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
  ;;                 (lambda (args)
  ;;                     (append
  ;;                      (list liu233w//company-clang-additional-clang-args-before)
  ;;                      args
  ;;                      (list liu233w//company-clang-additional-clang-args-after))))))
  )

(defun liu233w/init-auto-clang-format ()
  "在输入分号或右大括号的时候自动排版。"
  (use-package auto-clang-format
    :defer t
    :commands acf-enable-auto-clang-format
    :init
    (add-hook 'c++-mode-hook #'acf-enable-auto-clang-format)
    :config
    (when (spacemacs/system-is-mswindows)
      (defun liu233w//utf-8-dos-as-coding-system-for-write (func &rest rest)
        (let ((coding-system-for-write 'utf-8-dos))
          (apply func rest)))
      (advice-add #'clang-format-region :around
                  #'liu233w//utf-8-dos-as-coding-system-for-write))))

(defun liu233w/pre-init-dired ()
  (with-eval-after-load 'dired
    ;;在 dired 中使用 enter 时只使用同一个缓冲区
    (put 'dired-find-alternate-file 'disabled nil)
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))
  )

(defun liu233w/post-init-nlinum ()
  ;; 打开大文件的时候关闭 nlinum，否则速度太慢
  ;; from https://github.com/zilongshanren/spacemacs-private/blob/develop/layers/zilongshanren-better-defaults/config.el#L132
  (defun liu233w/nlinum-check-large-file ()
    (when (> (buffer-size) *large-buffer-threshold*)
      (nlinum-mode -1)))
  (add-hook 'find-file-hook 'liu233w/nlinum-check-large-file)
  )

(defun liu233w/post-init-emacs-lisp ()
  (with-eval-after-load 'elisp-mode
    (defun liu233w/eval-buffer-with-message ()
      (interactive)
      (command-execute #'eval-buffer)
      (message "Eval finished"))
    (spacemacs/set-leader-keys-for-major-mode
      'emacs-lisp-mode
      "e b" #'liu233w/eval-buffer-with-message)
    ;;
    ;; emacs-lisp-mode 下的 quick-sender
    (defun liu233w/evil-quick-sender-eval-last-sexp ()
      "在 normal state 下 eval 光标后面的点")
    (evil-quick-sender-add-command
     'emacs-lisp-mode
     (liu233w/get-command-with-evil-state #'eval-last-sexp)
     'normal)
    (evil-quick-sender-add-command 'emacs-lisp-mode
                                   #'eval-last-sexp
                                   'visual)

    (defun liu233w/replace-parenthese-in-docstring (beg end)
      "将选区中的`\('替换成`\\\('。主要用在 emacs-lisp 的 docstring 中。"
      (interactive "r")
      (unless (= beg end)
        (query-replace "(" "\\(" nil beg end)))
    ))

(defun liu233w/init-word-count-for-chinese ()
  (use-package word-count-for-chinese
    :defer t
    :commands (wc-word-count-for-chinese)
    ))

(defun liu233w/init-run-current-file ()
  "自动保存、编译、执行当前的文件。支持 java、cpp、python 等等"
  (use-package run-current-file
    :defer t
    :commands (rcf/run-current-file)
    :init
    (spacemacs/set-leader-keys "or" #'rcf/run-current-file)
    ))

(defun liu233w/init-try ()
  "可以不安装插件而是直接尝试它"
  (use-package try
    :defer t))

(defun liu233w/post-init-aggressive-indent ()
  "在每次操作\(输入、删除\)时都自动进行缩进。

owner 是 distribution-layer，因此不能使用 pre-init"
  ;; init
  (dolist (hooks
           '(
             emacs-lisp-mode-hook
             lisp-mode-hook
             web-mode-hook
             c++-mode-hook
             js2-mode-hook
             ))
    (add-hook hooks #'aggressive-indent-mode))
  ;; config
  (with-eval-after-load 'aggressive-indent
    (defun liu233w/disable-aggressive-indent-mode-for-command (cmd &rest rest)
      (let ((aggressive-indent-mode nil))
        (command-execute cmd)))
    (dolist (cmd '(undo-tree-undo
                   undo-tree-redo))
      (advice-add cmd :around
                  #'liu233w/disable-aggressive-indent-mode-for-command))

    (dolist (cmd '(evil-paste-after
                   evil-paste-before
                   evil-paste-pop
                   evil-paste-pop-next))
      (add-to-list 'aggressive-indent-protected-commands cmd))
    ))

(defun liu233w/init-focus ()
  "高亮当前的段落，取消其他段落的高亮"
  (use-package focus
    :defer t
    :init
    (spacemacs/set-leader-keys
      "otf" #'focus-mode)))

(defun liu233w/init-vline ()
  "高亮当前列"
  (use-package vline
    :defer t
    :init
    (spacemacs/set-leader-keys
      "otc" #'vline-mode
      "otC" #'vline-global-mode)
    :config
    ;; 与默认的行高亮的颜色相同
    (set-face-background vline-face "#073642")))

(defun liu233w/init-electric-operator ()
  "在运算符两边自动添加空格"
  (use-package electric-operator
    :defer t
    :init
    (dolist (hook '(python-mode-hook js2-mode-hook))
      (add-hook hook #'electric-operator-mode))))

(defun liu233w/pre-init-tern ()
  "for javascript"
  (spacemacs|use-package-add-hook tern
    :post-config
    (defvar liu233w/windows-tern-location
      "c:/Users/wwwlsmcom/AppData/Roaming/npm/node_modules/tern/bin/tern")
    (when (spacemacs/system-is-mswindows)
      (setq tern-command (list "node" liu233w/windows-tern-location)))))

(defun liu233w/init-helm-pages ()
  "使用 helm 列出以分页符分隔的段落"
  (use-package helm-pages
    :defer t
    :init
    (spacemacs/set-leader-keys "jp" #'helm-pages)))

(defun liu233w/init-org-slides-mode ()
  "用来演示org，在演示完之后需要手动恢复"
  (use-package org-slides-mode
    :defer t
    :commands (org-slides-mode)
    :config
    (defun liu233w/exit-org-slides-mode ()
      (interactive)
      (org-slides-mode -1)
      (widen)
      (liu233w/set-chinese-fonts))
    (define-key org-slides-mode-keymap
      (kbd "<home>") #'liu233w/exit-org-slides-mode)))

(defun liu233w/init-multi-keys ()
  "代替 key-chord.el"
  (use-package multi-keys
    :config
    (global-multi-keys-mode 1)
    (multi-keys-define-global "kl" #'liu233w/switch-to-pyim-and-convert)
    ))

(defun liu233w/init-elisp-format ()
  "给 elisp 提供自动折行。"
  ;; see https://github.com/Yuki-Inoue/elisp-format
  ;; and https://www.emacswiki.org/emacs/ElispFormat
  (use-package elisp-format
    :defer t))

(defun liu233w/init-vue-mode ()
  (use-package vue-mode
    :mode ("\\.vue\\'" . vue-mode)))

(defun liu233w/pre-init-sml-mode ()
  (spacemacs|use-package-add-hook sml-mode
    :post-init
    (evil-quick-sender-add-command 'sml-mode #'sml-send-function 'normal)
    (evil-quick-sender-add-command 'sml-mode #'sml-prog-proc-send-region 'visual)))

;;; packages.el ends here
