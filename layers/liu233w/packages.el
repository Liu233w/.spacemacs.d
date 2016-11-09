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
    ace-mc
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
    chinese-pyim-greatdict
    chinese-pyim-basedict
    chinese-pyim
    evil-find-char-pinyin
    (python :location built-in)
    (dubcaps-mode :location local)
    (cc-mode :location built-in)
    (auto-clang-format :location local)
    (dired :location built-in)
    (linum :location built-in)
    (emacs-lisp :location built-in)
    (word-count-for-chinese :location local)
    (run-current-file :location local)
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
    (liu233w|bind-keys
     (((kbd "mn") #'liu233w/emc:evil-mc-make-and-goto-next-match-then-enter-micro-state)
      ((kbd "mp") #'liu233w/emc:evil-mc-make-and-goto-prev-match-then-enter-micro-state))
     define-key evil-visual-state-map)
    (define-key evil-visual-state-map (kbd "ma")
      'evil-mc-make-all-cursors))

  (with-eval-after-load 'evil-mc
    ;; 设置在evil-mc之下可以执行的命令，主要是删除操作
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

;;     ;; 未完成，目前会在添加新cursors的时候询问是否在多个cursor上执行此命令
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
  "用来代替paredit"
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
  ;;首先赋值参数，防止本package覆盖s键位
  (spacemacs|use-package-add-hook evil-cleverparens
    :post-init
    (when (configuration-layer/layer-usedp 'evil-snipe)
      (setq evil-snipe-auto-disable-substitute t))))

(defun liu233w/init-ace-mc ()
  (use-package ace-mc
    :defer t
    :init
    (define-key evil-visual-state-map (kbd "mm") 'ace-mc-add-multiple-cursors)
    (define-key evil-visual-state-map (kbd "ms") 'ace-mc-add-single-cursor)))

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
              (lambda () (linum-mode t)))))

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
  ;; 在删除marks之后重置evil-visual-mark-mode来自动删除之前的标记
  (advice-add #'evil-delete-marks :after
              (lambda (&rest _)
                (evil-visual-mark-render))))

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
  "tiny自动扩展范围
网址：http://oremacs.com/2014/12/26/the-little-package-that-could/"
  (use-package tiny
    :defer t
    :init
    (defun liu233w/tiny-expand-with-undo ()
      "带undo的tiny-expand。在insert-state时直接调用进行扩展时可以undo
回扩展之前的状态而不会直接清除扩展语句；在normal-state中将光标放在扩展语句
的最后一个字母上调用此函数即可扩展；其他state下与tiny-expand一致。"
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
  "web-mode会与smartparents冲突，表现为在HTML模板中输入两个`{'的时候如果
接着输入空格，会在右侧出现三个大括号，这里是解决方法：http://web-mode.org"
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
  "覆盖Chinese-layer中的设置。默认关闭pangu-spacing，只有在buffer比较小的时候才启动，
如果是启动之后再关闭的话就开的太慢了。"
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
    ))

(defun liu233w/init-number-lock ()
  "启动number-lock，详见 https://github.com/Liu233w/number-lock.el"
  (use-package number-lock
    :init
    (spacemacs/set-leader-keys
      "otn" #'number-lock-toggle-number-lock)
    :config
    (require 'number-lock))
  )

(defun liu233w/init-flycheck-package ()
  "可以检查package的编码是否符合规范。"
  (use-package flycheck-package
    :defer t
    :init
    (eval-after-load 'flycheck
      '(flycheck-package-setup))))

(defun liu233w/pre-init-skewer-mode ()
  "交互式js, 不是node"
  (spacemacs|use-package-add-hook skewer-mode
    :post-config
    (evil-quick-sender-add-command
     'js2-mode
     (liu233w/get-command-with-evil-state #'skewer-eval-last-expression)
     'normal)
    (evil-quick-sender-add-command
     'js2-mode #'spacemacs/skewer-eval-region 'visual)))

;; (defun liu233w/init-js-comint ()
;;   "交互式运行Node.js"
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
    (setq-default js2-concat-multiline-strings 'eol)
    (setq-default js2-rebind-eol-bol-keys nil)
    (setq-default js2-auto-indent-p t)

    (setq-default js2-bounce-indent nil)
    (setq-default js-indent-level 4)
    (setq-default js2-basic-offset 4)
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
  "快速运行当前的buffer"
  ;; 缺陷：无法与运行的程序交互
  (use-package "quickrun"
    :defer t
    :init
    (spacemacs/set-leader-keys
      "o q" #'quickrun
      ;; "o k" #'liu233w/quickrun-kill-process-and-window
      )
    :config
    ;; ;; 默认使用shell来打开，从而允许交互
    ;; (setf quickrun/run-in-shell t)

    ;; (defun liu233w/quickrun-kill-process-and-window ()
    ;;   "关闭quickrun打开的buffer"
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
      (command-execute #'pyim-convert-pinyin-at-point))
    (define-key evil-insert-state-map
      (kbd "C-;") #'liu233w/switch-to-pyim-and-convert))

  (with-eval-after-load 'chinese-pyim
    ;; (require 'chinese-pyim-greatdict)
    ;; (chinese-pyim-greatdict-enable)
    (require 'chinese-pyim-basedict)
    (chinese-pyim-basedict-enable)

    ;; 选词框样式
    (if (spacemacs/system-is-mswindows)
        (setq pyim-use-tooltip nil)
      (setq pyim-use-tooltip 'popup)
      (when (spacemacs/system-is-linux)
        (setq x-gtk-use-system-tooltips t)))

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

    ;; 进入helm 的时候自动关闭当前的输入法，退出时自动恢复到之前的状态 =======
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
  "可以使 evil 的f、t操作支持拼音的首字母，而且支持 evil-snipe"
  (use-package evil-find-char-pinyin
    :defer t
    :init
    (with-eval-after-load 'evil-snipe
      (evil-find-char-pinyin-mode +1)
      (evil-find-char-pinyin-toggle-snipe-integration t))
    ))

(defun liu233w/pre-init-python ()
  (spacemacs|use-package-add-hook python
    :post-config
    ;; fix: https://github.com/gregsexton/ob-ipython/issues/28
    (setq python-shell-completion-native-enable nil))
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
    ;;在dired中使用enter时只使用同一个缓冲区
    (put 'dired-find-alternate-file 'disabled nil)
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))
  )

(defun liu233w/pre-init-linum ()
  (spacemacs|use-package-add-hook linum
    :post-config
    ;; 打开大文件的时候关闭linum，否则速度太慢
    ;; from https://github.com/zilongshanren/spacemacs-private/blob/develop/layers/zilongshanren-better-defaults/config.el#L132
    (defun spacemacs/check-large-file ()
      (when (> (buffer-size) *large-buffer-threshold*)
        (linum-mode -1)))
    (add-hook 'find-file-hook 'spacemacs/check-large-file))
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
    ;; emacs-lisp-mode下的quick-sender
    (defun liu233w/evil-quick-sender-eval-last-sexp ()
      "在normal state 下eval 光标后面的点")
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
  "自动保存、编译、执行当前的文件。支持java、cpp、python等等"
  (use-package run-current-file
    :defer t
    :commands (rcf/run-current-file)
    :init
    (spacemacs/set-leader-keys "or" #'rcf/run-current-file)
    ))

;;; packages.el ends here
