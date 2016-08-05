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
    clang-format
    ;multiple-cursors
    evil-mc
    smart-compile
    slime
    paredit
    ace-mc
    evil-vimish-fold
    ahk-mode
    fcitx
    evil-visual-mark-mode
    visual-regexp-steroids
    tiny
    web-mode
    pangu-spacing
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

(defun liu233w/post-init-evil-mc ()
  (add-hook 'prog-mode-hook 'evil-mc-mode)
  (add-hook 'text-mode-hook 'evil-mc-mode)

  ;; 设置在evil-mc之下可以执行的命令，主要是删除操作
  (setq evil-mc-custom-known-commands
        '((paredit-backward-delete . ((:default . evil-mc-execute-default-call-with-count)))
          (paredit-doublequote . ((:default . evil-mc-execute-default-call)))
          (paredit-open-round  . ((:default . evil-mc-execute-default-call)))
          (paredit-close-round  . ((:default . evil-mc-execute-default-call)))
          (paredit-open-square  . ((:default . evil-mc-execute-default-call)))
          (paredit-close-square  . ((:default . evil-mc-execute-default-call)))
          (paredit-forward-slurp-sexp . ((:default . evil-mc-execute-default-call-with-count)))
          (paredit-backward-slurp-sexp . ((:default . evil-mc-execute-default-call-with-count)))
          (hungry-delete-backward . ((:default . evil-mc-execute-default-call-with-count)))
          (org-delete-backward-char . ((:default . evil-mc-execute-default-call-with-count)))
          ))

  (spacemacs|define-micro-state liu233w/evil-mc-add-cursor
    :doc
    "`n' make-and-go-to-next-match `t' skip-and-go-to-next-match"
    :use-minibuffer t
    :on-enter (evil-mc-make-and-goto-next-match)
    :bindings
    ("n" evil-mc-make-and-goto-next-match)
    ("t" evil-mc-skip-and-goto-next-match))

  (define-key evil-visual-state-map (kbd "mn")
    'spacemacs/liu233w/evil-mc-add-cursor-micro-state)
  (define-key evil-visual-state-map (kbd "ma")
    'evil-mc-make-all-cursors)
  )

(defun liu233w/init-multiple-cursors ()
  (use-package multiple-cursors
    :defer t
    :init
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

    ;; 未完成，目前会在添加新cursors的时候询问是否在多个cursor上执行此命令
    (spacemacs|define-micro-state liu233w/mc-mark-next
      :bindings
      ("n" mc/mark-next-like-this))

    (define-key evil-visual-state-map (kbd "mn")
      'spacemacs/liu233w/mc-mark-next-micro-state)
    (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this)
    :config
    (setq mc/list-file "~/.emacs.d/.cache/.mc-lists.el")
    ;; multiple-cursors 中执行下列指令时不询问
    ;; see https://github.com/syl20bnr/spacemacs/issues/2669
    (setq mc/cmds-to-run-for-all
          '(
            electric-newline-and-maybe-indent
            evil-backward-char
            evil-delete-char
            evil-escape-emacs-state
            evil-escape-insert-state
            evil-exit-emacs-state
            evil-forward-char
            evil-insert
            evil-next-line
            evil-normal-state
            evil-previous-line
            forward-sentence
            kill-sentence
            org-self-insert-command
            sp-backward-delete-char
            sp-delete-char
            sp-remove-active-pair-overlay
            evil-force-normal-state
            ))

    (setq mc/cmds-to-run-once
          '(
            helm-M-x
            spacemacs/default-pop-shell
            ))

    ))

(defun liu233w/post-init-slime ()
  (setq inferior-lisp-program (cond
                               ((spacemacs/system-is-mswindows) "wx86cl64")
                               ((spacemacs/system-is-linux) "sbcl")
                               (t "sbcl")
                               ))
  )

(defun liu233w/init-paredit ()
  (use-package paredit
    :defer t
    :init
    (progn
      (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
      (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
      (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
      (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
      (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
      (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
      (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
      (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
      ;; Stop SLIME's REPL from grabbing DEL,
      ;; which is annoying when backspacing over a '('
      (defun override-slime-repl-bindings-with-paredit ()
        (define-key slime-repl-mode-map
          (read-kbd-macro paredit-backward-delete-key) nil))
      (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
      )))

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
    (progn
      (define-key evil-normal-state-map (kbd "zf") 'vimish-fold)
      (define-key evil-visual-state-map (kbd "zf") 'vimish-fold)
      (define-key evil-normal-state-map (kbd "zd") 'vimish-fold-delete)
      (define-key evil-normal-state-map (kbd "za") 'vimish-fold-toggle))))

(defun liu233w/init-ahk-mode ()
  (use-package ahk-mode
    :defer t
    :config
    (add-hook 'ahk-mode-hook '(lambda ()
                                (linum-mode t)))))

(defun liu233w/init-fcitx ()
  "可以在输入快捷键时自动切换输入法"
  (use-package fcitx
    ;; :defer t      ;不能延迟加载，否则无法正常加载
    :config
    (progn
      (setq w32-pass-lwindow-to-system nil)
      (setq w32-lwindow-modifier 'super)
      (setq fcitx-active-evil-states '(insert emacs hybrid)) ; if you use hybrid mode
      (fcitx-aggressive-setup)
      (fcitx-shell-command-turn-on)
      (fcitx-eval-expression-turn-on)
      (fcitx-prefix-keys-add "M-m") ; M-m is common in Spacemacs
      ))
  )

(defun liu233w/init-evil-visual-mark-mode ()
  (use-package evil-visual-mark-mode
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "ot" "toggles")
      (spacemacs|add-toggle evil-visual-mark-mode
        :status evil-visual-mark-mode
        :on (evil-visual-mark-mode)
        :off (evil-visual-mark-mode -1)
        :documentation "Show evil marks"
        :evil-leader "otm")
      (evil-visual-mark-mode))))

(defun liu233w/init-visual-regexp-steroids ()
  "可视化地使用替换模式"
  (use-package visual-regexp-steroids
    :defer t
    :init
    (progn
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
        "xvm" 'vr/mc-mark))))

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

    (global-set-key (kbd "C-;") 'liu233w/tiny-expand-with-undo)
    (spacemacs/set-leader-keys "oe" 'liu233w/tiny-expand-with-undo))
  )

(defun liu233w/post-init-web-mode ()
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
  (add-hook 'web-mode-hook '(lambda ()
                              (turn-off-smartparens-mode)
                              (setq web-mode-enable-auto-pairing t)))
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
                 (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

    (defun liu233w/enable-pangu-spacing-when-buffer-not-large ()
      "when buffer is not large, turn on it"
      (when (< (buffer-size) *large-buffer-threshold*)
        (pangu-spacing-mode 1)))

    (dolist (i '(prog-mode-hook text-mode-hook))
      (add-hook i 'liu233w/enable-pangu-spacing-when-buffer-not-large))
    ))

;;; packages.el ends here
