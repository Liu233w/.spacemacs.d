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
    multiple-cursors
    smart-compile
    slime
    paredit
    ace-mc
    evil-vimish-fold
    ahk-mode
    fcitx
    org
    hungry-delete
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
    :init (evil-leader/set-key "cs" 'smart-compile)))

(defun liu233w/init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

    (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
    (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this)
    ))

(defun liu233w/post-init-slime ()
  (setq inferior-lisp-program (cond
                               ((spacemacs/system-is-mswindows) "wx86cl64")
                               ((spacemacs/system-is-linux) "sbcl")
                               (t "sbcl")
                               ))
  )

(defun liu233w/init-paredit ()
  (use-package paredit)
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
  )

(defun liu233w/init-ace-mc ()
  (use-package ace-mc
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
  (use-package ahk-mode))

(defun liu233w/init-fcitx ()
  "可以在输入快捷键时自动切换输入法"
  (use-package fcitx)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (setq fcitx-active-evil-states '(insert emacs hybrid)) ; if you use hybrid mode
  (fcitx-aggressive-setup)
  (fcitx-shell-command-turn-on)
  (fcitx-eval-expression-turn-on)
  (fcitx-prefix-keys-add "M-m") ; M-m is common in Spacemacs
  )

(defun liu233w/post-init-hungry-delete ()
  (global-hungry-delete-mode t))

(defun liu233w/post-init-org ()
  ;;org-mode和org-mobile的文件夹
  (setf org-directory "~/documents/org-mode/host/"
        org-mobile-directory "~/documents/org-mode/"
        org-mobile-inbox-for-pull "~/documents/org-mode/index.org")

  ;; 完成状态
  (setf org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n)" "WAITTING(w)" "SOMEDAY(s)" "|" "DONE(d@/!)" "ABORT(a@/!)")
          ))

  ;; 用来检测md5的程序
  (defcustom org-mobile-checksum-binary
    (or (executable-find "md5sums") (executable-find "md5sum"))
    ' "Executable used for computing checksums of agenda files."
      ':group 'org-mobile
      ' :type 'string)

  ;; org-agenda的全局文件
  (setf org-agenda-files (list "~/documents/org-mode/host/TODO.org"
                               "~/documents/org-mode/host/notes.org"))

  ;; 在下一级任务的完成度达到100%时自动将上一级设置为DONE
  ;; from http://www.cnblogs.com/holbrook/archive/2012/04/14/2447754.html
  (defun liu233w/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'liu233w/org-summary-todo)
  )

;;; packages.el ends here
