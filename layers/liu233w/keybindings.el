;;add clang-format support
(add-hook 'c++-mode-hook
          (lambda ()
            (when (executable-find "clang-format")
              ;;使用clang-format作为默认排版工具
              (local-set-key (kbd "C-M-\\") 'clang-format)
              ;;当插入分号时自动对当前行排版
              (local-set-key (kbd ";")
                             'liu233w/semi-clang-format)
              (local-set-key (kbd "}")
                             'liu233w/brace-clang-format))))

;;光标在行首时注释此行
;;说明见http://cmdblock.blog.51cto.com/415170/557978/
(defun liu233w/qiang-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected and current line is not blank and we are not at the end of the line, then comment current line. Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'liu233w/qiang-comment-dwim-line)

;;使用F10在当前buffer和shell之间来回切换
(defun liu233w/toggle-shell ()
  "Switch current buffer to shell.
If current buffer is shell, then switch to prevous buffer.
When enter the shell buffer, evil state will be switched to emacs-state,
C-z can switch between emacs-mode and normal-mode"
  (interactive)
  (if (string-equal (buffer-name) "*shell*")
      (bs-cycle-next)
    (shell)))
(global-set-key (kbd "<f10>") 'liu233w/toggle-shell)

;;在插入模式下使用C-O来在光标前插入一个换行符
(define-key evil-insert-state-map (kbd "C-S-o") 'open-line)
;;在普通模式下在下一行增加一个空行，但光标不移动也不进入插入模式
(define-key evil-normal-state-map (kbd "C-S-o")
  '(lambda () (interactive) (save-excursion (evil-open-below 1) (evil-normal-state))))

;; don't need it anymore
;; (evil-leader/set-key "oe" 'eclim-project-mode)

;;使用occur搜索
(evil-leader/set-key "so" 'occur)

;;在插入模式下使用C-d删除光标后的内容（<delete>）
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)

;; from zilongshanren
(global-set-key (kbd "C-S-y") 'helm-show-kill-ring)
