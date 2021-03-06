;; remove all the duplicated emplies in current buffer
;; from zilongshanren
(defun liu233w/single-lines-only ()
  "replace multiple blank lines with a single one"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))

;;from zilongshanren
(defun liu233w/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))


;; insert date and time
(defun liu233w/now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%D %-I:%M %p")))

(defun liu233w/today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

(defun liu233w/project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (or (locate-dominating-file directory ".git")
        (locate-dominating-file directory ".svn")
        (locate-dominating-file directory ".hg"))))

(defun liu233w/open-file-with-projectile-or-lsgit ()
  (interactive)
  (if (liu233w/project-root)
      (counsel-git)
    (helm-projectile-find-file)))

(defun liu233w/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line
endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun liu233w/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(unless (functionp 'liu233w/set-chinese-fonts)
  (defun liu233w/set-chinese-fonts (&optional frame)
    "根据当前系统重新设置中文字体"
    (when (window-system)
      (when frame
        (select-frame frame))
      (cond
       ((spacemacs/system-is-mswindows)
        ;; Setting English Font
        (unless (search "Source Code Pro" (frame-parameter nil 'font))
          (set-face-attribute
           'default nil :font "Consolas 18"))
        ;; Chinese Font
        (dolist (charset '(kana han cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Microsoft Yahei" :size 22))))
       ((spacemacs/system-is-linux)
        (set-default-font "文泉驿等宽微米黑-18"))))))

(unless (functionp 'liu233w/reset-frame-size)
  (defun liu233w/reset-frame-size (&optional frame)
    "重设窗体大小"
    (interactive)
    (when frame
      (select-frame frame))
    (set-frame-width (selected-frame) 100)
    (set-frame-height (selected-frame) 30)))

(defun liu233w/ex-kill-buffer-and-close ()
  "删除当前 buffer 并关闭窗口，如果 buffer 删除失败（例如没有保
存），则不会关闭窗口。"
  (interactive)
  ;; 如果当前的 buffer 没有关联文件，则不删除当前 buffer
  (if (buffer-file-name)
      (when (kill-buffer)
        (evil-quit))
    (evil-quit)))

(defun liu233w/ex-save-kill-buffer-and-close ()
  "保存当前 buffer 的内容，删除 buffer，并关闭窗口，如果删除失败，
则不会关闭窗口。"
  (interactive)
  (save-buffer)
  (when (kill-buffer)
    (evil-quit)))

(defun liu233w/insert-user-name-and-email ()
  "生成和 package 的元数据中格式相同的用户名和邮箱"
  (interactive)
  (insert (format "%s <%s>" (user-full-name) user-mail-address)))
