
;; For my language code setting (UTF-8)
(set-language-environment "chinese-GBK")
(prefer-coding-system 'utf-8-auto)

;;设置窗口大小
(when (spacemacs/system-is-mswindows)
  (defun reset-frame-size ()
    (interactive)
    (set-frame-width (selected-frame) 80)
    (set-frame-height (selected-frame) 20))
  (reset-frame-size))

;;setting Font
(cond
 ((spacemacs/system-is-mswindows)
  ;; Setting English Font
  (set-face-attribute
   'default nil :font "Consolas 18")
  ;; Chinese Font
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Microsoft Yahei" :size 19))))
 ((spacemacs/system-is-linux)
  (set-default-font "文泉驿等宽微米黑-18"))
 )

(display-time-mode 1)

(when (spacemacs/system-is-mswindows)
  (setq eclim-eclipse-dirs "c:/Software/eclipse"
        eclim-executable "c:/Software/eclipse/eclim.bat")
  (add-to-list 'eclim--file-coding-system-mapping '("utf-8-auto-dos" . "utf-8")))
