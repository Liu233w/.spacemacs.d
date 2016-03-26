(setq inferior-lisp-program (cond
                             ((spacemacs/system-is-mswindows) "wx86cl64")
                             ((spacemacs/system-is-linux) "sbcl")
                             (t "sbcl")
                             ))

;;读取在不同系统之下的配置
(if (file-exists-p "~/.myemacs.el")
    (load-file "~/.myemacs.el"))

