(defun liu233w/semi-clang-format (args)
  "format by clang-format when enter ';'"
  (interactive "*P")
  (c-electric-semi&comma args)
  (clang-format-region (line-beginning-position 0) (line-beginning-position 2))
  )

(defun liu233w/brace-clang-format (args)
  "format by clang-format when enter '}'"
  (interactive "*P")
  (c-electric-brace args)
  (let ((end-position (point))
        begin-position)
    (save-excursion
      (evil-jump-item)
      (setf begin-position (point)))
    (clang-format-region begin-position end-position))
  )

;; remove all the duplicated emplies in current buffer
;; from zilongshanren
(defun zilongshanren/single-lines-only ()
  "replace multiple blank lines with a single one"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))

;;from zilongshanren
(defun zilongshanren/rename-file-and-buffer ()
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

;;add count for chinese, mainly used for writing chinese blog post
;; http://kuanyui.github.io/2014/01/18/count-chinese-japanese-and-english-words-in-emacs/
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")

;;from zilongshanren
(defun zilongshanren/word-count-for-chinese ()
  "「較精確地」統計中/日/英文字數。
- 文章中的註解不算在字數內。
- 平假名與片假名亦包含在「中日文字數」內，每個平/片假名都算單獨一個字（但片假
  名不含連音「ー」）。
- 英文只計算「單字數」，不含標點。
- 韓文不包含在內。

※計算標準太多種了，例如英文標點是否算入、以及可能有不太常用的標點符號沒算入等
。且中日文標點的計算標準要看 Emacs 如何定義特殊標點符號如ヴァランタン・アルカン
中間的點也被 Emacs 算為一個字而不是標點符號。"
  (interactive)
  (let* ((v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ; 去掉 org 文件的 OPTIONS（以#+開頭）
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                (buffer-substring-no-properties (point-min) (point-max))))
              (setq v-buffer-string (buffer-substring-no-properties (point-min) (point-max))))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string)))
                                        ; 把註解行刪掉（不把註解算進字數）。
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文（含標點、片假名）
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文標點符號
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字數（不含標點）
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中日文字數（不含標點）：%s
中日文字數（包含標點）：%s
英文字數（不含標點）：%s
=======================
中英文合計（不含標點）：%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)))))

;; insert date and time
(defun zilongshanren/now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%D %-I:%M %p")))

(defun zilongshanren/today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

;;; -----------liu233w/run-current-file-------------------
(defun liu233w/run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2016-01-28"
  (interactive)
  (let (
        (-suffix-map
         ;; (‹extension› . ‹shell program name›)
         `(
           ("php" . "php")
           ("pl" . "perl")
           ("py" . "python")
           ("py3" . ,(if (string-equal system-type "windows-nt") "python.exe" "python3"))
           ("rb" . "ruby")
           ("go" . "go run")
           ("js" . "node") ; node.js
           ("sh" . "bash")
           ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
           ("rkt" . "racket")
           ("ml" . "ocaml")
           ("vbs" . "cscript")
           ("tex" . "pdflatex")
           ("latex" . "pdflatex")
           ("java" . "javac")
           ("cpp" . ,(or (executable-find "myclang")
                         (executable-find "clang")
                         "g++"))
           ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
           ))

        -fname
        -fSuffix
        -prog-name
        -cmd-str)

    (when (null (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))

    (setq -fname (buffer-file-name))
    (setq -fSuffix (file-name-extension -fname))
    (setq -prog-name (cdr (assoc -fSuffix -suffix-map)))
    (setq -cmd-str (concat -prog-name " \""   -fname "\""))

    (cond
     ((string-equal -fSuffix "el") (load -fname))
     ((string-equal -fSuffix "java")
      (liu233w//compile-and-run
       -cmd-str
       (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname)))))
     ((string-equal -fSuffix "cpp")
      (liu233w//compile-and-run
       (format "%s --std=c++11" -cmd-str)
       "a"))
     (t (if -prog-name
            (progn
              (message "Running…")
              (async-shell-command -cmd-str "*liu233w/run-current-file output*" ))
          (message "No recognized program file suffix for this file."))))))

(defun liu233w//compile-and-run (cmp-cmd run-cmd)
  "Run cmp-cmd, if success, then run run-cmd and print the result.
Or just print the error message.

When it's compiling a file, this function may cause error behavior."
  (setf liu233w//compile-status (cons run-cmd compile-command))
  (compile cmp-cmd)
  )

(defvar liu233w//compile-status nil "doc")

(defun liu233w//run-after-compile (buffer string)
  (when (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         liu233w//compile-status)
    ;; 防止liu233w/bury-compile-buffer-if-successful删除compilation buffer
    ;; 这样async-shell-command 命令就能覆盖compilation buffer 而不是源代码的
    ;; buffer了。
    (with-current-buffer "*compilation*"
      (insert "warning LOL"))
    (async-shell-command (car liu233w//compile-status)
                         "*liu233w/run-current-file output*")
    (setf compile-command (cdr liu233w//compile-status)
          liu233w//compile-status nil)))
(add-hook 'compilation-finish-functions
          #'liu233w//run-after-compile)
;;; END -----------liu233w/run-current-file-------------------

(defun zilongshanren/project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (or (locate-dominating-file directory ".git")
        (locate-dominating-file directory ".svn")
        (locate-dominating-file directory ".hg"))))

(defun zilongshanren/open-file-with-projectile-or-lsgit ()
  (interactive)
  (if (zilongshanren/project-root)
      (counsel-git)
    (helm-projectile-find-file)))

(defun zilongshanren/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun zilongshanren/remove-dos-eol ()
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
  "删除当前buffer并关闭窗口，如果buffer名称带有*则不删除"
  (interactive)
  ;; 只有名称不带*的buffer才会被删除
  (unless (char-equal (elt (buffer-name) 0) ?*)
    (kill-this-buffer))
  ;; bug：在新建client的时候，不能使用-c参数新建frame，否则，
  ;; (menu-bar-menu-frame-live-and-visible-p)会变成nil，导致kill-this-buffer
  ;; 失效（也就是说连SPC b d都不能用了）。至于为什么那个变成nil
  ;; 会影响kill-this-buffer，请自行查看kill-this-buffer的源代码
  (evil-quit)
  )

(defun liu233w/ex-save-kill-buffer-and-close ()
  "保存当前buffer的内容，删除buffer，并关闭窗口"
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  (evil-quit))

;; (defun liu233w/set-in-all-prog-mode  ()
;;   "在所有编程语言之中启动80列指示器和行号"
;;   (turn-on-fci-mode)
;;   (linum-mode t))

(defun liu233w/insert-user-name-and-email ()
  "生成和package的元数据中格式相同的用户名和邮箱"
  (interactive)
  (insert (format "%s <%s>" (user-full-name) user-mail-address)))

;; 一个micro-state ，用来快速翻页
(require 'multiple-micro-state)
(mms|define-multiple-micro-state
 liu233w/view
 :use-minibuffer t
 :doc "`d' scroll-down `u' scroll-up `f' scroll-page-down `b' scroll-page-up"
 :with-full-arguments t
 :bindings
 ("d" evil-scroll-down)
 ("u" evil-scroll-up)
 ("f" evil-scroll-page-down)
 ("b" evil-scroll-page-up))

(defun liu233w/super-send (func)
  "如果是normal，则执行光标上的语句\(而不是光标前)"
  (interactive)
  (if (evil-insert-state-p)
      (command-execute func)
    (evil-save-state
      (evil-append 0)
      (command-execute func))))
