;;; run-current-file.el --- Execute the current file  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Liu233w

;; Author: http://ergoemacs.org/emacs/elisp_run_current_file.html
;; Keywords: convenience, languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Execute the current file.

;; For example, if the current buffer is the file x.py, then it'll call「python
;; x.py」in a shell. The file can be Emacs Lisp, PHP, Perl, Python, Ruby,
;; JavaScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure. File suffix is
;; used to determine what program to run.

;; If the file is modified or not saved, save it automatically before run.

;; URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
;; version 2016-01-28

;;; Code:

(defun rcf/run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll
call「python x.py」in a shell. The file can be Emacs Lisp, PHP,
Perl, Python, Ruby, JavaScript, Bash, Ocaml, Visual Basic, TeX,
Java, Clojure. File suffix is used to determine what program to
run.

If the file is modified or not saved, save it automatically
before run.

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
      (rcf//compile-and-run
       -cmd-str
       (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname)))))
     ((string-equal -fSuffix "cpp")
      (rcf//compile-and-run
       (format "%s --std=c++11" -cmd-str)
       "a"))
     (t (if -prog-name
            (progn
              (message "Running…")
              (async-shell-command -cmd-str "*rcf/run-current-file output*" ))
          (message "No recognized program file suffix for this file."))))))

(defun rcf//compile-and-run (cmp-cmd run-cmd)
  "Run cmp-cmd, if success, then run run-cmd and print the result.
Or just print the error message.

When it's compiling a file, this function may cause error
behavior."
  (setf rcf//compile-status (cons run-cmd compile-command))
  (compile cmp-cmd)
  )

(defvar rcf//compile-status nil "doc")

(defun rcf//run-after-compile (buffer string)
  (when (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         rcf//compile-status)
    ;; 防止 rcf/bury-compile-buffer-if-successful 删除 compilation buffer
    ;; 这样 async-shell-command 命令就能覆盖 compilation buffer 而不是源代码的
    ;; buffer 了。
    (with-current-buffer "*compilation*"
      (insert "warning LOL"))
    (async-shell-command (car rcf//compile-status)
                         "*rcf/run-current-file output*")
    (setf compile-command (cdr rcf//compile-status)
          rcf//compile-status nil)))
(add-hook 'compilation-finish-functions
          #'rcf//run-after-compile)

(provide 'run-current-file)
;;; run-current-file.el ends here
