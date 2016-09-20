;;; 包含正在测试中的代码，还不能正式开始使用

(defvar liu233w//compile-status nil "doc")

(defun liu233w//run-after-compile (buffer string)
  (when (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         liu233w//compile-status)
    (async-shell-command (car liu233w//compile-status)
                         "*liu233w/run-current-file output*")
    (setf liu233w//compile-status nil)))
(add-hook 'compilation-finish-functions
          #'liu233w//run-after-compile)

(defun liu233w//compile-and-run (cmp-cmd run-cmd)
  "Run cmp-cmd, if success, then run run-cmd and print the result.
Or just print the error message.

When it's compiling a file, this function may cause error behavior."
  (setf liu233w//compile-status (cons run-cmd compile-command))
  (compile cmp-cmd)
  )
