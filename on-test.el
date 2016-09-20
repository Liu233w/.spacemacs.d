;;; 包含正在测试中的代码，还不能正式开始使用

(defvar liu233w//prev-compile-command "make -k ")
(defvar liu233w//cr-run-command "")

(defun liu233w//compile-and-run (cmp-cmd run-cmd)
  "Run cmp-cmd, if success, then run run-cmd and print the result.
Or just print the error message."
  (setf liu233w//prev-compile-command compile-command
        liu233w//cr-run-command run-cmd)
  (compile cmp-cmd))

(defun liu233w//show-compile-result (status_ code message)
  (when (equal code 0)
    (set-window-buffer
     (get-buffer-window "*compilation*")
     (get-buffer-create "*liu233w/run-current-file output*")))
  (setf compile-command liu233w//prev-compile-command)
  (shell-command liu233w//cr-run-command "*liu233w/run-current-file output*"))

(setq compilation-exit-message-function
      'liu233w//show-compile-result)
