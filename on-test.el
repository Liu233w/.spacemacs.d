;;; 包含正在测试中的代码，还不能正式开始使用

(defvar liu233w//prev-compile-command "make -k")
(defvar liu233w//cr-run-command "")

(defun liu233w//compile-and-run (cmp-cmd run-cmd)
  "Run cmp-cmd, if success, then run run-cmd and print the result.
Or just print the error message."
  (make-local-variable 'liu233w//prev-compile-command)
  (make-local-variable 'liu233w//cr-run-command)
  (setf liu233w//prev-compile-command compile-command
        liu233w//cr-run-command run-cmd)
  (let* ((buffer (get-buffer-create "*liu233w/run-current-file output*"))
         )
    (compile cmp-cmd)
    (when (equal process 0)
      (async-shell-command run-cmd))
    (set-window-buffer window buffer)))

(defun liu233w//show-compile-result)

(defvar my-compilation-exit-code nil)
(defun my-compilation-exit-message-function (status_ code message)
  (setq my-compilation-exit-code code)
  (cons message code))

(setq compilation-exit-message-function 'my-compilation-exit-message-function)
