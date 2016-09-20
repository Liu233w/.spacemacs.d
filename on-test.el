;;; 包含正在测试中的代码，还不能正式开始使用

(defun liu233w//compile-and-run (cmp-cmd run-cmd)
  "Run cmp-cmd, if success, then run run-cmd and print the result.
Or just print the error message."
  (let* ((process (start-process-shell-command
                   (gensym)
                   "*liu233w/run-current-file output*"
                   cmp-cmd))
         )))
