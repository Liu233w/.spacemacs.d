;;; -*- lexical-binding: t -*-

;;; multiple-micro-state.el --- 在生成micro-state的同时对每个命令生成对应的函数。

;; 函数的参数和调用方式可以和原来的命令相同，在执行命令之后自动进入micro-state

;; Copyright (c) 2016 Liu233w
;;
;; Author: Liu233w <wwwlsmcom@outlook.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'core-micro-state)

(defun fun1 ()
  (interactive)
  (message "hello"))
(defalias 'fun2 'fun1)
(fset 'fun3 (symbol-function 'fun1))
;;; 然后再 advice-add 就可以了
(advice-add 'fun3 :after '(lambda () (message "add-adv")))

(fset 'new-next-page (symbol-function 'evil-scroll-page-down))
