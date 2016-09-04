;;; multiple-micro-state.el --- 在生成micro-state的同时对每个命令生成对应的函数。 -*- lexical-binding: t -*-

;; Copyright (c) 2016 Liu233w
;;
;; Author: Liu233w <wwwlsmcom@outlook.com>
;; Created: 2016-09-02
;;
;;; License: GPLv3

;;; Commentary:
;; 函数的参数和调用方式可以和原来的命令相同，在执行命令之后自动进入micro-state

;;; Code:

(require 'core-micro-state)
(require 'nadvice)
(require 'liu233w-util-funcs)

(defun mms//generate-micro-state-name (name)
  "生成micro-state的名字，将返回一个symbol。
参数name是一个symbol"
  (intern (format "%s:micro-state" name)))

(defun mms//generate-function-name (name func)
  "生成micro-state的各个函数的名称，返回一个symbol。
参数name和func都是symbol"
  (intern (format "%s:%s-then-enter-micro-state" name func)))

(defun mms//get-function-key-list (lst)
  "接受一个plist，返回一个在:bindings中的列表里面每个子列表的前两项组成的形如
`(键绑定 . 函数名)' 的列表"
  (mapcar #'(lambda (item) (cons (first item) (second item)))
          (spacemacs/mplist-get lst :bindings)))

(defun mms//generate-document (lst)
  "接受一个含有键绑定和函数名的cons的列表，返回一个字符串，含有键绑定和对应的函数名"
  (mapconcat #'(lambda (item) (format "`%s' %s " (car item) (cdr item)))
             lst " "))

(defun mms//list-all-argument (arglist-form)
  "接受一个函数的参数列表，返回一个列表，其第一项是固定的参数和optional函数
（去掉了 &optional symbol），第二项是 &rest 参数，如果没有的话就是nil

如： 参数\(a b &optional c &rest rest) 将返回 \(a b c \"???\")"
  (let ((res nil)
        (is-not-rest t))
    (dolist (item arglist-form)
      (let* ((first-char (elt (format "%s" item) 0)))
        (if (eql first-char ?&)
            (when (string-equal item "&rest")
              (setf is-not-rest nil))
          (push (if is-not-rest item  "???")
                res))))
    (nreverse res)))

(defun mms//generate-function-defination (name func state-name use-original-args)
  "生成函数的定义。
name是micro-state的名字，func是函数名，state-name是之前生成的micro-state
的名字，都是symbol。
生成的函数与原来的函数有相同的形参列表和interactive，在执行完原有函数的功能之后
会启动相应的micro-state。"
  (let ((func-name (mms//generate-function-name name func)))
    (let ((func-args (if use-original-args
                         (help-function-arglist func)
                       '(&rest rest))))
      `(defun ,func-name ,func-args
           ,(format "Call `%s' then call `%s'" func state-name)
         ,(or (interactive-form func)
              '(interactive))
         (apply (function ,func)
                (list ,@(mms//list-all-argument func-args)))
         (,state-name)))))

(defmacro mms|define-multiple-micro-state (name &rest props)
  "使用`spacemacs|define-micro-state'来生成micro-state，同时对每个命令生成一个函数，
调用函数会执行相应的命令并进入micro-state。

参数列表详见`spacemacs|define-micro-state'
额外的参数 :use-original-arg-list t表示生成的函数将拥有和原函数相同的
参数列表。由于使用了`byte-complie'，这样的执行速度可能会慢一些，默认为nil。
注意：如果使用 user-original-login-name 的话，请确保这个宏在原来的函数加载之后
才展开。所以如果把宏放在spacemacs的init 函数中，请这么使用宏：
\(eval '(mms|define-multiple-micro-state ...))
对于micro-state的`:doc'参数如果传入auto，则根据键绑定和命令名自动生成doc"
  (let ((state-name (mms//generate-micro-state-name name))
        (binding-list (mms//get-function-key-list props))
        (use-original-arg-list (plist-get props :use-orininal-arg-list)))
    (when (eql (plist-get props :doc) 'auto)
      (setq props (plist-put props :doc (mms//generate-document binding-list))))
    (setf props (plist-delete props :use-orininal-arg-list))
    `(progn
       (spacemacs|define-micro-state ,name ,@props)
       (defalias (quote ,state-name) (quote ,(spacemacs//micro-state-func-name name)))
       ,@(mapcar #'(lambda (item)
                     (mms//generate-function-defination name (cdr item) state-name use-original-arg-list))
                 binding-list))))


(provide 'multiple-micro-state)
;;; multiple-micro-state.el ends here
