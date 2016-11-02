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
  (mapcar (lambda (item) (cons (first item) (second item)))
          (spacemacs/mplist-get lst :bindings)))

(defun mms//generate-document (lst)
  "接受一个含有键绑定和函数名的cons的列表，返回一个字符串，含有键绑定和对应的函数名"
  (mapconcat (lambda (item) (format "`%s' %s " (car item) (cdr item)))
             lst " "))

(defun mms//generate-function-defination (name func state-name)
  "生成函数的定义。
name是micro-state的名字，func是函数名，state-name是之前生成的micro-state
的名字，都是symbol。
生成的函数使用`command-execute'执行原有的函数。在执行完原有函数的功能之后
会启动相应的micro-state。"
  (let* ((func-name (mms//generate-function-name name func)))
    `(defun ,func-name ()
       ,(format "Call `%s' then call `%s'" func state-name)
       (interactive)
       (command-execute (function ,func))
       (,state-name))))

(defun mms//list-all-argument (arglist-form)
  "接受一个函数的参数列表，返回一个列表，其第一项是固定的参数和optional函数
\(去掉了 &optional symbol\)，第二项是 &rest 参数，如果没有的话就是nil

如： 参数\(a b &optional c &rest rest\) 将返回 \(\(a b c\) rest\)"
  (let ((res nil)
        (the-rest nil))
    (dolist (item arglist-form)
      (cond
       (the-rest
        (setf the-rest item))
       ((string-equal item "&rest")
        (setf the-rest t))
       ((not (eql ?& (elt (format "%s" item) 0)))
        (push item res))))
    (list (reverse res) the-rest)))

(defun mms//generate-function-defination-with-full-args (name func state-name)
  "生成函数的定义。
name是micro-state的名字，func是函数名，state-name是之前生成的micro-state
的名字，都是symbol。
生成的函数与原来的函数有相同的形参列表和interactive，在执行完原有函数的功能之后
会启动相应的micro-state。
使用本函数生成定义时，原函数的定义必须完全加载完成。"
  (let* ((func-name (mms//generate-function-name name func))
         (func-args (help-function-arglist func))
         (send-args (mms//list-all-argument func-args)))
    `(defun ,func-name ,func-args
       ,(format "Call `%s' then call `%s'" func state-name)
       ,(or (interactive-form func)
            '(interactive))
       (apply (function ,func)
              ,@(first send-args) ,(second send-args))
       (,state-name))))

(defmacro mms|define-multiple-micro-state (name &rest props)
  "使用`spacemacs|define-micro-state'来生成micro-state，同时对每个命令生成一个函数，
调用函数会执行相应的命令并进入micro-state。

参数列表详见`spacemacs|define-micro-state'
对于micro-state的`:doc'参数如果传入auto，则根据键绑定和命令名自动生成doc。
额外的参数`:with-full-arguments'默认为nil，如果置为t，则生成的函数将拥有和
原函数同样的interactive和参数列表，但是在宏展开的时候原函数的定义必须完全加载
完成；如果为nil，生成的函数则不接受参数，只有一个无参数的interactive，会使用
`command-execute'来执行原来的函数。"
  (let ((state-name (mms//generate-micro-state-name name))
        (binding-list (mms//get-function-key-list props))
        (with-arguments (plist-get props :with-full-arguments)))
    (when (eql (plist-get props :doc) 'auto)
      (setq props (plist-put props :doc (mms//generate-document binding-list))))
    `(progn
       (spacemacs|define-micro-state ,name ,@props)
       (defalias (quote ,state-name)
         (quote ,(spacemacs//micro-state-func-name name)))
       ,@(mapcar (lambda (item)
                     (funcall
                      (if with-arguments
                          #'mms//generate-function-defination-with-full-args
                        #'mms//generate-function-defination)
                      name (cdr item) state-name))
                 binding-list))))


(provide 'multiple-micro-state)
;;; multiple-micro-state.el ends here
