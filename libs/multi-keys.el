;;; multi-keys.el --- Like key-chord.el but without input-method dependent  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Liu233w

;; Author: Liu233w <wwwlsmcom@outlook.com>
;; Keywords: convenience

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

;; Like key-chord.el but without input-method dependent. Notice that it has not
;; supported two continues keys such as `hh' so far.

;; Most of the commands are the same as which are in key-chord.el

;; Example:
;; (setq multi-keys-two-keys-delay 0.1) ; default
;; (multi-keys-define-global "kl" 'pyim-convert-code-at-point) ; set key
;; (multi-keys-unset-global "kl") ; unset key

;; commands:
;; `multi-keys-mode' ; toggle buffer local minor mode
;; `global-multi-keys-mode' ; toggle global

;; Change its translate strategy by changing variable
;; `multi-keys-translation-function'. There are two translation function listed
;; below. See their docs for more details.
;;
;; `multi-keys-translate-by-keysequence' -- default
;; `multi-keys-translate-by-timer'

;;; Code:
(require 'cl)
(require 'timer)
(require 'simple)


;;;; public variables

;;;###autoload
(defvar multi-keys-two-keys-delay 0.1
  "Max time delay between two key press to be considered a key chord.")

(defvar multi-keys-translation-function
  #'multi-keys-translate-by-keysequence
  "A function to translate the key.")


;;;; minor mode

;;;###autoload
(define-minor-mode multi-keys-mode
  "Like key-chord.el but without input-method dependent."
  :lighter "MK")

;;;###autoload
(define-global-minor-mode global-multi-keys-mode
  multi-keys-mode
  (lambda () (multi-keys-mode 1)))


;;;; util functions

(defun multi-keys--lookup-key (keys)
  "Lookup KEY in all current key maps."
  (let ((maps (current-minor-mode-maps))
        res)
    (while (and maps (not res))
      (setq res (multi-keys-lookup-key1 (car maps) key)
            maps (cdr maps)))
    (or res
        (if (current-local-map)
            (multi-keys-lookup-key1 (current-local-map) key))
        (multi-keys-lookup-key1 (current-global-map) key))))

(defun multi-keys--lookup-key1 (map keys)
  "Like lookup-key but no third arg and no numeric return value."
  (let ((cmd (lookup-key map keys)))
    (if (numberp cmd)
        nil
      ;; else is a command or nil
      cmd)))

(defun multi-keys--lookup-key (keys)
  (or (loop for map in (current-minor-mode-maps)
            for res = (multi-keys--lookup-key1 map keys)
            when res return res
            finally return nil)
      (if (current-local-map)
          (multi-keys--lookup-key1 (current-local-map) keys))
      (multi-keys--lookup-key1 (current-global-map) keys)))

(defun multi-keys--read-key-chord ()
  "Read current key-chord and return its command which binded in `multi-keys-map'.

If this function finds nothing, it returns nil."
  (let ((the-second-key (read-event nil nil multi-keys-two-keys-delay)))
    (if the-second-key
        (let* ((keys (concatenate 'vector
                                  [multi-keys]
                                  (this-command-keys-vector)
                                  (vector the-second-key)))
               (cmd (multi-keys--lookup-key keys)))
          (if cmd
              cmd
            ;; else
            (push the-second-key unread-command-events)
            nil))
      ;; else
      nil)))

(defun multi-keys-maybe-translate (prompt)
  "Translate two key press into a key chord.

It will call `multi-keys-translation-function' to do the actual
translation job."
  (when multi-keys-mode
    (funcall multi-keys-translation-function prompt)))


;;;; translation strategy: multi-keys-translate-by-timer

(defun multi-keys-translate-by-timer (_)
  "One of the translation strategy.

It will exit command loop by using quit signal. Be effective when
selection box of input method is active."

  "multi-keys 的转换策略之一，主要使用 quit 来退出当前的 command loop。

在输入法选词框打开的时候也可以使用，但在使用之后会强制退出选词框
并将选词框中的内容留在 buffer 中。并且会在 mini-buffer 里面留下
quit 的信息。"
  (let ((cmd (multi-keys--read-key-chord)))
    (if cmd
        (progn
          (run-at-time nil nil (lambda () (command-execute cmd)))
          (signal 'quit nil))
      ;;else
      nil)))


;;;; translation strategy: multi-keys-translate-by-keysequence

(defvar multi-keys--command 'ignore)

(defun multi-keys--execute-command ()
  (interactive)
  (command-execute multi-keys--command))

;; some input methods may have their own keymap
(global-set-key [multi-keys--command] #'multi-keys--execute-command)

(defun multi-keys-translate-by-keysequence (_)
  "One of the translation strategy.

When the selection box of input method is active, the key chords
you defined won't work."

  "multi-keys 的转换策略之一，主要使用 key-translation-map 的键映射来转换。

不使用 quit，因此不会在 message 中留下信息。在候选词框打开的时候不生效。"
  (if (null input-method-function) ; not a function or list
      nil
    ;; else
    (let ((cmd (multi-keys--read-key-chord)))
      (if cmd
          (progn
            (setq multi-keys--command cmd)
            [multi-keys--command])
        ;; else
        nil))))


;;;; set and unset keys

;;;###autoload
(defun multi-keys-define (keymap keys command)
  "Define a multi-keys of the two keys in KEYS starting a COMMAND.

KEYS must be a two-letter-string whose elements can't be the
same. Currently only elements that corresponds to ascii codes in
the range 32 to 126 can be used.

If COMMAND is nil, the multi-keys is removed. Notice that it
can't unset key defination in `key-translation-map'."
  (unless (and (stringp keys) (= (length keys) 2))
    (error "Keys must be a two-char-string"))
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (if (eq key1 key2)
        (error "Keys can't be the same.")
      ;; else
      (multi-keys--bind-key keymap (vector 'multi-keys key1 key2) command)
      (multi-keys--bind-key keymap (vector 'multi-keys key2 key1) command))))

(defun multi-keys--bind-key (keymap keys command)
  (define-key key-translation-map (subseq keys 1 2)
    #'multi-keys-maybe-translate)
  (define-key keymap keys command))

;;;###autoload
(defun multi-keys-define-global (keys command)
  "Define a multi-keys of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the multi-keys is removed. Notice that it
can't unset key defination in `key-translation-map'.

Note that KEYS defined locally in the current buffer will have precedence."
  (interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (multi-keys-define (current-global-map) keys command))

;;;###autoload
(defun multi-keys-define-local (keys command)
  "Locally define a multi-keys of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the multi-keys is removed.

The binding goes in the current buffer's local map,
which in most cases is shared with all other buffers in the same major mode."
  (interactive "sSet key chord locally (2 keys): \nCSet chord \"%s\" to command: ")
  (multi-keys-define (current-local-map) keys command))

(defun multi-keys-unset-global (keys)
  "Remove global multi-keys of the two keys in KEYS."
  (interactive "sUnset key chord globally (2 keys): ")
  (multi-keys-define (current-global-map) keys nil))

(defun multi-keys-unset-local (keys)
  "Remove local multi-keys of the two keys in KEYS."
  (interactive "sUnset key chord locally (2 keys): ")
  (multi-keys-define (current-local-map) keys nil))

(defun multi-keys-describe ()
  "List key chord bindings in a help buffer.

Two key chords will be listed twice and there will be Prefix Commands.
Please ignore that."
  (interactive)
  (describe-bindings [multi-keys]))


;;;; Other packages support

(with-eval-after-load 'chinese-pyim
  (define-key pyim-mode-map [multi-keys--command] #'multi-keys--execute-command))


(provide 'multi-keys)
;;; multi-keys.el ends here
