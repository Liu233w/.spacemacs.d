;;; multi-keys.el --- Like key-chord.el but without input-method depend  -*- lexical-binding: t; -*-

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

;; Like key-chord.el but without input-method depend. Notice that it has not
;; supported two continues keys such as `hh' yet.

;; Example:
;; (setq multi-keys-two-keys-delay 0.1) ; default
;; (multi-keys-define-key "kl" 'pyim-convert-code-at-point) ; set key
;; (multi-keys-unset-key "kl") ; unset key

;;; Code:


;;; variables defination

;;;###autoload
(defvar multi-keys-two-keys-delay 0.1
  "Max time delay between two key press to be considered a key chord.")

(defvar multi-keys-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    map)
  "A key-map for `multi-keys'. Can be modified by `'.

\\{multi-keys-map}")

(defvar multi-keys--command 'ignore)


;;; core functions

(defun multi-keys--execute-command ()
  (interactive)
  (command-execute multi-keys--command))

(global-set-key [multi-keys--command] #'multi-keys--execute-command)


(defun multi-keys-maybe-translate (_)
  "Translate two key press into a key chord.

Only if it has been defined in `multi-keys-map'."
  (if (null input-method-function)
      ;; not a function or list
      nil
    ;; else
    (let ((the-second-key (read-event nil nil multi-keys-two-keys-delay)))
      (if the-second-key
          (let ((cmd (lookup-key multi-keys-map
                                 (concatenate 'vector (this-command-keys-vector)
                                              (vector the-second-key)))))
            (if (and cmd (not (numberp cmd)))
                (progn
                  (setq multi-keys--command cmd)
                  [multi-keys--command])
              ;; else
              (push the-second-key unread-command-events)
              nil))
        ;; else
        nil))))

(defun multi-keys--bind-key (keys command)
  (define-key key-translation-map (substring keys 0 1)
    #'multi-keys-maybe-translate)
  (define-key multi-keys-map keys command))


;;; interfaces

;;;###autoload
(defun multi-keys-define-key (keys command)
  "Define a key-chord of the two keys in KEYS starting a COMMAND.

KEYS must be a two-letter-string."
  (unless (and (stringp keys) (= (length keys) 2))
    (error "Keys must be a two-char-string"))
  (multi-keys--bind-key keys command)
  (multi-keys--bind-key (nreverse keys) command))

;;;###autoload
(defun multi-keys-unset-key (keys)
  "Unset multi-keys in KEYS.

Can't unset key defination in `key-translation-map'."
  (unless (and (stringp keys) (= (length keys) 2))
    (error "Keys must be a two-char-string"))
  (dolist (k (list keys (reverse keys)))
    (define-key multi-keys-map k nil)))


;;; Other packages support

(with-eval-after-load 'chinese-pyim
  (define-key pyim-mode-map [multi-keys--command] #'multi-keys--execute-command))


(provide 'multi-keys)
;;; multi-keys.el ends here
