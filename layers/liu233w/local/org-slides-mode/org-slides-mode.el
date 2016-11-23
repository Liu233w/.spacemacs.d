;;; org-slides-mode.el --- 用来演示org               -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Nat Knight

;; Author: Nat Knight
;; Keywords:

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

;; 代码来自 https://github.com/lujun9972/emacs-document/blob/master/emacs-common/%E7%94%A8Emacs%E4%BD%9C%E5%B1%95%E7%A4%BA.org

;;; Code:

(defun org-next-slide () ;; move forward to the next top-level heading
  (interactive)
  (beginning-of-buffer) ;; navigate to the beginning of this header
  (forward-char)
  (widen) ;; expand to see the whole buffer
  (if (search-forward-regexp ;; if we can find another header,
       (rx line-start "* ") nil t)
      (progn
        (org-narrow-to-subtree) ;; go to it, narrow to it,
        (show-all)) ;; and show all its sub-trees
    (progn
      (org-narrow-to-subtree) ;; otherwise let the user know we're
      (beginning-of-buffer) ;; on the last slide
      (message "Last slide"))))

(defun org-prev-slide () ;; as `org-next-slide`, but searching backwards
  (interactive)
  (beginning-of-buffer)
  (widen)
  (if (search-backward-regexp
       (rx line-start "* ") nil t)
      (progn
        (org-narrow-to-subtree)
        (show-all))
    (progn
      (org-narrow-to-subtree)
      (beginning-of-buffer)
      (message "First slide"))))

(defvar org-slides-mode-keymap ;; bind functions to page-up and page-down
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<next>") 'org-next-slide)
    (define-key map (kbd "<prior>") 'org-prev-slide)
    map))

(define-minor-mode org-slides-mode ;; Synthesize into a minor mode
  "View org-mode sub-trees as slides."
  :lighter " Slides"
  :keymap org-slides-mode-keymap
  (progn
    (set-face-attribute 'default nil :height 300) ;; big font
    (set-variable 'left-margin-width '2 t) ;; little margin
    (set-window-buffer (selected-window) (current-buffer)))) ;; don't change windows

(provide 'org-slides-mode)
;;; org-slides-mode.el ends here
