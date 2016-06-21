;; 这些函数必须在org加载之后才能加载
;; 不能使用require，否则会加载内置的org


(with-eval-after-load 'org
  ;; 在下一级任务的完成度达到100%时自动将上一级设置为DONE
  ;; from http://www.cnblogs.com/holbrook/archive/2012/04/14/2447754.html
  (defun org-config/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  ;; 自动归档
  ;; from https://github.com/CodeFalling/wiki/wiki/Orgmode-as-GTD-system
  (defun org-config//org-archive-tasks (prefix)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     (format "/%s" prefix) 'file))

  (defun org-config/org-archive-all-tasks ()
    (interactive)
    (org-config//org-archive-tasks "DONE")
    (org-config//org-archive-tasks "ABORT")
    )

  ;; 自动整理列表，归类内容（未完成）
  (defun org-config//org-refile-tasks (prefix)
    (let* ((head-name (concatenate 'string "_" prefix))
           (org-refile-targets
            `((nil . (:regexp . ,(concatenate 'string "* " head-name))))))
      (org-map-entries
       (lambda ()
         (when (not (string= head-name (org-find-top-headline)))
           ;; 有没有办法让这个函数不提示而是直接进行操作？
           (org-refile)
           )
         ;; (setq org-map-continue-from (outline-previous-heading))
         )
       (format "/%s" prefix) 'file)))

  ;; 表示归类内容的时候会查找的todo keywords
  (defvar org-config//org-refile-tasks-list
    '("TODO" "NEXT" "WAITTING" "SOMEDAY" "DONE" "ABORT"))

  (require 'cl-lib)
  (defun org-config/org-refile-all-tasks ()
    (interactive)
    (loop for keys in org-config//org-refile-tasks-list
          do (org-config//org-refile-tasks keys)))

)

;; 覆盖默认行为：归档子树时保留树结构
;; from:https://gist.github.com/CodeFalling/87b116291aa87fde72cb
(with-eval-after-load 'org-archive
  ;; org-archive-subtree-hierarchical.el
  ;; modified from https://lists.gnu.org/archive/html/emacs-orgmode/2014-08/msg00109.html

  ;; In orgmode
  ;; * A
  ;; ** AA
  ;; *** AAA
  ;; ** AB
  ;; *** ABA
  ;; Archiving AA will remove the subtree from the original file and create
  ;; it like that in archive target:

  ;; * AA
  ;; ** AAA

  ;; And this give you
  ;; * A
  ;; ** AA
  ;; *** AAA

  (defun org-archive-subtree-hierarchical--line-content-as-string ()
    "Returns the content of the current line as a string"
    (save-excursion
      (beginning-of-line)
      (buffer-substring-no-properties
       (line-beginning-position) (line-end-position))))

  (defun org-archive-subtree-hierarchical--org-child-list ()
    "This function returns all children of a heading as a list. "
    (interactive)
    (save-excursion
      ;; this only works with org-version > 8.0, since in previous
      ;; org-mode versions the function (org-outline-level) returns
      ;; gargabe when the point is not on a heading.
      (if (= (org-outline-level) 0)
          (outline-next-visible-heading 1)
        (org-goto-first-child))
      (let ((child-list (list (org-archive-subtree-hierarchical--line-content-as-string))))
        (while (org-goto-sibling)
          (setq child-list (cons (org-archive-subtree-hierarchical--line-content-as-string) child-list)))
        child-list)))

  (defun org-archive-subtree-hierarchical--org-struct-subtree ()
    "This function returns the tree structure in which a subtree
belongs as a list."
    (interactive)
    (let ((archive-tree nil))
      (save-excursion
        (while (org-up-heading-safe)
          (let ((heading
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))
            (if (eq archive-tree nil)
                (setq archive-tree (list heading))
              (setq archive-tree (cons heading archive-tree))))))
      archive-tree))

  (defun org-archive-subtree-hierarchical ()
    "This function archives a subtree hierarchical"
    (interactive)
    (let ((org-tree (org-archive-subtree-hierarchical--org-struct-subtree))
          (this-buffer (current-buffer))
          (file (abbreviate-file-name
                 (or (buffer-file-name (buffer-base-buffer))
                     (error "No file associated to buffer")))))
      (save-excursion
        (setq location (org-get-local-archive-location)
              afile (org-extract-archive-file location)
              heading (org-extract-archive-heading location)
              infile-p (equal file (abbreviate-file-name (or afile ""))))
        (unless afile
          (error "Invalid `org-archive-location'"))
        (if (> (length afile) 0)
            (setq newfile-p (not (file-exists-p afile))
                  visiting (find-buffer-visiting afile)
                  buffer (or visiting (find-file-noselect afile)))
          (setq buffer (current-buffer)))
        (unless buffer
          (error "Cannot access file \"%s\"" afile))
        (org-cut-subtree)
        (set-buffer buffer)
        (org-mode)
        (goto-char (point-min))
        (while (not (equal org-tree nil))
          (let ((child-list (org-archive-subtree-hierarchical--org-child-list)))
            (if (member (car org-tree) child-list)
                (progn
                  (search-forward (car org-tree) nil t)
                  (setq org-tree (cdr org-tree)))
              (progn
                (goto-char (point-max))
                (newline)
                (org-insert-struct org-tree)
                (setq org-tree nil)))))
        (newline)
        (org-yank)
        (when (not (eq this-buffer buffer))
          (save-buffer))
        (message "Subtree archived %s"
                 (concat "in file: " (abbreviate-file-name afile))))))

  (defun org-insert-struct (struct)
    "TODO"
    (interactive)
    (when struct
      (insert (car struct))
      (newline)
      (org-insert-struct (cdr struct))))

  (defun org-archive-subtree ()
    (interactive)
    (org-archive-subtree-hierarchical)
    )

)
