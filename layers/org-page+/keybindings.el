(evil-leader/set-key "opp"
  '(lambda () (interactive) "publish your blog automatically"
    (op/do-publication nil nil nil (if (spacemacs/system-is-linux) t nil) nil)))

(evil-leader/set-key "opn" 'op/new-post)
