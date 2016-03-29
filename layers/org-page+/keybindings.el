(evil-leader/set-key "opp"
  '(lambda () (interactive) "publish your blog automatically"
    (op/do-publication nil nil nil t nil)))

(evil-leader/set-key "opn" 'op/new-post)
