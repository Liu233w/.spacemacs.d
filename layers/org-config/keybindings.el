;; microstate which help us navigate between headings
(spacemacs|define-micro-state org-config/heading-move
  :doc "`j' next heading same level `k' prev heading same level
`l' next heading visible `h' prev heading visible"
  :use-minibuffer t
  :bindings
  ("j" org-forward-heading-same-level)
  ("k" org-backward-heading-same-level)
  ("l" org-next-visible-heading)
  ("h" org-previous-visible-heading))

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "gg" 'spacemacs/org-config/heading-move-micro-state)
