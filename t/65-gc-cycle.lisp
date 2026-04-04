; This creates an unreachable self-referential list.
; Supplemental mark-sweep should reclaim it after x is cleared.
(setq x (list 1))
(rplacd x x)
(setq x nil)
(prin1 t)
