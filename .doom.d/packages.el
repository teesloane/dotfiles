(package! org-super-agenda)
;; (package! solaire-mode :disable tter)
;; (package! pretty-magit :recipe (:local-repo "~/.doom.d/local"))
(package! focus)
(package! parseclj :recipe (:build (:not compile)))
(unpin! cider)
(package! blamer :recipe (:host github :repo "artawower/blamer.el"))

;; (package! nano-emacs :recipe (:host github :repo "rougier/nano-emacs"))

(package! org-bars :recipe (:local-repo "~/.doom.d/lisp/"))
