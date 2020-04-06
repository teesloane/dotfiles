;; -*- no-byte-compile: t; -*-
;;; ~/Development/dotfiles/doom/packages.el


(package! writeroom-mode)
(package! org-super-agenda)
(package! olivetti)
(package! solaire-mode :disable t)
(package! org-roam
  :recipe (:host github :repo "jethrokuan/org-roam"))

(package! pretty-magit :recipe (:local-repo "~/.doom.d/local"))
