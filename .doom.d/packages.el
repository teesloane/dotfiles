;; -*- no-byte-compile: t; -*-
;;; ~/Development/dotfiles/doom/packages.el


(package! writeroom-mode)
(package! org-super-agenda)
(package! olivetti)
(package! solaire-mode :disable t)
(package! emojify)
(package! org-roam
  :recipe (:host github :repo "jethrokuan/org-roam"))

;; Borrowed from: https://github.com/fuxialexander/doom-emacs-private-xfu/blob/9ab46f036fda6bce0f8497c47ccb5b8b06d9fdae/gc.org
;;
(package! pretty-magit
  :recipe (:local-repo "~/.doom.d/local"))
  ;; :commands (pretty-magit)
  ;; (pretty-magit "Feature" ? '(:foreground "slate gray" :height 1.0 :family "FontAwesome"))
  ;; (pretty-magit "Add" ? '(:foreground "#375E97" :height 1.0 :family "FontAwesome"))
  ;; (pretty-magit "Fix" ? '(:foreground "#FB6542" :height 1.0 :family "FontAwesome"))
  ;; (pretty-magit "Clean" ? '(:foreground "#FFBB00" :height 1.0 :family "FontAwesome"))
  ;; (pretty-magit "Docs" ? '(:foreground "#3F681C" :height 1.0 :family "FontAwesome"))
  ;; (pretty-magit "master" ? '(:box nil :height 1.0 :family "github-octicons") t)
  ;; (pretty-magit "origin" ? '(:box nil :height 1.0 :family "github-octicons") t))
