;; -*- no-byte-compile: t; -*-
;;; private/tees/packages.el

(package! rjsx-mode)
(package! doom-themes)
(package! prettier-js)
(package! browse-at-remote)
(package! yaml-mode)
(package! markdown-mode)


;; This might be slowing boot ... should make this happen on hooks...
(def-package! rjsx-mode        :demand t :config)
;; (def-package! doom-themes      :demand t :config) ;; I think this is already installed
(def-package! prettier-js      :demand t :config)
(def-package! browse-at-remote :demand t :config)
(def-package! yaml-mode        :demand t :config)
(def-package! markdown-mode    :demand t :config)
