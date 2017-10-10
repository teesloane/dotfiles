;;; private/tees/config.el -*- lexical-binding: t; -*-

;; TODO: avy jump between buffers
;; TODO: neotree adjustable or add treemacs.
;; TODO: version control for stuff
;; TODO: org code snippetj

;; PACKAGES 
(def-package! rjsx-mode   :demand t :config)
(def-package! emojify     :demand t :config)
(def-package! doom-themes :demand t :config)
(def-package! prettier-js :demand t :config)

;; MODES
(push '("\\.js\\'" . rjsx-mode) auto-mode-alist)

;; UI
(setq-default 
    line-spacing 0.5
    tab-width 2
    which-key-idle-delay 0.3
    js2-bounce-indent-p nil
    js2-highlight-level 3
    js2-basic-offset 2
    js-indent-level 2
)


;; org mode

(add-to-list 'org-structure-template-alist
             '("s" "#+NAME: ?\n#+BEGIN_SRC \n\n#+END_SRC"))

;; KEYBINDINGS

(map!
 (:leader
    :desc "project-search"        :nv "/" #'counsel-rg))


