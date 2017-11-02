;;; private/tees/config.el -*- lexical-binding: t; -*-

;; TODO: add cmd+z / cmd+shift+z undo redo

;; Other / Files to lad
(load! +bindings)

;; MODES
(def-package! flx :demand t)
(def-package! prettier-js    :mode "\\.js$"           :config)
(def-package! js-import      :commands js-import      :config)
(def-package! rjsx-mode      :commands rjsx-mode      :config)
(def-package! writeroom-mode :commands writeroom-mode :config)

;; Setup modes as necessary (ugh)
(push '("\\.js\\'" . rjsx-mode)   auto-mode-alist)
(push '("\\.css\\'" . web-mode)   auto-mode-alist)
(push '("\\.sass\\'" . sass-mode) auto-mode-alist)


;; DEFAULTS
(setq-default
    ;; GENERAL STUFF
    line-spacing 0.8
    tab-width 2
    indent-tab-mode nil
    which-key-idle-delay 0.3
    initial-major-mode 'org-mode      ;; set scratch buffer to org

    ;; JAVASCRIPT STUFF
    js2-bounce-indent-p nil
    js2-highlight-level 3
    js2-basic-offset 2
    js-indent-level 2
    web-mode-markup-indent-offset 2
    web-mode-css-indent-offset 2
    web-mode-code-indent-offset 2
    web-mode-style-padding 2
    web-mode-script-padding 2

    ;; PLUGINS
    avy-all-windows t
)

;; Local Vars?
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))) ;; make ivy fuzzy search


;; hooks
(defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)
