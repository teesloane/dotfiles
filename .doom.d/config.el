;; -- General variables --
;;
(menu-bar-mode t)
(fringe-mode 0)

(setq-default
 avy-all-windows        'all-frames
 doom-font              (font-spec :family "JetBrains Mono" :size 13 :weight 'regular)
 doom-theme             'doom-nord
 which-key-idle-delay   0.2
 global-whitespace-mode 0
 line-spacing 2
 which-key-idle-delay   0.2

 ;; Org mode stuff
 deft-directory         "~/Dropbox/wiki"

 ;; Web stuff
 js2-bounce-indent-p nil
 js2-highlight-level 3
 js2-basic-offset 2
 js-indent-level 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-style-padding 2
 web-mode-script-padding 2
 css-indent-offset 2
 writeroom-width 90

 ;; Tool stuff
 counsel-rg-base-command "rg -i -M 160 --no-heading --line-number --color never %s .") ;; stop rg crashing on long files.

;;
;; -- Custom Packages
;;

;; TODO THIS NEEDS TO RETURN A FUNCTION because :file expects a single param func
(defun org-roam--title-maker (title)
  (let ((timestamp     (format-time-string "%Y-%m-%d--%H-%M" (current-time)))
        (slug (org-roam--title-to-slug title)))
      (format "FIXME____%s_%s" timestamp slug)))

(use-package! org-roam
  :after org
  :config
  (setq-default
   org-roam-directory "~/Dropbox/wiki"
   org-roam-link-title-format "%sº"
   org-roam-templates
   ;; templates for creating a new note!
   (let* ((timestamp     (format-time-string "%Y-%m-%d--%H-%M" (current-time)))
          (title         "#+TITLE: ${title}")
          (slug          (lambda (title) (org-roam--title-to-slug title)))
          (nl            "\n")
          (section       "#+SECTION: nil")
          (date_created  (concat "#+DATE_CREATED: " timestamp))
          (template      (concat title nl section nl date_created)) ;; FIXME: handle file name depending on template
          (title-maker   (lambda (title) (format "FIXME-%s_%s" timestamp "fixme"))))

      (list (list "default" (list :file #'org-roam--file-name-timestamp-title
                                  :content template))
            (list "private" (list :file  #'org-roam--title-maker
                                  :content template)))))

  (org-roam-mode)
  (org-roam--build-cache-async)

  (map! :map org-mode-map
        :localleader
        (:prefix ("R" . "Org Roam")
          "o"   #'org-roam
          "f"   #'org-roam-find-file
          "g"   #'org-roam-show-graph
          "i"   #'org-roam-insert)))

;; -- Custom Bindings ----------------------------------------------------------doom-one
;;

(map!
 ;; -- <GLOBAL> ---------------------------------------------------------------

 :desc "Switch to 1st workspace" :n  "s-1"   (λ! (+workspace/switch-to 0))
 :desc "Switch to 2nd workspace" :n  "s-2"   (λ! (+workspace/switch-to 1))
 :desc "Switch to 3rd workspace" :n  "s-3"   (λ! (+workspace/switch-to 2))
 :desc "Switch to 4th workspace" :n  "s-4"   (λ! (+workspace/switch-to 3))
 :desc "Switch to 5th workspace" :n  "s-5"   (λ! (+workspace/switch-to 4))
 :desc "Switch to 6th workspace" :n  "s-6"   (λ! (+workspace/switch-to 5))
 :desc "Switch to 7th workspace" :n  "s-7"   (λ! (+workspace/switch-to 6))
 :desc "Switch to 8th workspace" :n  "s-8"   (λ! (+workspace/switch-to 7))
 :desc "Switch to 9th workspace" :n  "s-9"   (λ! (+workspace/switch-to 8))
 :desc "Create workspace"        :n  "s-t"   (λ! (+workspace/new))

 ;; -- <LEADER> ----------------------------------------------------------------
 (:leader
   (:desc "tees" :prefix "v"
     :desc "M-X Alt"             :n "v" #'execute-extended-command)


   (:desc "lisp" :prefix "k"
     :desc "sp-copy"              :n "c" #'sp-copy-sexp
     :desc "sp-kill"              :n "k" #'sp-kill-sexp
     :desc "sp-slurp"             :n "S" #'sp-forward-slurp-sexp
     :desc "sp-barf"              :n "B" #'sp-forward-barf-sexp
     :desc "sp-up"                :n "u" #'sp-up-sexp
     :desc "sp-down"              :n "d" #'sp-down-sexp
     :desc "sp-next"              :n "l" #'sp-next-sexp
     :desc "sp-prev"              :n "h" #'sp-previous-sexp)))


;; -- Hooks --------------------------------------------------------------------

(add-hook! 'doom-load-theme-hook
  (after! outline
    (set-face-attribute 'outline-1 nil :height 1.0 :background nil)))

;; -- Local file requires --

(load! "+funcs")
(load! "+org")

;; -- Enable gpg stuff ---------------------------------------------------------

(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption nil) ; disable caching of passphrases.
