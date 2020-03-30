;; Startup stuff --

(menu-bar-mode t)
(fringe-mode 0)
(global-emojify-mode)
;; stop async buffer from popping up (https://emacs.stackexchange.com/a/5554)
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; -- General variables --
;;


(setq-default
 async-shell-command-buffer   'rename-buffer  ; stop async buffer from bothering me when clocking.
 _wiki-path                   "~/Dropbox/wiki/"
 avy-all-windows              'all-frames
 css-indent-offset             2
 deft-directory               _wiki-path
 doom-font                     (font-spec :family "Iosevka" :size 14 :weight 'regular)
 doom-theme                    'doom-tomorrow-night
 global-whitespace-mode        0
 js-indent-level               2
 js2-basic-offset              2
 js2-bounce-indent-p           nil
 js2-highlight-level           3
 line-spacing                  2
 olivetti-body-width           80
 org-attach-id-dir             (concat _wiki-path "attach/")
 org-agenda-files              (list _wiki-path)
 org-default-notes-file        (concat _wiki-path "index.org")
 org-directory                 _wiki-path
 org-link-file-path-type       'relative
 time-stamp-active             t
 time-stamp-format             "%04y-%02m-%02d %02H:%02M:%02S"
 web-mode-code-indent-offset   2
 web-mode-css-indent-offset    2
 web-mode-markup-indent-offset 2
 web-mode-script-padding       2
 web-mode-style-padding        2
 which-key-idle-delay          0.2
 which-key-idle-delay          0.2
 writeroom-width               90

 ;; Tool stuff
 counsel-rg-base-command "rg -i -M 160 --no-heading --line-number --color never %s .") ;; stop rg crashing on long files.

;;
;; -- Custom Packages
;;

;; ----- ORG ROAM STUFF ----------------------------------------------------------------
;;
(defun tees/org-roam-template-head (file-under)
 (concat "#+TITLE: ${title}\n#+FILE_UNDER: " file-under "\n#+DATE_UPDATED: <>\n\n"))

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init
  (setq org-roam-directory "~/Dropbox/wiki"
        org-roam-link-title-format "%sº") ;; appends a  `º` to each Roam link.
  (map!
   :desc "Org-Roam-Insert" "C-c i" #'org-roam-insert
   :desc "Org-Roam-Find"   "C-c n" #'org-roam-find-file
   :leader
   :prefix "n"
   :desc "Org-Roam-Insert" "i" #'org-roam-insert
   :desc "Org-Roam-Find"   "/" #'org-roam-find-file
   :desc "Org-Roam-Buffer" "r" #'org-roam)
  :config
  (setq org-roam-capture-templates
        `(("p" "project" entry (function org-roam--capture-get-point)
           ;; "r Entry item!"
           (file "~/.doom.d/templates/org-roam-project.org")
           :file-name "${slug}"
           :head ,(tees/org-roam-template-head "project")
           :unnarrowed t)
          ("r" "research" entry (function org-roam--capture-get-point)
           ;; "r Entry item!"
           (file "~/.doom.d/templates/org-roam-research.org")
           :file-name "${slug}"
           :head ,(tees/org-roam-template-head "research")
           :unnarrowed t)
          ("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head ,(tees/org-roam-template-head "general")
           :unnarrowed t)))
  (org-roam-mode +1))

;; Make magit render icons for common commit leaders (ex: "Fix:" becomes "")

(use-package! pretty-magit
  :init
  (pretty-magit "Feat" ? '(:foreground "slate gray" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Add" ? '(:foreground "#375E97" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Fix" ? '(:foreground "#FB6542" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Clean" ? '(:foreground "#B5E655" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Docs" ? '(:foreground "#FFBB00" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Test" ? '(:foreground "#4BB5C1" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Start" ? '(:foreground "#2ecc71" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Stop" ? '(:foreground "#e74c3c" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Refactor" ? '(:foreground "#9b59b6" :height 1.0 :family "FontAwesome"))
  (pretty-magit "master" ? '(:box nil :height 1.0 :family "github-octicons") t)
  (pretty-magit "origin" ? '(:box nil :height 1.0 :family "github-octicons") t))


;; -- Custom Bindings ----------------------------------------------------------doom-one

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

    ;; additional org roam bindings to `SPC n`

    (:prefix-map ("k" . "lisp")
      :desc "sp-copy"              :n "c" #'sp-copy-sexp
      :desc "sp-kill"              :n "k" #'sp-kill-sexp
      :desc "sp-slurp"             :n "S" #'sp-forward-slurp-sexp
      :desc "sp-barf"              :n "B" #'sp-forward-barf-sexp
      :desc "sp-up"                :n "u" #'sp-up-sexp
      :desc "sp-down"              :n "d" #'sp-down-sexp
      :desc "sp-next"              :n "l" #'sp-next-sexp
      :desc "sp-prev"              :n "h" #'sp-previous-sexp)))


;; -- Hooks --------------------------------------------------------------------
;;
(add-hook 'write-file-hooks 'time-stamp) ; update timestamp, if it exists, when saving

;; -- Local file requires --

(load! "+funcs")
(load! "+org")

;; -- Enable gpg stuff ---------------------------------------------------------

(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption nil) ; disable caching of passphrases.


