
;; -- Startup stuff --

(menu-bar-mode t)
(fringe-mode 0)

;; -- General variables --

(setq-default
 avy-all-windows        'all-frames
 ;; doom-font              (font-spec :family "JetBrains Mono" :size 13 :weight 'regular)
 doom-font              (font-spec :family "Iosevka" :size 14 :weight 'regular)
 which-key-idle-delay   0.2
 doom-theme             'doom-tomorrow-night
 global-whitespace-mode 0
 olivetti-body-width 80
 line-spacing 2
 which-key-idle-delay   0.2
 deft-directory         "~/Dropbox/wiki" ; org mode stuff


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


;; ----- ORG ROAM STUFF ----------------------------------------------------------------

;; (defun org-roam--title-maker (project-type)
;;   "Is used to create the FILE NAME for an org-roam-file.
;;    Returns a func: used by the org-roam-template '(list :file <FUNC>)
;;    @param - project-type"
;;   (lambda (title)
;;     (let ((timestamp     (format-time-string "%Y-%m-%d--%H-%M" (current-time)))
;;           (slug (org-roam--title-to-slug title)))
;;       (if project-type
;;           (format "%s_%s_%s" project-type slug timestamp)
;;           (format "%s_%s" slug timestamp)))))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; (defun org-roam--content-template (project-type)
;;   "Generates the 'front-matter' of the org files for org-roam"
;;   (let* ((timestamp       (format-time-string "%Y-%m-%d--%H-%M" (current-time)))
;;          (nl              "\n")
;;          (title           "#+TITLE: ${title}")
;;          (date_created    (concat "#+DATE_CREATED: " timestamp))
;;          (base-template   (concat title nl date_created nl))
;;          (adv-template    (cond
;;                            ((string= project-type "default") (get-string-from-file "~/.doom.d/templates/org-roam-default.org"))
;;                            ((string= project-type "project") (get-string-from-file "~/.doom.d/templates/org-roam-project.org"))
;;                            ((string= project-type "research") (get-string-from-file "~/.doom.d/templates/org-roam-research.org"))
;;                            (t "")))
;;          (template         (concat base-template adv-template)))
;;     template))

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
        '(("p" "project" entry (function org-roam--capture-get-point)
           ;; "r Entry item!"
           (file "~/.doom.d/templates/org-roam-project.org")
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+STATUS: active \n#+FILE_UNDER: project \n"
           :unnarrowed t)
          ("r" "research" entry (function org-roam--capture-get-point)
           ;; "r Entry item!"
           (file "~/.doom.d/templates/org-roam-research.org")
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+STATUS: active \n#+FILE_UNDER: research \n"
           :unnarrowed t)
          ("d" "default" plain (function org-roam--capture-get-point)
              "%?"
              :file-name "${slug}"
              :head "#+TITLE: ${title}\n"
              :unnarrowed t)))

          ;; ("d" "default" entry (function org-roam--capture-get-point)
          ;;  ;; "r Entry item!"
          ;;  (file "~/.doom.d/templates/org-roam-default.org")
          ;;  :file-name "${slug}"
          ;;  :head "#+TITLE: ${title}\n"
          ;;  :unnarrowed t)))



  (org-roam-mode +1))

  ;; (setq org-roam-templates
  ;;       (list (list "default"   (list :file (org-roam--title-maker nil)
  ;;                                 :content (org-roam--content-template "default")))
  ;;         (list "proj"      (list :file  (org-roam--title-maker "proj")
  ;;                                 :content (org-roam--content-template "project")))
  ;;         (list "research"  (list :file  (org-roam--title-maker "research")
  ;;                                 :content (org-roam--content-template "research")))
  ;;         (list "priv"      (list :file  (org-roam--title-maker "priv"))))))



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

;; (add-hook! 'doom-load-theme-hook
  ;; (after! outline
    ;; (set-face-background 'org-block-begin-line (doom-color 'bg))
    ;; (set-face-background 'org-block (doom-color 'bg-alt))
    ;; (set-face-background 'org-quote (doom-color 'bg-alt))
    ;; (set-face-attribute 'outline-1 nil :height 1.0 :background nil)))

;; -- Local file requires --

(load! "+funcs")
(load! "+org")

;; -- Enable gpg stuff ---------------------------------------------------------

(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption nil) ; disable caching of passphrases.


