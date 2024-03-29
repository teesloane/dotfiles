;; Startup funcs


;;; Startup Calls --
;; unbreak doom on emacs@29
(general-auto-unbind-keys :off)

(remove-hook 'doom-after-init-modules-hook #'general-auto-unbind-keys)
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/libgccjit/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin20/10.2.0")

;; no menu bar
;; (add-to-list 'default-frame-alist '(undecorated . t))

(menu-bar-mode t)
(fringe-mode 0)
(global-auto-revert-mode 1) ; enable for reloading files when they change on disk. Used for sync thing.

;;; Variable overrides --

(setq-default
 _wiki-path                     "~/Sync/wiki/"
 avy-all-windows                'all-frames
 deft-directory                 _wiki-path
 projectile-project-search-path '("~/Projects" "~/Development")
 vterm-module-cmake-args       "-DUSE_SYSTEM_LIBVTERM=no" ; keep vterm happy
 time-stamp-active             t
 time-stamp-format             "%04y-%02m-%02d %02H:%02M:%02S"
 which-key-idle-delay          0.3
 ;; counsel-rg-base-command       "rg -i -M 160 --no-heading --line-number --color never %s ." ;; stop rg crashing on long files.
 )

(setq org-babel-default-header-args '((:results . "replace") (:comments . "org")))

;;; Webby web web

(setq-default
 css-indent-offset             2
 js-indent-level               2
 js2-basic-offset              2
 js2-bounce-indent-p           nil
 js2-highlight-level           3
 web-mode-code-indent-offset   2
 web-mode-css-indent-offset    2
 web-mode-markup-indent-offset 2
 web-mode-script-padding       2
 web-mode-style-padding        2
 )

;; Doom things
(setq dark-theme 'doom-opera
      light-theme 'doom-flatwhite)

(setq-default
 global-whitespace-mode        0
 line-spacing                  2
  ;; doom-font                     (font-spec :family "IBM Plex Mono" :size 13 :weight 'regular)
 ;; doom-font                     (font-spec :family "Fantasque Sans Mono" :size 13)
 doom-font                     (font-spec :family "JetBrains Mono" :size 12)
 ;; doom-font                     (font-spec :family "Iosevka" :size 14 :weight 'regular)
 doom-variable-pitch-font      (font-spec :family "JetBrains Mono" :size 12)
 +zen-text-scale               0
 doom-theme                    dark-theme)

(after! lsp-mode
  (setq lsp-ui-doc-enable nil))

;; https://is.gd/3VuSXj
(defface org-checkbox-done-text
  '((t (:foreground "#5a637b")))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords 'org-mode
                        '(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
                           1 'org-checkbox-done-text prepend))
                        'append)

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme light-theme t))
    ('dark (load-theme dark-theme t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (remove-hook hook 'highlight-indent-guides-mode))

;;; Org Mode --
(setq
 org-agenda-files              '("~/Sync/wiki/inbox.org" "~/Sync/wiki/projects.org")
 org-default-notes-file        (concat _wiki-path "inbox.org")
 org-directory                  "~/Sync/wiki/" ; _wiki-path
 org-link-file-path-type       'relative
 )

;;; Org: general variable setting --

(after! org
  ;; org variables not related to directories.
  (setq
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets                     '((org-agenda-files :maxlevel . 2))
   org-refile-use-outline-path            'file ; Show/full/paths for refiling
   org-attach-id-dir                   "data/attachments/"
   org-startup-folded                  t
   org-log-done                        t
   org-log-into-drawer                 t
   org-outline-path-complete-in-steps  nil ; refile easy
   ))

(defun org-find-month-in-datetree()
  (org-datetree-find-date-create (calendar-current-date))
  (kill-line))

(setq my-org-capture-templates '(("i" "Inbox" entry (file "inbox.org") "* %?\n%i\n" :prepend t :kill-buffer t)
                                 ("l" "Log" entry (file+datetree "priv/log.org.gpg") "**** %U %^{Title} %(org-set-tags-command) \n%?" :prepend t)
                                 ;; ("c" "Chronolog" entry (file+headline "chronolog.org" "The Chronolog") "** %u - %?\nSCHEDULED: %T" :prepend t)
                                 ("c" "Chronolog" entry (file+datetree "chronolog.org") "**** %U %^{Title}\n%?" :prepend t)
                                 ("t" "Todo" entry (file "inbox.org") "* TODO %?\n%i" :prepend t)
                                 ("T" "Todo Today" entry (file+headline "inbox.org" "Scheduled") "** TODO %?\n%i\nSCHEDULED: %T" :prepend t)
                                 ("S" "Todo Scheduled" entry (file+headline "inbox.org" "Scheduled") "** TODO %?\n%i" :prepend t)
                                 ("b" "New Book" entry (file+headline "books/index.org" "Reading")
"** %^{Author} - %^{Title}
:PROPERTIES:
:author: %\\1
:title: %\\2
:pages: %^{Pages}
:page: 0
:date_started: %U
:date_completed:
:genre:
:year:
:type: %^{Type|Novel|Graphic Novel|Manga|Short Stories|Poetry|Other}
:rating: 0
:END:
"
:prepend t :kill-buffer t)))

(after! org
  (setq org-capture-templates my-org-capture-templates))

(setq
 org-pomodoro-finished-sound-args "-volume 0.3"
 org-pomodoro-finished-sound-args "-volume 0.3"
 org-pomodoro-long-break-sound-args "-volume 0.3"
 org-pomodoro-short-break-sound-args "-volume 0.3"
 )

;; Org general settings / ui

(after! org
  (setq
   line-spacing                           3
   org-cycle-separator-lines 2
   org-startup-truncated                  t
   org-startup-folded                     t
   org-ellipsis                           " ⋱ " ;; " • " ;; " ⇢ " ;; " ⋱ " ;;
   org-list-demote-modify-bullet          '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))
   org-fontify-whole-heading-line         nil
   org-tags-column                        65
   org-image-actual-width                 400 ; set the width of inline images.
   ;; Habit glyphs
   org-habit-completed-glyph              ?x
   org-habit-show-all-today               t
   org-habit-preceding-days               7
   org-habit-today-glyph                  ?!
   ))

(add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode)

(after! mixed-pitch
  (pushnew! mixed-pitch-fixed-pitch-faces
            'org-level-1 'org-level-2 'org-level-3
            'org-level-4 'org-level-5 'org-level-6
            'org-level-7 'org-link
            )
  )

(add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode)

(after! org
  (setq-default
   org-todo-keyword-faces
   '(
     ("DONE"       :foreground "#7c7c75") ; :weight normal :underline t)
     ("[X]"        :foreground "#7c7c75") ;add-face :weight normal :underline t)
     ("PROJ"       :foreground "#7c7c75") ; :weight normal :underline t)
     ("WAIT"       :foreground "#9f7efe") ; :weight normal :underline t)
     ("[?]"        :foreground "#9f7efe") ; :weight normal :underline t)
     ("STRT"       :foreground "#0098dd") ; :weight normal :underline t)
     ("NEXT"       :foreground "#0098dd") ; :weight normal :underline t)
     ("TODO"       :foreground "#50a14f") ; :weight normal :underline t)
     ("[ ]"        :foreground "#50a14f") ; :weight normal :underline t)
     ("HOLD"       :foreground "#ff6480") ; :weight normal :underline t)
     ("[-]"        :foreground "#ff6480") ; :weight normal :underline t)
     ("ABRT"       :foreground "#ff6480") ; :weight normal :underline t)
     )))

(after! org
  (appendq! +ligatures-extra-symbols
            `(:checkbox      "☐"
              :pending       "❍"
              :checkedbox    "☒"
              :list_property "∷"
              :results       "🠶"
              :begin_quote   "❮"
              :end_quote     "❯"
              :begin_export  "⯮"
              :end_export    "⯬"
              ;; :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
              ;; :priority_b   ,(propertize "🡅" 'face 'all-the-icons-orange)
              ;; :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
              ;; :priority_d   ,(propertize "🡇" 'face 'all-the-icons-green)
              ;; :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)
              :em_dash       "—"))
  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :results       "#+results:"
    :begin_quote   "#+begin_quote"
    :end_quote     "#+end_quote"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    ;; :priority_a    "[#A]"
    ;; :priority_b    "[#B]"
    ;; :priority_c    "[#C]"
    ;; :priority_d    "[#D]"
    ;; :priority_e    "[#E]"
    :em_dash       "---"))
(plist-put +ligatures-extra-symbols :name "⁍") ; or › could be good?

(custom-set-faces
  '(org-block-begin-line ((t (:background nil))))
  '(org-block-end-line   ((t (:background nil)))))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  (setq  org-superstar-prettify-item-bullets t))

;; Org Roam Config

(add-to-list '+evil-collection-disabled-list 'org-roam)

(defun tees/org-roam-template-head (file-under)
  (concat "#+TITLE: ${title}\n#+DATE_CREATED: <> \n#+DATE_UPDATED: <> \n#+FIRN_UNDER: " file-under "\n#+FIRN_LAYOUT: default\n\n"))

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init
  (setq org-roam-directory "~/Sync/wiki/"
        org-roam-db-location "~/.org/org-roam.db"
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
  (setq +org-roam-open-buffer-on-find-file nil)
  (setq org-roam-capture-templates
        `(("p" "project" entry (function org-roam--capture-get-point)
           ;; "r Entry item!"
           (file "~/.doom.d/templates/org-roam-project.org")
           :file-name "${slug}"
           :head ,(tees/org-roam-template-head "project")
           :unnarrowed t)
          ("r" "blog" entry (function org-roam--capture-get-point)
           ;; "r Entry item!"
           (file "~/.doom.d/templates/org-roam-research.org")
           :file-name "${slug}"
           :head ,(tees/org-roam-template-head "blog")
           :unnarrowed t)

          ("r" "research" entry (function org-roam--capture-get-point)
           ;; "r Entry item!"
           (file "~/.doom.d/templates/org-roam-research.org")
           :file-name "${slug}"
           :head ,(tees/org-roam-template-head "research")
           :unnarrowed t)
          ("l" "log" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "log/%<%Y-%m-%d-%H%M>-${slug}"
           :head ,(tees/org-roam-template-head "log")
           :unnarrowed t)
          ("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head ,(tees/org-roam-template-head "general")
           :unnarrowed t)))
  )

;;; Custom Bindings --

(map!
 ;; -- <GLOBAL> --
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

; ; -- <LEADER> --

 (:leader
    (:desc "tees" :prefix "v"
     :desc "M-X Alt"                   :n "v" #'execute-extended-command
     :desc "Focus it"                  :n "f" #'focus-mode
     :desc "Expand region"             :n "e" #'er/expand-region
     :desc "Hydra-Clock"               :n "c" #'tees/hydra-org-clock/body
     :desc "Hydra-Workspaces"          :n "w" #'tees/hydra-workspace-nav/body
     :desc "Hydra-Agenda"              :n "a" #'tees/hydra-org-agenda/body
     :desc "Hydra-Windows"             :n "l" #'tees/hydra-winnav/body
     :desc "Correct Spelling at Point" :n "s" #'flyspell-correct-word-before-point
     :desc "Correct Spelling at Point" :n "s" #'flyspell-correct-word-before-point
     )

    ;; additional org roam bindings to `SPC n`
    (:prefix-map ("n" . "notes")
      :desc "Org-Roam-Find"                "/" #'org-roam-find-file
       ;; :desc "Browse notes"                "/" #'+default/browse-notes

        )

    (:prefix-map ("k" . "lisp")
      :desc "sp-copy"              :n "c" #'sp-copy-sexp
      :desc "sp-kill"              :n "k" #'sp-kill-sexp
      :desc "sp-slurp"             :n "S" #'sp-forward-slurp-sexp
      :desc "sp-barf"              :n "B" #'sp-forward-barf-sexp
      :desc "sp-up"                :n "u" #'sp-up-sexp
      :desc "sp-down"              :n "d" #'sp-down-sexp
      :desc "sp-next"              :n "l" #'sp-next-sexp
      :desc "sp-prev"              :n "h" #'sp-previous-sexp)))

(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)

;; mac switch meta key
(defun mac-switch-meta nil
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
	(setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'hyper)
	)
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta)
      )
    )
  )

(defhydra tees/hydra-org-agenda (:pre (setq which-key-inhibit t)
                            :post (setq which-key-inhibit nil)
                            :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

;;; Hydras

(defhydra tees/hydra-winnav (:color red)
  ("s" shrink-window-horizontally "shrink horizontally" :column "Sizing")
  ("e" enlarge-window-horizontally "enlarge horizontally")
  ("b" balance-windows "balance window height")
  ("m" maximize-window "maximize current window")
  ("M" minimize-window "minimize current window")

  ("H" split-window-below "split horizontally" :column "Split management")
  ("v" split-window-right "split vertically")
  ("d" delete-window "delete current window")
  ("x" delete-other-windows "delete-other-windows")


  ("z" ace-window "ace window" :color blue :column "Navigation")
  ("h" windmove-left "← window")
  ("j" windmove-down "↓ window")
  ("k" windmove-up "↑ window")
  ("l" windmove-right "→ window")
  ("r" toggle-window-split "rotate windows") ; Located in utility functions
  ("q" nil "quit menu" :color blue :column nil))

(defhydra tees/hydra-workspace-nav (:color red)
  ("s" +workspace/display "Show workspaces" )
  ("h" +workspace/switch-left "Go left" )
  ("l" +workspace/switch-right "Go left" )
  ("n" +workspace/new "New" )
  ("d" +workspace/delete "Delete" )
  ("r" +workspace/rename "Rename" )
  ("q" nil "quit menu" :color blue :column nil))

(defhydra tees/hydra-org-clock (:color blue :hint nil)
  "
Clock   In/out^     ^Edit^    ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_ffort _g_oto entry
        _c_ontinue   _q_uit   _d_isplay
        _o_ut        ^ ^      _r_eport
      "
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands")))

;;;  Hooks --

;; update timestamp, if it exists, when saving
(add-hook 'write-file-hooks 'time-stamp)

;; Don't show line numbers in writeroom mode.
(add-hook! 'writeroom-mode-hook
  (display-line-numbers-mode (if writeroom-mode -1 +1)))

(setq
 lsp-lens-enable nil
 lsp-ui-imenu-auto-refresh t
 lsp-ui-sideline-show-code-actions nil
 lsp-zig-zls-executable "/usr/local/bin/zls"
 )

(after! cider
  (add-hook 'company-completion-started-hook 'custom/set-company-maps)
  (add-hook 'company-completion-finished-hook 'custom/unset-company-maps)
  (add-hook 'company-completion-cancelled-hook 'custom/unset-company-maps))

(defun custom/unset-company-maps (&rest unused)
  "Set default mappings (outside of company).
    Arguments (UNUSED) are ignored."
  (general-def
    :states 'insert
    :keymaps 'override
    "<down>" nil
    "<up>"   nil
    "RET"    nil
    [return] nil
    "C-n"    nil
    "C-p"    nil
    "C-j"    nil
    "C-k"    nil
    "C-h"    nil
    "C-u"    nil
    "C-d"    nil
    "C-s"    nil
    "C-S-s"   (cond ((featurep! :completion helm) nil)
                    ((featurep! :completion ivy)  nil))
    "C-SPC"   nil
    "TAB"     nil
    [tab]     nil
    [backtab] nil))

(defun custom/set-company-maps (&rest unused)
  "Set maps for when you're inside company completion.
    Arguments (UNUSED) are ignored."
  (general-def
    :states 'insert
    :keymaps 'override
    "<down>" #'company-select-next
    "<up>" #'company-select-previous
    "RET" #'company-complete
    [return] #'company-complete
    "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
    "C-n"     #'company-select-next
    "C-p"     #'company-select-previous
    "C-j"     #'company-select-next
    "C-k"     #'company-select-previous
    "C-h"     #'company-show-doc-buffer
    "C-u"     #'company-previous-page
    "C-d"     #'company-next-page
    "C-s"     #'company-filter-candidates
    "C-S-s"   (cond ((featurep! :completion helm) #'helm-company)
                    ((featurep! :completion ivy)  #'counsel-company))
    "C-SPC"   #'company-complete-common
    "TAB"     #'company-complete-common-or-cycle
    [tab]     #'company-complete-common-or-cycle
    [backtab] #'company-select-previous    ))

(setenv "PATH" (concat (getenv "PATH") "~/Development/go/bin"))

(use-package! zig-mode
  :hook ((zig-mode . lsp-deferred))
  :custom (zig-format-on-save nil)
  :config
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
    (lsp-register-client
      (make-lsp-client
        :new-connection (lsp-stdio-connection "/usr/local/bin/zls")
        :major-modes '(zig-mode)
        :server-id 'zls))))

;; Configure elixir-lsp
;; replace t with nil to disable.
(setq lsp-elixir-fetch-deps nil)
(setq lsp-elixir-suggest-specs nil)
(setq lsp-elixir-signature-after-complete t)
(setq lsp-elixir-enable-test-lenses t)


;; Compile and test on save
;; (setq alchemist-hooks-test-on-save t)
(setq alchemist-hooks-compile-on-save t)

;; Disable popup quitting for Elixir’s REPL
;; Default behaviour of doom’s treating of Alchemist’s REPL window is to quit the
;; REPL when ESC or q is pressed (in normal mode). It’s quite annoying so below
;; code disables this and set’s the size of REPL’s window to 30% of editor frame’s
;; height.
(set-popup-rule! "^\\*Alchemist-IEx" :quit nil :size 0.35)

(set-popup-rule! "^\\*exunit-compilation*" :quit nil :size 0.35)
;; Do not select exunit-compilation window
(setq shackle-rules '(("*exunit-compilation*" :noselect t))
      shackle-default-rule '(:select t))

;; Set global LSP options
(after! lsp-mode (
setq lsp-lens-enable nil
lsp-ui-peek-enable t
lsp-ui-doc-enable nil
lsp-ui-doc-position 'bottom
lsp-ui-doc-max-height 70
lsp-ui-doc-max-width 150
lsp-ui-sideline-show-diagnostics t
lsp-ui-sideline-show-hover nil
lsp-ui-sideline-show-code-actions t
lsp-ui-sideline-diagnostic-max-lines 20
lsp-ui-sideline-ignore-duplicate t
lsp-ui-sideline-enable t))

(add-hook 'elixir-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

(use-package!
   polymode
  :ensure t
  :mode ("\\.ex\\'" . poly-elixir-web-mode)
  :init (setq web-mode-engines-alist '(("elixir" . "\\.ex\\'")))
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-surface-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~H" (= 3 (char "\"'")) line-end)
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-surface-expr-elixir-innermode)))

(defun tees/file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

(defun tees/copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the current or marked files.

If a buffer is not file and not dired, copy value of `default-directory'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_file_path.html'
Version 2018-06-18 2021-09-30"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath )))))
