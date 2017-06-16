;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()

   ;; ----------------------------------------------------------------
   ;; ### --- LAYERS --- ###
   ;; ----------------------------------------------------------------
   dotspacemacs-configuration-layers
   '(
     ruby
     yaml
     go
     javascript
     elm
     html
     helm
     auto-completion
     better-defaults
     emacs-lisp
     github
     git
     markdown
     org
     deft
     osx
     shell
     react
     ;; themes-megapack
     colors
     syntax-checking
     version-control
     erc
     )

   dotspacemacs-additional-packages '(
                                      js-import
                                      centered-window-mode
                                      all-the-icons
                                      spaceline-all-the-icons
                                      doom-themes
                                    )

   dotspacemacs-frozen-packages '()    ;; A list of packages that cannot be updated.
   dotspacemacs-excluded-packages '()    ;; A list of packages that will not be installed and loaded.
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization function. DONT CHANGE THIS, besides modifying the variable values."

  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'doge
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(brin spacemacs-dark spacemacs-light)

   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t

   dotspacemacs-default-font '("Fira Code"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1)

   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1  ; size in mb

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'original

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup `all
   ))

(defun dotspacemacs/user-init ()
  "It is called immediately after `dotspacemacs/init', before layer configuration executes.
  This function is mostly useful for variables that need to be set
  before packages are loaded.

  If you are unsure, you should try in setting them in `dotspacemacs/user-config' first."
)

(defun dotspacemacs/user-config ()   "PUT YOUR CODE HERE YO"
  ;; REQUIREMENTS
  (require 'spaceline-all-the-icons)

  ;; DEFAULTS
  (global-hl-line-mode -1)            ; Disable current line highlight
  (global-linum-mode)                 ; Show line numbers by default
  (global-vi-tilde-fringe-mode -1)    ; turn off le fringe ~~~~ things
  (global-visual-line-mode t)         ; break word on wrap
  (spacemacs/toggle-indent-guide-globally-on) ; turn on the line wrap

  (setq-default
   line-spacing 0.8
   line-spacing 0.8
   spaceline-all-the-icons-theme t
   spaceline-all-the-icons
   spaceline-all-the-icons-separator-type 'arrow
   )

  (setq
   create-lockfiles nil
   require-final-newline t
   persp-add-buffer-on-after-change-major-mode t
   neo-theme 'icons
  )

  ;; ==========================================
  ;; THEME SETTINGS
  ;; ==========================================


  (use-package spaceline-all-the-icons
    :after spaceline
    :config (spaceline-all-the-icons-theme))

  ;; settings for doom theme https://github.com/hlissner/emacs-doom-theme
  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
  (doom-themes-nlinum-config)   ; requires nlinum and hl-line-mode | Line highlighting

  ;; ==========================================
  ;; Mode settings
  ;; ==========================================

  ;; js2, css mode settings
  (setq
    js2-strict-missing-semi-warning nil
    js2-pretty-multiline-declarations 'all
    js2-bounce-indent-p nil
    js2-highlight-level 3
    js2-mode-indent-ignore-first-tab nil
    js2-mode-show-parse-errors nil
    js2-mode-show-strict-warnings nil
    setq css-indent-offset 2
   )

  (setq-default
   js2-basic-offset 2
   js-indent-level 2
   )

  (push '("\\.js\\'" . react-mode) auto-mode-alist)


  ;; SOME COOL INDENT TIMES!     "Hooks for Web mode."
  (defun my-web-mode-hook ()
    (setq
     web-mode-markup-indent-offset 2
     web-mode-css-indent-offset 2
     web-mode-code-indent-offset 2
     web-mode-style-padding 2
     web-mode-script-padding 2
    ))
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  ;; disable jshint since we prefer eslint checking
  (with-eval-after-load "flycheck"
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint))))

  ;; =================================
  ;; ORG MODE + Deft
  ;; =================================

  (defun my/org-mode-hook ()
    ;; Stop the org-level headers from increasing in height relative to the other text.
    (dolist (face '(org-level-1
                    org-level-2
                    org-level-3
                    org-level-4
                    org-level-5))
      (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
  (add-hook 'org-mode-hook 'my/org-mode-hook)

  ;; SETUP DEFT
  (setq
   deft-auto-save-interval 20)         ; Prevent unecessary auto save / cursor movement
   deft-directory "~/Dropbox/notes/"   ; set save directory to dropbox notes.
   deft-extensions '("txt" "md" "org")
   )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(linum-format " %5i ")
 '(neo-theme (quote icons) t)
 '(package-selected-packages
   (quote
    (erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks deft doom-themes diminish packed evil helm helm-core avy projectile async s spaceline-all-the-icons memoize font-lock+ origami flycheck-elm elm-mode magit-gh-pulls github-search github-clone github-browse-file gist gh marshal logito pcache ht all-the-icons rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby centered-window-mode js-import zonokai-theme zenburn-theme zen-and-art-theme yaml-mode xterm-color web-mode web-beautify underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme slim-mode shell-pop seti-theme scss-mode sass-mode reverse-theme reveal-in-osx-finder rainbow-mode rainbow-identifiers railscasts-theme purple-haze-theme pug-mode professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pbcopy pastels-on-dark-theme osx-trash osx-dictionary organic-green-theme org alert log4e gntp omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme multi-term monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme markdown-mode majapahit-theme lush-theme livid-mode skewer-mode simple-httpd light-soap-theme less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme helm-css-scss hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme go-guru go-eldoc gitignore-mode fringe-helper git-gutter+ git-gutter gandalf-theme flyspell-correct-helm flyspell-correct pos-tip flycheck flatui-theme flatland-theme firebelly-theme farmhouse-theme magit magit-popup git-commit with-editor espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web web-completion-data company-tern dash-functional tern company-go go-mode company color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme yasnippet auto-dictionary apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme auto-complete ws-butler window-numbering volatile-highlights vi-tilde-fringe uuidgen toc-org spaceline powerline restart-emacs rainbow-delimiters popwin persp-mode paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-ediff evil-args evil-anzu anzu eval-sexp-fu highlight dumb-jump f define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol aggressive-indent adaptive-wrap ace-link which-key use-package spacemacs-theme smeargle quelpa pcre2el orgit org-projectile org-present org-pomodoro org-download mwim mmm-mode markdown-toc magit-gitflow macrostep hydra htmlize help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md flycheck-pos-tip exec-path-from-shell evil-visualstar evil-magit evil-escape elisp-slime-nav diff-hl company-statistics bind-map auto-yasnippet auto-compile ace-window ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
