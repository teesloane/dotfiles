;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()


   ;; ----------------------------------------------------------------
   ;; ### --- LAYERS --- ###
   ;; ----------------------------------------------------------------

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(rust
     racket
     better-defaults
     clojure
     colors
     deft
     ;; elm
     emacs-lisp
     erc
     git
     github
     go
     helm
     html
     javascript
     markdown
     org
     osx
     parinfer
     react
     ruby
     shell
     sql
     syntax-checking
     theming
     version-control
     yaml
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      js-import
                                      centered-window-mode
                                      prettier-js
                                      color-theme-solarized
                                      all-the-icons
                                      spaceline-all-the-icons
                                      doom-themes
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'doge
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type :
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-one
                         brin
                         spacegray
                         spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("fira code"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.3)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'original
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficieng space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup `all
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
   It is called immediately after `dotspacemacs/init', before layer configuration executes.
   Don't know what should go here? put it in `dospacemacs/user-config' first."

  ;; might not be the right place for this.
  (set-face-attribute 'org-level-1 nil :height 1.0 :background nil)

  ;; SETUP DEFT
  (setq deft-auto-save-interval 20)         ; Prevent unecessary auto save / cursor movement
  (setq deft-extensions '("txt" "md" "org"))
  (setq deft-directory "~/Dropbox/notes/"))



(defun dotspacemacs/user-config ()

  ;; ==========================================
  ;; DEFAULTS
  ;; ==========================================

  (global-hl-line-mode -1)                             ; Disable current line highlight
  (global-linum-mode)                                  ; Show line numbers by default
  (global-vi-tilde-fringe-mode -1)                     ; turn off le fringe ~~~~ things
  (global-visual-line-mode t)                          ; break word on wrap
  (spacemacs/toggle-indent-guide-globally-on)          ; turn on the line wrap
  (setq powerline-default-separator 'arrow)
  (setq ns-use-srgb-colorspace nil)                    ; fixes ghosted powerline: https://goo.gl/JfnTtv
  (setq-default line-spacing 0.7)                      ; line height, oh my poor eyes
  (setq create-lockfiles nil)                          ; DISABLE LOCK FILES
  (setq require-final-newline t)                       ; final new line on file
  (setq persp-add-buffer-on-after-change-major-mode t) ; I don't remember what this is
  (setq initial-major-mode 'org-mode)                  ; default scratch pos to org mode

  ;; ==========================================
  ;; SECTION: Modeline stuff
  ;; =========================================
  (use-package spaceline-all-the-icons
    :after spaceline
    :config (spaceline-all-the-icons-theme))
  (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
  (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
  (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
  (spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
  (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
  (spaceline-toggle-all-the-icons-vc-icon-on)
  (spaceline-toggle-all-the-icons-fullscreen-on)
  (spaceline-toggle-all-the-icons-flycheck-status-on)
  (spaceline-toggle-all-the-icons-git-status-on)
  (spaceline-toggle-all-the-icons-vc-icon-on)
  (spaceline-toggle-all-the-icons-mode-icon-on)
  (spaceline-toggle-all-the-icons-package-updates-on)
  (spaceline-toggle-all-the-icons-text-scale-on)
  (spaceline-toggle-all-the-icons-region-info-on)

  (setq anzu-cons-mode-line-p t)
  (global-anzu-mode +1)
  (setq-default spaceline-all-the-icons-separator-type 'arrow)

  ;; ==========================================
  ;; Section: Mode settings ORG MODE
  ;; ==========================================

  ;; add org eval for org code blocks
  (require 'ob-clojure)
  (require 'ob-ruby)


  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (ruby . t)))


  (defun my/org-mode-hook ()
    (interactive)
    (set-face-attribute 'org-level-1 nil :height 1.0)
    (set-face-attribute 'org-level-2 nil :height 1.0)
    (set-face-attribute 'org-level-3 nil :height 1.0)
    (set-face-attribute 'org-level-4 nil :height 1.0)
    )

  (add-hook 'org-load-hook #'my/org-mode-hook)

  ;; ==========================================
  ;; Utilities
  ;; ==========================================
  (global-set-key (kbd "<f5>") 'set-selective-display-dlw)

  (defun fold-dents (&optional level)
    "Fold text indented same of more than the cursor.
    If level is set, set the indent level to LEVEL.
    If 'selective-display' is already set to LEVEL, clicking
    F5 again will unset 'selective-display' by setting it to 0."
    (interactive "P")
    (if (eq selective-display (1+ (current-column)))
        (set-selective-display 0)
      (set-selective-display (or level (1+ (current-column))))))

  ;; ==========================================
  ;; Mode settings CIDER / CLOJURE
  ;; ==========================================
  (require 'cider)
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel! \"dev\" )
           (figwheel-sidecar.repl-api/cljs-repl \"dev\"))")


  ;; ==========================================
  ;; Mode settings javascript etc
  ;; ==========================================
  ;; js2 mode settings
  (push '("\\.js\\'" . react-mode) auto-mode-alist)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-pretty-multiline-declarations 'all)
  (setq js2-bounce-indent-p nil)
  (setq js2-highlight-level 3)
  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)
  (setq css-indent-offset 2)
  (setq js2-mode-indent-ignore-first-tab nil)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)

  ;; enable tooltips for docstrings
  ;; (setq-default dotspacemacs-configuration-layers
  ;;               '((auto-completion :variables
  ;;                                  auto-completion-enable-help-tooltip t)))

  ;; SOME COOL INDENT TIMES!
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2))

  (add-hook 'web-mode-hook  'my-web-mode-hook)
;; disable jshint since we prefer eslint checking
  (with-eval-after-load "flycheck"
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))))



;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
    ("5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(linum-format " %5i ")
 '(neo-theme (quote icons))
 '(package-selected-packages
   (quote
    (winum unfill prettier-js org-category-capture request fuzzy flx goto-chg undo-tree pkg-info epl bind-key popup doom-tomorrow-night-theme clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider seq queue clojure-mode dash sql-indent disaster company-c-headers cmake-mode clang-format tagedit erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks deft doom-themes diminish packed evil helm helm-core avy projectile async s spaceline-all-the-icons memoize font-lock+ origami flycheck-elm elm-mode magit-gh-pulls github-search github-clone github-browse-file gist gh marshal logito pcache ht all-the-icons rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby centered-window-mode js-import zonokai-theme zenburn-theme zen-and-art-theme yaml-mode xterm-color web-mode web-beautify slim-mode shell-pop seti-theme scss-mode sass-mode reverse-theme reveal-in-osx-finder rainbow-mode rainbow-identifiers railscasts-theme purple-haze-theme pug-mode professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pbcopy pastels-on-dark-theme osx-trash osx-dictionary organic-green-theme org alert log4e gntp omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme multi-term monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme markdown-mode majapahit-theme lush-theme livid-mode skewer-mode simple-httpd light-soap-theme less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme helm-css-scss hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme go-guru go-eldoc gitignore-mode fringe-helper git-gutter+ git-gutter gandalf-theme flyspell-correct-helm flyspell-correct pos-tip flycheck flatui-theme flatland-theme firebelly-theme farmhouse-theme magit magit-popup git-commit with-editor espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web web-completion-data company-tern dash-functional tern company-go go-mode company color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme yasnippet auto-dictionary apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme auto-complete ws-butler window-numbering volatile-highlights vi-tilde-fringe uuidgen toc-org spaceline powerline restart-emacs rainbow-delimiters popwin persp-mode paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-ediff evil-args evil-anzu anzu eval-sexp-fu highlight dumb-jump f define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol aggressive-indent adaptive-wrap ace-link which-key use-package spacemacs-theme smeargle quelpa pcre2el orgit org-projectile org-present org-pomodoro org-download mwim mmm-mode markdown-toc magit-gitflow macrostep hydra htmlize help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md flycheck-pos-tip exec-path-from-shell evil-visualstar evil-magit evil-escape elisp-slime-nav diff-hl company-statistics bind-map auto-yasnippet auto-compile ace-window ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(ansi-color-names-vector
     ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
   '(custom-safe-themes
     (quote
      ("5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" default)))
   '(doom-neotree-line-spacing 3)
   '(evil-want-Y-yank-to-eol nil)
   '(fci-rule-color "#383a42" t)
   '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
   '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
   '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
   '(linum-format " %5i ")
   '(neo-auto-indent-point t t)
   '(neo-banner-message "Press ? for neotree help" t)
   '(neo-create-file-auto-open t t)
   '(neo-show-hidden-files t t)
   '(neo-show-updir-line nil t)
   '(neo-smart-open t t)
   '(neo-theme (quote icons))
   '(neo-window-fixed-size nil)
   '(neo-window-width 40 t)
   '(org-fontify-quote-and-verse-blocks nil)
   '(org-fontify-whole-heading-line nil)
   '(package-selected-packages
     (quote
      (editorconfig toml-mode racer flycheck-rust cargo rust-mode winum unfill prettier-js org-category-capture request fuzzy flx goto-chg undo-tree pkg-info epl bind-key popup doom-tomorrow-night-theme clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider seq queue clojure-mode dash sql-indent disaster company-c-headers cmake-mode clang-format tagedit erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks deft doom-themes diminish packed evil helm helm-core avy projectile async s spaceline-all-the-icons memoize font-lock+ origami flycheck-elm elm-mode magit-gh-pulls github-search github-clone github-browse-file gist gh marshal logito pcache ht all-the-icons rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby centered-window-mode js-import zonokai-theme zenburn-theme zen-and-art-theme yaml-mode xterm-color web-mode web-beautify slim-mode shell-pop seti-theme scss-mode sass-mode reverse-theme reveal-in-osx-finder rainbow-mode rainbow-identifiers railscasts-theme purple-haze-theme pug-mode professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pbcopy pastels-on-dark-theme osx-trash osx-dictionary organic-green-theme org alert log4e gntp omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme multi-term monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme markdown-mode majapahit-theme lush-theme livid-mode skewer-mode simple-httpd light-soap-theme less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme helm-css-scss hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme go-guru go-eldoc gitignore-mode fringe-helper git-gutter+ git-gutter gandalf-theme flyspell-correct-helm flyspell-correct pos-tip flycheck flatui-theme flatland-theme firebelly-theme farmhouse-theme magit magit-popup git-commit with-editor espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web web-completion-data company-tern dash-functional tern company-go go-mode company color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme yasnippet auto-dictionary apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme auto-complete ws-butler window-numbering volatile-highlights vi-tilde-fringe uuidgen toc-org spaceline powerline restart-emacs rainbow-delimiters popwin persp-mode paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-ediff evil-args evil-anzu anzu eval-sexp-fu highlight dumb-jump f define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol aggressive-indent adaptive-wrap ace-link which-key use-package spacemacs-theme smeargle quelpa pcre2el orgit org-projectile org-present org-pomodoro org-download mwim mmm-mode markdown-toc magit-gitflow macrostep hydra htmlize help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md flycheck-pos-tip exec-path-from-shell evil-visualstar evil-magit evil-escape elisp-slime-nav diff-hl company-statistics bind-map auto-yasnippet auto-compile ace-window ace-jump-helm-line ac-ispell)))
   '(paradox-github-token t)
   '(vc-annotate-background "#f0f0f0")
   '(vc-annotate-color-map
     (list
      (cons 20 "#50a14f")
      (cons 40 "#688e35")
      (cons 60 "#807b1b")
      (cons 80 "#986801")
      (cons 100 "#ae7118")
      (cons 120 "#c37b30")
      (cons 140 "#da8548")
      (cons 160 "#c86566")
      (cons 180 "#b74585")
      (cons 200 "#a626a4")
      (cons 220 "#ba3685")
      (cons 240 "#cf4667")
      (cons 260 "#e45649")
      (cons 280 "#d2685f")
      (cons 300 "#c07b76")
      (cons 320 "#ae8d8d")
      (cons 340 "#383a42")
      (cons 360 "#383a42")))
   '(vc-annotate-very-old-color nil))
  )
