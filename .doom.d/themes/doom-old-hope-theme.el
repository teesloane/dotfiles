;;; doom-one-theme.el --- inspired by An Old Hope -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-one-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-one-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-one-theme
  :type 'boolean)

(defcustom doom-one-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-one-theme
  :type 'boolean)

(defcustom doom-one-comment-bg doom-one-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-one-theme
  :type 'boolean)

(defcustom doom-one-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-one-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-old-hope
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#1c1d20" "#1c1d20"       nil))
   (bg-alt     '("#313339" "#313339"       nil))
   (base0      '("#1B2229" "black"   "black"))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"))
   (base2      '("#202328" "#2e2e2e" "brightblack"))
   (base3      '("#23272e" "#262626" "brightblack"))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"))
   (base5      '("#5B6268" "#525252" "brightblack"))
   (base6      '("#686b78" "#686b78" "brightblack"))
   (base7      '("#9ca0a4" "#979797" "brightblack"))
   (base8      '("#DFDFDF" "#dfdfdf" "white"))
   (fg         '("#cbccd1" "#cbccd1" "brightwhite"))
   (fg-alt     '("#5B6268" "#2d2d2d" "white"))

   (grey       base4)
   (red        '("#ea3d54" "#ea3d54" "red"))
   (orange     '("#ee7b29" "#ee7b29" "brightred"))
   (green      '("#78bd65" "#78bd65" "green"))
   (teal       '("#78bd65" "#78bd65" "brightgreen"))
   (yellow     '("#fedd38" "#fedd38" "yellow"))
   (blue       '("#4fb3d8" "#4fb3d8" "brightblue"))
   (dark-blue  '("#5689f0" "#5689f0" "blue"))
   (magenta    '("#b978ab" "#b978ab" "brightmagenta"))
   (violet     '("#b978ab" "#b978ab" "brightmagenta"))
   (cyan       '("#4fb3d8" "#4fb3d8" "brightcyan"))
   (dark-cyan  '("#4fb3d8" "#4fb3d8" "cyan"))

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   (doom-darken base4 0.2))
   (selection      red)
   (builtin        yellow)
   (comments       base4)
   (doc-comments   (doom-lighten blue 0.25))
   (constants      orange)
   (functions      yellow)
   (keywords       green)
   (methods        yellow)
   (operators      green)
   (type           orange)
   (strings        blue)
   (variables      red)
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)


   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-one-brighter-modeline)
   (-modeline-pad
    (when doom-one-padded-modeline
      (if (integerp doom-one-padded-modeline) doom-one-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.35) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg) 0.03) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ;; (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-one-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :background bg-alt :distant-foreground bg :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; LaTeX-mode
   (font-latex-math-face :foreground green)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ())


;;; doom-one-theme.el ends here
