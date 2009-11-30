;;; auto-complete-config.el --- auto-complete additional configuations

;; Copyright (C) 2009  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:



(eval-when-compile
  (require 'cl))

(require 'auto-complete)



;; Emacs Lisp sources

(defvar ac-emacs-lisp-features nil)

(defvar ac-source-emacs-lisp-features
  '((init . (unless ac-emacs-lisp-features
              (let ((suffix (concat (regexp-opt (find-library-suffixes) t) "\\'")))
                (setq ac-emacs-lisp-features
                      (append (mapcar 'prin1-to-string features)
                              (loop for dir in load-path
                                    if (file-directory-p dir)
                                    append (loop for file in (directory-files dir)
                                                 if (string-match suffix file)
                                                 collect (substring file 0 (match-beginning 0)))))))))
    (candidates . ac-emacs-lisp-features)
    (prefix . "require +'\\(\\(?:\\sw\\|\\s_\\)*\\)")
    (limit . 0)))

(defun ac-emacs-lisp-features-setup ()
  (push 'ac-source-emacs-lisp-features ac-sources))

(defun ac-emacs-lisp-features-initialize ()
  (require 'find-func)
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-features-setup)
  t)



;; C++ sources

(ac-define-dictionary-source
 ac-source-c++-keywords
 ("and" "bool" "compl" "do" "export" "goto" "namespace" "or_eq" "return"
  "struct" "try" "using" "xor" "and_eq" "break" "const" "double" "extern"
  "if" "new" "private" "short" "switch" "typedef" "virtual" "xor_eq" "asm"
  "case" "const_cast" "dynamic_cast" "false" "inline" "not" "protected" 
  "signed" "template" "typeid" "void" "auto" "catch" "continue" "else" 
  "float" "int" "not_eq" "public" "sizeof" "this" "typename" "volatile"
  "bitand" "char" "default" "enum" "for" "long" "operator" "register"
  "static" "throw" "union" "wchar_t" "bitor" "class" "delete" "explicit"
  "friend" "mutable" "or" "reinterpret_cast" "static_cast" "true" 
  "unsigned" "while"))

(defun ac-c++-keywords-setup ()
  (push 'ac-source-c++-keywords ac-sources))

(defun ac-c++-keywords-initialize ()
  (add-hook 'c++-mode-hook 'ac-c++-keywords-setup)
  t)



;; CSS sources

(defun ac-css-keywords-setup ()
  (push 'ac-source-css-keywords ac-sources))

(defun ac-css-keywords-initialize ()
  (add-hook 'css-mode-hook 'ac-css-keywords-setup)
  t)

(ac-define-dictionary-source
 ac-source-css-keywords
 ("ActiveBorder" "ActiveCaption" "Alpha" "AppWorkspace" "Background"
  "Barn" "BasicImage" "Blinds" "Blur" "ButtonFace" "ButtonHighlight"
  "ButtonShadow" "ButtonText" "CaptionText" "CheckerBoard" "Chroma"
  "Compositor" "CradientWipe" "DXImageTransform" "DropShadow" "Emboss"
  "Engrave" "Fade" "FlipH" "FlipV" "Glow" "Gray" "GrayText" "Highlight"
  "HighlightText" "Hz" "ICMFilter" "InactiveBorder" "InactiveCaption"
  "InactiveCaptionText" "InfoBackground" "InfoText" "Inset" "Invert"
  "Iris" "Light" "MaskFilter" "Matrix" "Menu" "MenuText" "Microsoft"
  "MotionBlur" "Pixelate" "RadialWipe" "RandomBars" "RandomDissolve"
  "RevealTrans" "Scrollbar" "Shadow" "Slide" "Spiral" "Stretch"
  "Strips" "ThreeDDarkShadow" "ThreeDFace" "ThreeDHighlight"
  "ThreeDLightShadow" "ThreeDShadow" "Wave" "Wheel" "Window"
  "WindowFrame" "WindowText" "Xray" "Zigzag" "_azimuth" "_background"
  "_background-position-x" "_background-position-y" "_border" "_bottom"
  "_caption" "_clear" "_clip" "_color" "_content" "_counter" "_cue"
  "_cursor" "_direction" "_display" "_elevation" "_empty" "_filter"
  "_filter:progid:DXImageTransform.Microsoft" "_float" "_font"
  "_height" "_ime" "_ime-mode" "_layout" "_layout-flow" "_layout-grid"
  "_layout-grid-char" "_layout-grid-line" "_layout-grid-mode"
  "_layout-grid-type" "_left" "_letter" "_line" "_line-break" "_list"
  "_margin" "_orphans" "_outline" "_overflow" "_overflow-x"
  "_overflow-y" "_padding" "_page" "_pause" "_pitch" "_play"
  "_position" "_quotes" "_richness" "_right" "_ruby" "_ruby-align"
  "_ruby-overhang" "_ruby-position" "_scrollbar"
  "_scrollbar-3dlight-color" "_scrollbar-arrow-color"
  "_scrollbar-base-color" "_scrollbar-darkshadow-color"
  "_scrollbar-face-color" "_scrollbar-highlight-color"
  "_scrollbar-track-color" "_speak" "_speech" "_stress" "_table"
  "_text" "_text-align-last" "_text-autospace" "_text-justify"
  "_text-kashida-space" "_text-overflow" "_text-underline-position"
  "_top" "_unicode" "_vertical" "_visibility" "_voice" "_volume"
  "_white" "_widows" "_width" "_word" "_word-break" "_word-wrap"
  "_writing" "_writing-mode" "_z" "_zoom" "above" "active" "adjust"
  "after" "aliceblue" "align" "always" "antiquewhite" "aqua"
  "aquamarine" "armenian" "arrow" "attachment" "auto" "autospace"
  "avoid" "azimuth" "azure" "background" "background-attachment"
  "background-color" "background-image" "background-position"
  "background-repeat" "bar" "base" "baseline" "before" "behind" "beige"
  "below" "bidi" "bidi-override" "bisque" "black" "blanchedalmond"
  "blink" "block" "blue" "blueviolet" "bold" "bolder" "border"
  "border-bottom" "border-bottom-color" "border-bottom-style"
  "border-bottom-width" "border-collapse" "border-color" "border-left"
  "border-left-color" "border-left-style" "border-left-width"
  "border-right" "border-right-color" "border-right-style"
  "border-right-width" "border-spacing" "border-style" "border-top"
  "border-top-color" "border-top-style" "border-top-width"
  "border-width" "both" "bottom" "box" "break" "brown" "burlwood"
  "cadetblue" "capitalize" "caps" "caption" "caption-side" "cell"
  "cells" "center" "center-left" "center-right" "char" "chartreuse"
  "chocolate" "circle" "cjk" "cjk-ideographic" "clear" "clip" "close"
  "close-quote" "cm" "code" "collapse" "color" "column" "compact"
  "condensed" "content" "continuous" "coral" "cornflowerblue"
  "cornsilk" "counter" "counter-increment" "counter-reset" "crimson"
  "crop" "cross" "crosshair" "cue" "cue-after" "cue-before" "cursive"
  "cursor" "cyan" "darkblue" "darkcyan" "darkgoldenrod" "darkgray"
  "darkgreen" "darkkhaki" "darkmagenta" "darkolivegreen" "darkorange"
  "darkorchid" "darkred" "darksalmon" "darkseagreen" "darkshadow"
  "darkslateblue" "darkslategray" "darkturquoise" "darkviolet" "dashed"
  "decimal" "decimal-leading-zero" "decoration" "deeppink"
  "deepskyblue" "default" "deg" "digits" "dimgray" "direction" "disc"
  "display" "dodgerblue" "dotted" "double" "during" "e" "e-resize"
  "elevation" "em" "embed" "empty" "empty-cells" "ex" "expanded"
  "extra" "extra-condensed" "extra-expanded" "face" "family" "fantasy"
  "far" "far-left" "far-right" "fast" "faster" "firebrick" "first"
  "first-child" "first-letter" "first-line" "fixed" "float"
  "floralwhite" "flow" "focus" "font" "font-family" "font-size"
  "font-size-adjust" "font-stretch" "font-style" "font-variant"
  "font-weight" "footer" "forestgreen" "fuchsia" "gainsboro" "georgian"
  "ghostwhite" "gold" "goldenrod" "gray" "greek" "green" "greenyellow"
  "grid" "groove" "group" "header" "hebrew" "height" "help" "hidden"
  "hide" "high" "higher" "hiragana" "hiragana-iroha" "honeydew"
  "hotpink" "hover" "icon" "ideographic" "image" "in" "increment"
  "indent" "index" "indianred" "indigo" "inherit" "inline"
  "inline-block" "inline-table" "inset" "inside" "iroha" "italic"
  "item" "ivory" "justify" "kHz" "kashida" "katakana" "katakana-iroha"
  "khaki" "landscape" "lang()" "large" "larger" "last" "latin"
  "lavender" "lavenderblush" "lawngreen" "layout" "leading" "left"
  "left-side" "leftwards" "lenonchiffon" "letter" "letter-spacing"
  "level" "lightblue" "lightcoral" "lightcyan" "lighter"
  "lightgoldenrodyellow" "lightgray" "lightgreen" "lightgrey"
  "lightpink" "lightsalmon" "lightseagreen" "lightskyblue"
  "lightslategray" "lightsteelblue" "lightyellow" "lime" "limegreen"
  "line" "line-height" "line-through" "linen" "link" "list" "list-item"
  "list-style" "list-style-image" "list-style-position"
  "list-style-type" "loud" "low" "lower" "lower-alpha" "lower-greek"
  "lower-latin" "lower-roman" "lowercase" "ltr" "magenta" "margin"
  "margin-bottom" "margin-left" "margin-right" "margin-top" "marker"
  "marker-offset" "marks" "maroon" "max" "max-height" "max-width"
  "medium" "mediumaquamarine" "mediumblue" "mediumorchid"
  "mediumpurple" "mediumseagreen" "mediumslateblue" "mediumspringgreen"
  "mediumturquoise" "mediumvioletred" "menu" "message" "message-box"
  "middle" "midnightblue" "min" "min-height" "min-width" "mintcream"
  "mistyrose" "mix" "mm" "moccasin" "mode" "monospace" "move" "ms" "n"
  "n-resize" "naby" "narrower" "navajowhite" "ne" "ne-resize" "no"
  "no-close-quote" "no-open-quote" "no-repeat" "none" "normal" "nowrap"
  "number" "numeral" "nw" "nw-resize" "oblique" "offset" "oldlace"
  "olive" "olivedrab" "once" "open" "open-quote" "orange" "orangered"
  "orchid" "orphans" "out" "outline" "outline-color" "outline-style"
  "outline-width" "outset" "outside" "overflow" "overhang" "overline"
  "override" "padding" "padding-bottom" "padding-left" "padding-right"
  "padding-top" "page" "page-break-after" "page-break-before"
  "page-break-inside" "palegoldenrod" "palegreen" "paleturquoise"
  "palevioletred" "papayawhip" "pause" "pause-after" "pause-before"
  "pc" "peachpuff" "peru" "pink" "pitch" "pitch-range" "play"
  "play-during" "plum" "pointer" "portarait" "position" "powderblue"
  "pre" "pre-line" "pre-wrap" "progid" "progress" "pt" "punctuation"
  "purple" "px" "quote" "quotes" "rad" "range" "rate" "red" "relative"
  "repeat" "repeat-x" "repeat-y" "reset" "resize" "richness" "ridge"
  "right" "right-side" "rightwards" "roman" "rosybrown" "row"
  "royalblue" "rtl" "run" "run-in" "s" "s-resize" "saddlebrown"
  "salmon" "sandybrown" "sans-serif" "scroll" "se" "se-resize"
  "seagreen" "seashell" "semi" "semi-condensed" "semi-expanded"
  "separate" "serif" "shadow" "show" "side" "sienna" "silent" "silever"
  "silver" "size" "skyblue" "slateblue" "slategray" "slow" "slower"
  "small" "small-caps" "small-caption" "smaller" "snow" "soft" "solid"
  "space" "spacing" "speak" "speak-header" "speak-numeral"
  "speak-punctuation" "specific" "specific-voice" "speech"
  "speech-rate" "spell" "spell-out" "springgreen" "square" "static"
  "status" "status-bar" "steelblue" "stress" "stretch" "style" "sub"
  "super" "sw" "sw-resize" "table" "table-caption" "table-cell"
  "table-column" "table-column-group" "table-footer-group"
  "table-header-group" "table-layout" "table-row" "table-row-group"
  "tan" "teal" "text" "text-align" "text-bottom" "text-decoration"
  "text-indent" "text-shadow" "text-top" "text-transform" "thick"
  "thin" "thistle" "through" "tomato" "top" "track" "transform"
  "transparent" "turquoise" "type" "ultra" "ultra-condensed"
  "ultra-expanded" "underline" "unicode" "unicode-bidi" "upper"
  "upper-alpha" "upper-latin" "upper-roman" "uppercase" "variant"
  "vertical" "vertical-align" "violet" "visibility" "visible" "visited"
  "voice" "voice-family" "volume" "w" "w-resize" "wait" "weight"
  "wheat" "white" "white-space" "whitesmoke" "wider" "widows" "width"
  "word" "word-spacing" "wrap" "x" "x-fast" "x-high" "x-large" "x-loud"
  "x-low" "x-slow" "x-small" "x-soft" "xx" "xx-large" "xx-small" "y"
  "yellow" "yellowgreen" "z" "z-index" "zero"))



;; Gtags sources

(defvar ac-gtags-modes
  '(c-mode cc-mode c++-mode java-mode))

(defface ac-gtags-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for gtags candidate")

(defface ac-gtags-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the gtags selected candidate.")

(defun ac-gtags-candidate ()
  (if (memq major-mode ac-gtags-modes)
      (ignore-errors
        (with-temp-buffer
          (when (eq (call-process "global" nil t nil "-ci" ac-prefix) 0)
            (goto-char (point-min))
            (let (candidates)
              (while (and (not (eobp))
                          (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) candidates)
                          (eq (forward-line) 0)))
              (nreverse candidates)))))))

(defvar ac-source-gtags
  '((candidates . ac-gtags-candidate)
    (candidate-face . ac-gtags-candidate-face)
    (selection-face . ac-gtags-selection-face)
    (requires . 3))
  "Source for gtags.")

(defun ac-gtags-initialize ()
  t)



;; Python sources

(defvar ac-ropemacs-loaded nil)
(defun ac-ropemacs-require ()
  (unless ac-ropemacs-loaded
    ;; Almost people hate rope to use `C-x p'.
    (if (not (boundp 'ropemacs-global-prefix))
        (setq ropemacs-global-prefix nil))
    (pymacs-load "ropemacs" "rope-")
    (setq ropemacs-enable-autoimport t)
    (setq ac-ropemacs-loaded t)))

(defun ac-ropemacs-setup ()
  (ac-ropemacs-require)
  ;(setq ac-sources (append (list 'ac-source-ropemacs) ac-sources))
  (setq ac-omni-completion-sources '(("\\." ac-source-ropemacs))))

(defun ac-ropemacs-initialize ()
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  (add-hook 'python-mode-hook 'ac-ropemacs-setup)
  t)

(defvar ac-ropemacs-completions-cache nil)
(defvar ac-source-ropemacs
  '((init
     . (lambda ()
         (setq ac-ropemacs-completions-cache
               (mapcar
                (lambda (completion)
                  (concat ac-prefix completion))
                (ignore-errors
                  (rope-completions))))))
    (candidates . ac-ropemacs-completions-cache)))



;; Ruby sources

(defvar ac-source-rcodetools
  '((init . (lambda ()
              (condition-case x
                  (save-excursion
                    (rct-exec-and-eval rct-complete-command-name "--completion-emacs-icicles"))
                (error) (setq rct-method-completion-table nil))))
    (candidates . (lambda ()
                    (all-completions
                     ac-prefix
                     (mapcar
                      (lambda (completion)
                        (replace-regexp-in-string "\t.*$" "" (car completion)))
                      rct-method-completion-table))))))

(defun ac-rcodetools-setup ()
  )

(defun ac-rcodetools-initialize ()
  (and (require 'rcodetools nil t)
       (prog1 t (add-hook 'ruby-mode-hook 'ac-rcodetools-setup))))



;; Semantic sources

(defvar ac-semantic-modes '(c-mode c++-mode jde-mode java-mode))

(defun ac-semantic-candidate (prefix)
  (if (memq major-mode ac-semantic-modes)
      (mapcar 'semantic-tag-name
              (ignore-errors
                (or (semantic-ia-get-completions
                     (semantic-analyze-current-context) (point))
                    (senator-find-tag-for-completion (regexp-quote prefix)))))))

(defvar ac-source-semantic
  '((candidates . (lambda () (all-completions ac-prefix (ac-semantic-candidate ac-prefix)))))
  "Source for semantic.")

(defun ac-semantic-initialize ()
  (require 'semantic-ia))



;; Yasnippet sources

(defun ac-yasnippet-candidate-1 (table)
  (let ((hashtab (yas/snippet-table-hash table))
        (parent (if (fboundp 'yas/snippet-table-parent)
                    (yas/snippet-table-parent table)))
        candidates)
    (maphash (lambda (key value)
               (push key candidates))
             hashtab)
    (setq candidates (all-completions ac-prefix (nreverse candidates)))
    (if parent
        (setq candidates
              (append candidates (ac-yasnippet-candidate-1 parent))))
    candidates))

(defun ac-yasnippet-candidate ()
  (if (fboundp 'yas/get-snippet-tables)
      ;; >0.6.0
      (apply 'append (mapcar 'ac-yasnippet-candidate-1 (yas/get-snippet-tables major-mode)))
    (let ((table
           (if (fboundp 'yas/snippet-table)
               ;; <0.6.0
               (yas/snippet-table major-mode)
             ;; 0.6.0
             (yas/current-snippet-table))))
      (if table
          (ac-yasnippet-candidate-1 table)))))

(defun ac-yasnippet-initialize ()
  (and (require 'yasnippet nil t)
       t))

(defface ac-yasnippet-candidate-face
  '((t (:background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate.")

(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white")))
  "Face for the yasnippet selected candidate.")

(defvar ac-source-yasnippet
  '((candidates . ac-yasnippet-candidate)
    (action . yas/expand)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face))
  "Source for Yasnippet.")



;; Eclim sources

(defun ac-eclim-candidates ()
  (loop for c in (eclim/java-complete)
        collect (nth 1 c)))

(defvar ac-source-eclim
  '((candidates . ac-eclim-candidates)
    (prefix . c-dot)))

(provide 'auto-complete-config)
;;; auto-complete-config.el ends here
