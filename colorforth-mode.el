;;; colorforth-mode.el --- Major mode for editing colorForth code

;; Copyright (c) 2022,2023 Eric Sessoms
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Author: Eric Sessoms <esessoms@polya.io>
;; Package-Requires: ((emacs "24"))
;; Version: 0.1.0
;; Keywords: colorForth, forth, languages

;; This file is not part of GNU Emacs.


;;; Commentary:

;; A major mode for editing colorForth code.

;; Some editor support is necessary for working with colorForth.  You
;; must of course display the "colors" in some fashion, but you also
;; need to hide the color codes lest they contribute too much
;; clutter.

;; This mode also provides a few keybindings to insert and remove
;; color codes.

;; Indentation is a minor concern, because there's so little of it.

;; Setup:
;; (add-to-list 'load-path "~/site-lisp/1xforth")
;; (setq colorforth-theme 'light)
;; (require 'colorforth-mode)

;; Recommended:
;; (add-hook 'colorforth-mode-hook
;;           (lambda ()
;;             (setq-local display-fill-column-indicator-column 64)
;;             (display-fill-column-indicator-mode 1)
;;             (setq-local whitespace-style '(face tabs trailing))
;;             (whitespace-mode 1)))

;; General:
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; Credits:

;; array-forth-mode.el -- Tikhon Jelvis <tikhon@berkeley.edu>
;; forth-mode.el -- Lars Brinkhoff <lars@nocrew.org>
;; gforth.el -- Goran Rydqvist <gorry@ida.liu.se>, David Kühling <dvdkhlng@gmx.de>
;; pygmy-mode.el -- Frank Sergeant <frank@pygmy.utoh.org>


;;; Code:

(defgroup colorforth nil
  "Customization options for colorForth mode."
  :prefix "colorforth-"
  :group 'languages)


;; Moore's recommendation, before he quit bothering with indentation.
(defcustom colorforth-indent-basic 3
  "Indentation in colorForth mode."
  :type 'integer)
(put 'colorforth-mode-basic 'safe-local-variable
     (lambda (v)
       (or (null v) (integerp v))))

(defcustom colorforth-theme
  (if (member 'dichromacy custom-enabled-themes)
      'deuteranomaly
    'dark)
  "Have it your way."
  :type '(member dark deuteranomaly light low-color monochrome))


;; Default is dark theme.
(defconst colorforth--colors
  '((red    "bright red"    "definitions")
    (green  "bright green"  "compiled words")
    (yellow "bright yellow" "executed words")
    (cyan   "bright cyan"   "postponed words")
    (white  "bright white"  "comments"))
  "List of colors for input into generator macros.")

(defmacro colorforth--generate-colors (generator)
  `(progn
     ,@(mapcar (lambda (color)
                 (apply generator color))
               colorforth--colors)))


;; Declare customization points for display colors.
(colorforth--generate-colors
 (lambda (name color doc)
   `(defcustom ,(intern (concat "colorforth-" (symbol-name name) "-color")) ,color
      ,(concat "Default color used to mark " doc ".")
      :type 'color)))

(defun colorforth--light ()
  (custom-set-variables
   '(colorforth-red-color    "red"    nil nil "definitions")
   '(colorforth-green-color  "green"  nil nil "compiled")
   '(colorforth-yellow-color "yellow" nil nil "executed")
   '(colorforth-cyan-color   "cyan"   nil nil "postponed")
   '(colorforth-white-color  "black"  nil nil "comments")))

(defun colorforth--deuteranomaly ()
  "Colors from dichromacy theme."
  (custom-set-variables
   '(colorforth-red-color    "#d55e00" nil nil "vermillion")
   '(colorforth-green-color  "#009e73" nil nil "bluegreen")
   '(colorforth-yellow-color "#e69f00" nil nil "orange")
   '(colorforth-cyan-color   "#56b4e9" nil nil "skyblue")
   '(colorforth-white-color  "black"   nil nil "black")))

(cl-case colorforth-theme
  (deuteranomaly (colorforth--deuteranomaly))
  (light         (colorforth--light)))

(when (or (eq colorforth-theme 'deuteranomaly)
          (and (eq colorforth-theme 'light)
               (member 'dichromacy custom-enabled-themes)))
  (colorforth--deuteranomaly))

;; Declare customization points for display faces.
(colorforth--generate-colors
 (lambda (name color doc)
   (let ((color (intern (concat "colorforth-" (symbol-name name) "-color"))))
     `(defface ,(intern (concat "colorforth-" (symbol-name name) "-face"))
        `((t (:foreground ,,color)))
        ,(concat "Face used to highlight " doc ".")
        :set-after '(,color)))))

(defun colorforth--low-color ()
  (custom-set-faces
   '(colorforth-red-face    ((t (:foreground "black"       :weight bold))))
   '(colorforth-green-face  ((t (:foreground "brightblack" :slant italic))))
   '(colorforth-yellow-face ((t (:foreground "#e69f00"))))  ; orange
   '(colorforth-cyan-face   ((t (:foreground "#56b4e9"))))  ; skyblue
   '(colorforth-white-face  ((t (:foreground "brightblack"))))))

(defun colorforth--monochrome ()
  (colorforth--low-color)
  (custom-set-faces
   '(colorforth-yellow-face ((t (:background "white" :foreground "black"))))
   '(colorforth-cyan-face   ((t (:background "black" :foreground "brightwhite"))))))

(cl-case colorforth-theme
  (low-color  (colorforth--low-color))
  (monochrome (colorforth--monochrome)))


;; Everything in Forth is whitespace or a word.
(defconst colorforth-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry '(?\x01 . ?\x20) "-" table)
    (modify-syntax-entry '(?\x21 . ?\x7E) "w" table)
    (modify-syntax-entry           ?\x7F  "-" table)
    ;; We'll call the color codes punctuation.
    (modify-syntax-entry '(?\x80 . ?\x8F) "." table)
    table)
  "Syntax table for use in colorForth mode buffers.")


;; Binary codes used to markup colorForth source.  (Octal comments are
;; to better show the bit patterns.)
(defconst colorforth--red    "\x81" "Marks definitions.")      ;; ?\201
(defconst colorforth--green  "\x82" "Marks compiled words.")   ;; ?\202
(defconst colorforth--yellow "\x83" "Marks executed words.")   ;; ?\203
(defconst colorforth--cyan   "\x86" "Marks compiled macros.")  ;; ?\206
(defconst colorforth--white  "\x87" "Marks comments.")         ;; ?\207

;; We reserve the entire range.  Not all codes are in-use.
(defconst colorforth--color-code "[\x80-\x8F]")


(defun colorforth--codes-hidden ()
  (member 'colorforth-color-code buffer-invisibility-spec))

(defun colorforth-show-codes ()
  "Turns off hiding of color codes."
  (interactive)
  (remove-from-invisibility-spec 'colorforth-color-code)
  (font-lock-fontify-buffer))

(defun colorforth-hide-codes ()
  "Turns on hiding of color codes."
  (interactive)
  (add-to-invisibility-spec 'colorforth-color-code)
  (font-lock-fontify-buffer))

(defun colorforth-toggle-codes ()
  "Toggles hiding of color codes."
  (interactive)
  (if (colorforth--codes-hidden)
      (colorforth-show-codes)
    (colorforth-hide-codes)))


(defun colorforth-forward-color (arg)
  "Can substitute for moving by sexp."
  (interactive "p")
  (dotimes (_ arg)
    (while (looking-at-p colorforth--color-code)
      (forward-char 1))
    (skip-syntax-forward "^.")))

(defun colorforth-backward-color (arg)
  (interactive "p")
  (dotimes (_ arg)
    (while (looking-at-p colorforth--color-code)
      (backward-char 1))
    (skip-syntax-backward "^.")))


(defun colorforth-delete-color ()
  (interactive)
  (while (looking-at-p colorforth--color-code)
    (delete-char 1))
  (while (looking-back colorforth--color-code)
    (backward-delete-char 1)))

(defun colorforth-backward-delete-color ()
  (interactive)
  (save-excursion
    (colorforth-backward-color 1)
    (when (looking-at-p colorforth--color-code)
      (colorforth-delete-color))))


(defun colorforth--do-insert-color (arg color)
  ;; Cleanup any existing codes in this location.
  (colorforth-delete-color)
  ;; Negative arg means delete only, don't insert.
  (when (>= arg 1)
    ;; Insert the code to mark this word.
    (insert color)
    (unless (eolp)
      (forward-char 1))))

(defun colorforth-insert-color (arg color)
  "Mark the current word with COLOR."
  (if (looking-at "\\w+")
      (save-excursion
        ;; Find beginning of current word.
        (forward-word)
        (backward-word)
        ;; Insert color before current word.
        (colorforth--do-insert-color arg color))
    (colorforth--do-insert-color arg color)))

(defun colorforth-backward-insert-color (arg color)
  "Mark the current word with COLOR."
  (save-excursion
    (backward-word)
    (colorforth--do-insert-color arg color)))


;; Define functions to insert or delete color codes.
(colorforth--generate-colors
 (lambda (name color doc)
   (let ((arg (gensym "ARG")))
     `(defun ,(intern (concat "colorforth-" (symbol-name name) "-word")) (,arg)
        ,(concat "Mark the current word as a " doc ".")
        (interactive "p")
        (colorforth-insert-color ,arg ,(intern (concat "colorforth--" (symbol-name name))))))))

;; Define functions to insert or delete color codes.
(colorforth--generate-colors
 (lambda (name color doc)
   (let ((arg (gensym "ARG")))
     `(defun ,(intern (concat "colorforth-backward-" (symbol-name name) "-word")) (,arg)
        ,(concat "Mark the current word as a " doc ".")
        (interactive "p")
        (colorforth-backward-insert-color ,arg ,(intern (concat "colorforth--" (symbol-name name))))))))


;;; Keys

(defvar colorforth-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Show, hide, or toggle display of raw color codes.
    (define-key map (kbd "C-c s")   'colorforth-show-codes)
    (define-key map (kbd "C-c h")   'colorforth-hide-codes)
    (define-key map (kbd "C-c t")   'colorforth-toggle-codes)
    ;; Move between colors as sexps.
    (define-key map (kbd "C-M-f")   'colorforth-forward-color)
    (define-key map (kbd "C-M-b")   'colorforth-backward-color)
    ;; Delete color codes.
    (define-key map (kbd "C-c d")   'colorforth-delete-color)
    (define-key map (kbd "C-c M-d") 'colorforth-backward-delete-color)
    ;; Insert or delete (with prefix argument) color codes.
    (define-key map (kbd "C-c r")   'colorforth-red-word)
    (define-key map (kbd "C-c M-r") 'colorforth-backward-red-word)
    (define-key map (kbd "C-c g")   'colorforth-green-word)
    (define-key map (kbd "C-c M-g") 'colorforth-backward-green-word)
    (define-key map (kbd "C-c y")   'colorforth-yellow-word)
    (define-key map (kbd "C-c M-y") 'colorforth-backward-yellow-word)
    (define-key map (kbd "C-c c")   'colorforth-cyan-word)
    (define-key map (kbd "C-c M-c") 'colorforth-backward-cyan-word)
    (define-key map (kbd "C-c w")   'colorforth-white-word)
    (define-key map (kbd "C-c M-w") 'colorforth-backward-white-word)
    map)
  "Keymap for colorForth mode.")

;; Evil key bindings.
(with-eval-after-load 'evil
  (evil-define-key 'normal colorforth-mode-map
    ;; Show, hide, or toggle display of raw color codes.
    (kbd "<localleader>s")   'colorforth-show-codes
    (kbd "<localleader>h")   'colorforth-hide-codes
    (kbd "<localleader>t")   'colorforth-toggle-codes
    (kbd "<localleader>d")   'colorforth-delete-color
    ;; Delete color codes.
    (kbd "<localleader>M-d") 'colorforth-backward-delete-color
    (kbd "<localleader>r")   'colorforth-red-word
    ;; Insert color codes.
    (kbd "<localleader>M-r") 'colorforth-backward-red-word
    (kbd "<localleader>g")   'colorforth-green-word
    (kbd "<localleader>M-g") 'colorforth-backward-green-word
    (kbd "<localleader>y")   'colorforth-yellow-word
    (kbd "<localleader>M-y") 'colorforth-backward-yellow-word
    (kbd "<localleader>c")   'colorforth-cyan-word
    (kbd "<localleader>M-c") 'colorforth-backward-cyan-word
    (kbd "<localleader>w")   'colorforth-white-word
    (kbd "<localleader>M-w") 'colorforth-backward-white-word))


(defun colorforth--prefix (color)
  "Construct a regular expression to match a colored word."
  (concat color "\\s-*\\<\\([[:word:][:space:]]+\\)"))

(defvar colorforth-font-lock-keywords
  `(;; Unmarked text at the beginning of a buffer is always a comment.
    ("\\`\\s-*\\<\\([[:word:][:space:]]+\\)"  . (1 'colorforth-white-face))
    ;; Unmarked text at the beginning of a page is always a comment.
    (,(colorforth--prefix page-delimiter)     . (1 'colorforth-white-face))
    ;; Apply color codes.
    (,(colorforth--prefix colorforth--red)    . (1 'colorforth-red-face))
    (,(colorforth--prefix colorforth--green)  . (1 'colorforth-green-face))
    (,(colorforth--prefix colorforth--yellow) . (1 'colorforth-yellow-face))
    (,(colorforth--prefix colorforth--cyan)   . (1 'colorforth-cyan-face))
    (,(colorforth--prefix colorforth--white)  . (1 'colorforth-white-face))
    ;; Hide color codes at the beginning a word.  Any other
    ;; occurrences are likely errors.
    (,(concat "\\(" colorforth--color-code "\\)\\<")
     . (1 '(face font-lock-warning-face invisible colorforth-color-code))))
  "Default syntax highlighting for colorForth.")

(defun colorforth-font-lock-extend-region ()
  "Delimit a region for syntax highlighting."
  (let (changed)
    (save-excursion
      ;; Extend to previous color code or beginning of buffer.
      (goto-char font-lock-beg)
      (unless (or (bobp)
                  (looking-at colorforth--color-code))
        (setq font-lock-beg
              (or (re-search-backward colorforth--color-code nil t)
                  (point-min)))
        (setq changed t))
      ;; Extend to next character code or end of buffer.
      (goto-char font-lock-end)
      (unless (or (eobp)
                  (looking-at colorforth--color-code))
        (setq font-lock-end
              (or (re-search-forward colorforth--color-code nil t)
                  (point-max)))
        (setq changed t)))
    changed))


(defun colorforth--line-color ()
  "If the line starts with a color code, that's our color.
Otherwise look backward for the most recent color code.  If there
is none, the color is white."
  (save-excursion
    ;; First non-whitespace character.
    (forward-to-indentation 0)
    ;; If it's not a color code, look backwards.
    (unless (looking-at colorforth--color-code)
      (re-search-backward colorforth--color-code nil t))
    ;; N.B., redundancy necessary here b/c Elisp doesn't handle 8-bit
    ;; characters well.
    (cl-case (char-after)
      (?\x81 'red)
      (?\x82 'green)
      (?\x83 'yellow)
      (?\x86 'cyan)
      (t     'white))))

(defun colorforth--prev-line-color ()
  "Whenever the previous line is empty, we want to return NIL so
that it can never match the current line."
  (save-excursion
    (forward-to-indentation -1)
    (if (eolp)
        nil
      (colorforth--line-color))))

(defun colorforth--current-indent ()
  "How many spaces is the previous line indented?  If for some
reason the current block of code is not indented to `tab-width',
we want to stay consistent with it."
  (save-excursion
    (forward-to-indentation -1)
    (let ((end (point)))
      (beginning-of-line)
      (- end (point)))))

(defun colorforth--indent-to (col)
  "Indent the current line to the desired number of spaces."
  (beginning-of-line)
  (indent-to col))

;; N.B., there is no identation for "structure".
(defun colorforth-indent-line ()
  (let ((curr (colorforth--line-color))
        (prev (colorforth--prev-line-color)))
    (if (eq curr prev)
        ;; Align consecutive lines of the same color.
        (colorforth--indent-to (colorforth--current-indent))
      (cl-case curr
        ;; Start definitions at the left margin.
        ((red)
         (colorforth--indent-to 0))
        ;; Indent compiled code.
        ((green cyan)
         (colorforth--indent-to colorforth-indent-basic))
        ((yellow)
         (if (eq prev 'red)
             ;; Indent executable code under a definition.
             (colorforth--indent-to colorforth-indent-basic)
           (colorforth--indent-to (colorforth--current-indent))))
        ;; Indent comments to match the previous line.
        ((white)
         (colorforth--indent-to (colorforth--current-indent)))
        ;; Cleanup lines containing only whitespace.
        (nil
         (colorforth--indent-to 0))))))


;;; colorForth mode

;;;###autoload:
(define-derived-mode colorforth-mode prog-mode "colorForth"
  "Major mode for editing colorForth code.

\\{colorforth-mode-map}"

  :group 'colorforth
  :syntax-table colorforth-mode-syntax-table

  ;; Color codes depend on an 8-bit encoding.
  (set-buffer-multibyte nil)

  ;; Hide color codes during normal operation.
  (add-to-invisibility-spec 'colorforth-color-code)

  ;; Whitespace.
  (setq-local electric-indent-inhibit t)
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline t)
  (setq-local tab-width colorforth-indent-basic)

  ;; One line of code is traditionally 64 columns, but note that Moore
  ;; uses 48 columns in his colorForth.
  (setq-local fill-column 64)

  ;; Allow a little slop to avoid throwing comments out-of-alignment.
  (setq-local comment-fill-column 72)

  ;; For those times you need to write vertical code.
  (setq-local comment-column 31)
  (setq-local comment-multi-line t)
  (setq-local comment-start "\x87")
  (setq-local comment-start-skip "\x87[[:word:][:space:]]*")

  ;; Definitions to the left.
  (setq-local indent-line-function 'colorforth-indent-line)

  ;; Enable multiline font-locking.
  (add-to-list 'font-lock-extra-managed-props 'invisible)
  (setq-local font-lock-defaults '(colorforth-font-lock-keywords))
  (setq-local font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions
            'colorforth-font-lock-extend-region))


;;; Footer:

;;;###autoload:
(add-to-list 'auto-mode-alist '("\\.cf\\'" . colorforth-mode))

;;;###autoload
(modify-coding-system-alist 'file "\\.cf\\'" 'no-conversion)

(provide 'colorforth-mode)

;;; colorforth-mode.el ends here
