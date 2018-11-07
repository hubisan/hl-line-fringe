;;; hl-line-fringe.el --- Highlight current line and/or indicate in fringe -*- lexical-binding:t -*-

;; Copyright (C) 2018 Daniel Hubmann

;; Author: Daniel Hubmann <hubisan@gmail.com>
;; Maintainer: Daniel Hubmann <hubisan@gmail.com>
;; URL: https://github.com/hubisan/hl-line-fringe
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Highlight current line and/or indicate the current line in the fringe locally
;; or globally.

;; Main features:
;;   - Highlights the current line.
;;   - Indicates the current line in the fringe.
;;   - Different faces for current and inactive buffers.
;;   - Different bitmaps for the fringe indicator for current and
;;     inactive buffers.

;;;; Usage

;; Use `hl-line-fringe-mode' to turn on the mode in a buffer or
;; `global-hl-line-fringe-mode' to turn it on in all buffers.

;; Customization:
;; - Line hightlight and fringe indicator can be turned off individually with
;;   `hl-line-fringe-line' and `hl-line-fringe-indicator'.
;; - Change faces `hl-line-fringe-line', `hl-line-fringe-line-inactive',
;;   `hl-line-fringe-indicator', `hl-line-fringe-indicator-inactive'.
;; - Change the bitmaps used for the fringe indicator by setting
;;   `hl-line-fringe-indicator-bitmap' and
;;   `hl-line-fringe-indicator-bitmap-inactive'.
;; - Both overlays are sticky by default (stay on when buffer is inactive). This
;;   behaviour can be changed with `hl-line-fringe-line-sticky' and
;;   `hl-line-fringe-indicator-sticky'
;; - If using the global mode major modes can be added to
;;   `hl-line-fringe-global-ignored-major-modes' to be ignored.
;; See all available customizations with `customize-group'.

;;; Code:

;; * Requirements

(require 'fringe)

;; * Customization

(defgroup hl-line-fringe nil
  "Highlight current line and/or indicate in fringe."
  :group 'convenience)

;; ** Defcustom

(defcustom hl-line-fringe-line t
  "If non-nil highlighting the line is enabled."
  :type 'boolean)

(defcustom hl-line-fringe-line-sticky t
  "If nil the line highlight is only shown in the current buffer."
  :type 'boolean)

(defcustom hl-line-fringe-line-overlay-priority -50
  "Priority of line highlight overlay.
Higher priority overlays others with lower priority."
  :type 'integer)

(defcustom hl-line-fringe-indicator t
  "If non-nil the fringe indicator is enabled."
  :type 'boolean)

(defcustom hl-line-fringe-indicator-char "X"
  "Character used to draw the indicator on character terminals."
  :type 'string)

(defcustom hl-line-fringe-indicator-bitmap
  (if (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'hl-line-fringe-indicator-bitmap-1
        (vector #b00000000
                #b01000000
                #b01100000
                #b01110000
                #b01111000
                #b01110000
                #b01100000
                #b01000000
                #b00000000)
        nil nil 'center)
    'vertical-bar)
  "Fringe bitmap to use for the indicator.
See `fringe-bitmaps' for other available ones, create one with
`define-fringe-bitmap' or use package `fringe-helper' to create one."
  :type '(restricted-sexp :match-alternatives (fringe-bitmap-p)))

(defcustom hl-line-fringe-indicator-bitmap-inactive
  (if (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'hl-line-fringe-indicator-bitmap-2
        (vector #b01000000
                #b00100000
                #b00010000
                #b00001000
                #b00010000
                #b00100000
                #b01000000)
        nil nil 'center)
    'horizontal-bar)
  "Fringe bitmap to use for the indicator if buffer is inactive.
See `fringe-bitmaps' for other available ones, create one with
`define-fringe-bitmap' or use package `fringe-helper' to create one."
  :type '(restricted-sexp :match-alternatives (fringe-bitmap-p)))

(defcustom hl-line-fringe-indicator-sticky t
  "If nil the fringe indicator is only shown in the current buffer."
  :type 'boolean)

(defcustom hl-line-fringe-indicator-overlay-priority 1
  "Priority of fringe indicator overlay.
Higher priority overlays others with lower priority."
  :type 'integer)

(defcustom hl-line-fringe-global-ignored-major-modes nil
  "List of major modes to ignore with global mode."
  :type 'list)

;; ** Defface

(defface hl-line-fringe-line
  '((t :inherit highlight))
  "Face used to highlight the current line if buffer is active.")

(defface hl-line-fringe-line-inactive
  '((t :inherit lazy-highlight))
  "Face used to highlight the current line if buffer is inactive.
This only applies if `hl-line-fringe-line-sticky' is non-nil.")

(defface hl-line-fringe-indicator
  '((t :inherit font-lock-keyword-face))
  "Face used to indicate the current line in the fringe if buffer is active.")

(defface hl-line-fringe-indicator-inactive
  '((t :inherit font-lock-comment-face))
  "Face used to indicate the current line in the fringe if buffer is inactive.
This only applies if `hl-line-fringe-indicator-sticky' is non-nil.")

;; * Variables

;; TODO Can be removed? See auxiliary function.
(defvar hl-line-fringe--mode-prefix "hl-line-fringe-"
  "Prefix used for variables and function names.")

(defvar hl-line-fringe--line-overlays nil
  "List of overlays used to highlight the current line.")

(defvar hl-line-fringe--line-overlays-recycle-bin nil
  "List of unused (deleted) overlays to highlight the current line.")

(defvar hl-line-fringe--indicator-overlays nil
  "List of overlays used to indicate the current line in the fringe.")

(defvar hl-line-fringe--indicator-overlays-recycle-bin nil
  "List of unused (deleted) overlays to indicate current line in the fringe.")

(defvar hl-line-fringe--buffers nil
  "List of buffers with `hl-line-fringe-mode' on.")

(defvar hl-line-fringe--selected-window nil
  "Store focused window to detect changes.")

;; * Functions

;; ** Make overlays

;; Make the overlays. Recycle existing overlays if possible.

(defun hl-line-fringe--make-all-overlays ()
  "Make all overlays in all windows if appropriate."
  (walk-windows (lambda (win)
                  (unless (window-minibuffer-p win)
                    (with-current-buffer (window-buffer win)
                      (hl-line-fringe--make-both-overlays win))))
                nil t))

(defun hl-line-fringe--make-both-overlays (win)
  "Make both overlays at current line in window WIN if appropriate."
  (when hl-line-fringe-line (hl-line-fringe--make-overlay 'line win))
  (when hl-line-fringe-indicator (hl-line-fringe--make-overlay 'indicator win)))

(defun hl-line-fringe--make-overlay (type win)
  "Make overlay of TYPE in window WIN.
TYPE can be 'line or 'indicator."
  (let* ((ovs-sym (hl-line-fringe--get-overlays type))
         (bin-sym (hl-line-fringe--get-bin type))
         (ovs (symbol-value ovs-sym))
         (bin (symbol-value bin-sym))
         ov)
    ;; Check if there is already a overlay in the window.
    (setq ov (hl-line-fringe--get-overlay-in-window type win))
    (unless (overlayp ov)
      ;; Else take overlay from recycle bin if exists or make a new one.
      (if bin
          (progn
            (setq ov (pop bin))
            ;; Update the global recycle bin variable.
            (set bin-sym bin))
        (setq ov (make-overlay 1 1)))
      ;; Add the overlay to the stored overlay lists to be able to delete unused
      ;; overlays afterwards.
      (set ovs-sym (push ov ovs))
      ;; Store the overlay as a window parameter.
      (set-window-parameter win (hl-line-fringe--get-win type) ov))
    ;; Set the fixed (don't change) properties.
    (hl-line-fringe--set-fixed-overlay-properties type win)
    ;; Set variable (need to be updated after buffer/window change) properties.
    (hl-line-fringe--set-variable-overlay-properties type win)
    ;; Move the overlay to the current line in the appropriate buffer.
    (hl-line-fringe--move-overlay type win)
    ov))

;; *** Set overlay properties

;; Set the fixed and variable properties for the overlays.
;; The fixed properties never change unless the mode is turned off and on again.
;; The variable properties need to change after changes of window configuration,
;; switching buffer or focusing another window to change the appearance of the
;; overlays.

(defun hl-line-fringe--set-fixed-overlay-properties (type win)
  "Set initial properties for line overlay in window WIN."
  (let ((ov (hl-line-fringe--get-overlay-in-window type win)))
    ;; Set the overlay properties.
    (overlay-put ov 'priority (if (equal type 'line)
                                  hl-line-fringe-line-overlay-priority
                                hl-line-fringe-indicator-overlay-priority))
    (overlay-put ov 'hl-line-fringe t)
    ov))

(defun hl-line-fringe--set-variable-overlay-properties (type win)
  "Set initial properties for line overlay in window WIN."
  (let ((ov (hl-line-fringe--get-overlay-in-window type win))
        (active (eq win (selected-window))))
    (if (equal type 'line)
        (overlay-put ov 'face (if active
                                  'hl-line-fringe-line
                                'hl-line-fringe-line-inactive))
      (let ((fa 'hl-line-fringe-indicator)
            (ind hl-line-fringe-indicator-bitmap))
        ;; Change face and bitmap to use if buffer is inactive.
        (unless active
          (setq fa 'hl-line-fringe-indicator-inactive)
          (setq ind hl-line-fringe-indicator-bitmap-inactive))
        (overlay-put ov 'before-string
                     (propertize hl-line-fringe-indicator-char
                                 'display `(left-fringe ,ind ,fa)))))
    ;; Make it only be visible in the current window. This is important when
    ;; the same buffer is visible in multiple windows.
    (overlay-put ov 'window win)
    ov))

;; *** Move overlays

;; Move the overlays to the current line.

(defun hl-line-fringe--move-both-overlays (win)
  "Move both overlays in window WIN to current line if appropriate."
  (when hl-line-fringe-line (hl-line-fringe--move-line-overlay win))
  (when hl-line-fringe-indicator (hl-line-fringe--move-indicator-overlay win)))

(defun hl-line-fringe--move-overlay (type win)
  "Move line overlay in window WIN to current line."
  (save-excursion
    (goto-char (window-point win))
    (let* ((beg (line-beginning-position))
          (end (if (equal type 'line) (line-beginning-position 2) beg))
          (ov (hl-line-fringe--get-overlay-in-window type win)))
      (move-overlay ov beg end (current-buffer)))))

;; *** Update overlays

(defun hl-line-fringe--update-all-overlays ()
  "Update all overlays in all windows if appropriate.")

(defun hl-line-fringe--update-both-overlays (win)
  "Update both overlays in window WIN if appropriate.")

;; *** Delete overlays.

;; A deleted overlay continues to exist as a Lisp object, and its property list
;; is unchanged, but it ceases to be attached to the buffer it belonged to, and
;; ceases to have any effect on display. Therefore this mode stores deleted
;; overlays in a recycle bin to be able to reause them.

(defun hl-line-fringe--delete-all-overlays ()
  "Delete all overlays and move to recycle bin."
  (hl-line-fringe--delete-overlay 'line t)
  (hl-line-fringe--delete-overlay 'indicator t))

(defun hl-line-fringe--delete-unused-overlays ()
  "Delete unused overlays and move to recycle bin."
  (hl-line-fringe--delete-overlay 'line nil)
  (hl-line-fringe--delete-overlay 'indicator nil))

(defun hl-line-fringe--delete-overlay (type all)
  "Delete overlays and move to recycle bin to reuse later on.
TYPE can be 'line or 'indicator. If ALL is non-nil all overlays are deleted and
moved else only unused overlays."
  (let* ((ovs-sym (hl-line-fringe--get-overlays type))
         (bin-sym (hl-line-fringe--get-bin type))
        (ovs (symbol-value ovs-sym))
        (bin (symbol-value bin-sym)))
    (dolist (item ovs)
      (when (or all (not (window-live-p (overlay-get item 'window))))
        ;; Remove overlay from list.
        (setq ovs (delq item ovs))
        ;; Delete the overlay.
        (delete-overlay item)
        ;; Add to recycle bin.
        (push item bin)))
    ;; Set the symbols to be able to change them.
    ;; https://stackoverfow.com/questions/1249991/variable-references-in-lisp
    (set ovs-sym ovs)
    (set bin-sym bin)))

;; *** Get overlays

(defun hl-line-fringe--get-overlay-in-window (type win)
  "Get the overlay of TYPE 'line or 'indicator in window WIN.
TYPE can be 'line or 'indicator."
  (window-parameter win (hl-line-fringe--get-win type)))

;; ** Auxiliary

(defun hl-line-fringe--get-overlays (type)
  "Get the symbol of the overlays list depending on TYPE.
TYPE can be 'line or 'indicator."
  (cond ((equal type 'line) 'hl-line-fringe--line-overlays)
        ((equal type 'indicator) 'hl-line-fringe--indicator-overlays)
        (t (error "TYPE is not valid."))))

(defun hl-line-fringe--get-bin (type)
  "Get the symbol of the recycle bin depending on TYPE.
TYPE can be 'line or 'indicator."
  (cond
   ((equal type 'line) 'hl-line-fringe--line-overlays-recycle-bin)
   ((equal type 'indicator) 'hl-line-fringe--indicator-overlays-recycle-bin)
   (t (error "TYPE is not valid."))))

(defun hl-line-fringe--get-win (type)
  "Get the symbol to use for window-parameter depending on TYPE.
TYPE can be 'line or 'indicator."
  (cond ((equal type 'line) 'hl-line-fringe-line)
        ((equal type 'indicator) 'hl-line-fringe-indicator)
        (t (error "TYPE is not valid."))))

;; TODO probably not needed, delete?
(defun hl-line-fringe--get-symbol (private type prefix suffix)
  "Get the symbol of a hl-line-fringe variable or function.
Returns nil if symbol doesn't exist. If PRIVATE is non-nil another \"-\" is
added to `hl-line-fringe--mode-prefix'. The TYPE can be 'line, 'indicator or
shorthand 'indi. The PREFIX is added before the type, the SUFFIX after the
type."
  (let* ((ov-type
         (cond ((equal type 'line) "line")
               ((or (equal type 'indicator)
                    (equal type 'indi))
                "indicator")
               (t (error "TYPE is not valid.")))))
    (intern-soft
     (concat hl-line-fringe--mode-prefix
             (when private "-")
             (when prefix (concat prefix "-"))
             ov-type
             (when suffix (concat "-" suffix))))))

;; * Events

;; Functions triggered by hooks or advising.

;; TODO needs to be tested, especially `selected-window'.
;; https://emacs.stackexchange.com/questions/13842/how-to-get-the-window-for-mode-line-format

;; * Init

(defun hl-line-fringe--global-init ()
  "Initialize `global-hl-line-fringe-mode'."
  (hl-line-fringe--make-all-overlays))

;; * Destroy


;; * Footer

(provide 'hl-line-fringe)

;;; hl-line-fringe.el ends here
