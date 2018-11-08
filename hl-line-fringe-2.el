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

(defvar hl-line-fringe--overlays nil
  "List of overlays used.")

(defvar hl-line-fringe--deleted-overlays nil
  "List of deleted and unused overlays.")

(defvar hl-line-fringe--buffers nil
  "List of buffers with `hl-line-fringe-mode' on.")

(defvar hl-line-fringe--selected-window nil
  "Save `selected-window'.")

(defvar hl-line-fringe--window-buffer nil
  "Save `window-buffer'.")

(defvar hl-line-fringe--line-beginning-position nil
  "Save `line-beginning-position'.")

;; * Functions

;; ** Make overlays

;; Make the overlays. Recycle existing overlays if possible.

(defun hl-line-fringe--make-and-update-all-overlays ()
  "Make and update both overlays in all windows."
  (walk-windows
   (lambda (win)
     (unless (window-minibuffer-p win)
       (with-current-buffer (window-buffer win)
         (hl-line-fringe--make-both-overlays win)
         (hl-line-fringe--update-both-overlays win))))
   nil t))

(defun hl-line-fringe--make-both-overlays (win)
  "Make both overlays at current line in window WIN if appropriate."
  (when hl-line-fringe-line (hl-line-fringe--make-overlay 'line win))
  (when hl-line-fringe-indicator (hl-line-fringe--make-overlay 'indicator win)))

(defun hl-line-fringe--make-overlay (type win)
  "Make overlay of TYPE in window WIN.
TYPE can be 'line or 'indicator."
  (let (ov)
    ;; Check if there is already a overlay in the window.
    (setq ov (hl-line-fringe--get-overlay-in-window type win))
    (unless (overlayp ov)
      ;; Else take overlay from recycle bin if exists or make a new one.
      (if hl-line-fringe--deleted-overlays
          (progn
            (setq ov (pop hl-line-fringe--deleted-overlays))
            ;; Delete existing properties.
            (overlay-put ov 'face nil)
            (overlay-put ov 'before-string nil))
        (setq ov (make-overlay 1 1)))
      ;; Add the overlay to the stored overlay lists to be able to delete unused
      ;; overlays afterwards.
      (push ov hl-line-fringe--overlays)
      ;; Store the overlay as a window parameter.
      (set-window-parameter win (hl-line-fringe--get-win type) ov))
    ;; Set the fixed (don't change) properties.
    (hl-line-fringe--set-fixed-overlay-properties type win)
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
    (overlay-put ov 'hl-line-fringe type)
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
  "Move both overlays in window WIN to current line."
  (when hl-line-fringe-line (hl-line-fringe--move-overlay 'line win))
  (when hl-line-fringe-indicator (hl-line-fringe--move-overlay 'indicator win)))

(defun hl-line-fringe--move-overlay (type win)
  "Move overlay of TYPE in window WIN to current line."
  (when (or (bound-and-true-p hl-line-fringe-mode)
            (bound-and-true-p global-hl-line-fringe-mode))
    (save-excursion
      (goto-char (window-point win))
      (let* ((beg (line-beginning-position))
             (end (if (equal type 'line) (line-beginning-position 2) beg))
             (ov (hl-line-fringe--get-overlay-in-window type win)))
        (when (overlayp ov)
          (move-overlay ov beg end (current-buffer)))))))

;; *** Update overlays

(defun hl-line-fringe--update-all-overlays ()
  "Make and update both overlays in all windows."
  (walk-windows
   (lambda (win)
     (unless (window-minibuffer-p win)
       (with-current-buffer (window-buffer win)
         (hl-line-fringe--update-both-overlays win))))
   nil t))

(defun hl-line-fringe--update-both-overlays (win)
  "Update (move and update properties) both overlays in window win."
  (let ((active (eq win (selected-window))))
    ;; check if mode or global mode is on.
    (if (or (bound-and-true-p hl-line-fringe-mode)
            (bound-and-true-p global-hl-line-fringe-mode))
        (progn
          (when hl-line-fringe-line
            (if (or active hl-line-fringe-line-sticky)
                (hl-line-fringe--update-overlay 'line win)
              (hl-line-fringe--hide-overlay 'line win)))
          (when hl-line-fringe-indicator
            (if (or active hl-line-fringe-indicator-sticky)
                (hl-line-fringe--update-overlay 'indicator win)
              (hl-line-fringe--hide-overlay 'indicator win))))
      ;; if mode is not on hide.
      (hl-line-fringe--hide-overlay 'line win)
      (hl-line-fringe--hide-overlay 'indicator win))))

(defun hl-line-fringe--update-overlay (type win)
  "Update (move and update variable properties) overlay of TYPE in window WIN."
  (hl-line-fringe--set-variable-overlay-properties type win)
  (hl-line-fringe--move-overlay type win))

;; *** Hide overlays

;; delete the overlay but don't remove it from the window-parameter to sort of
;; hide it to show later on again if appropriate.

(defun hl-line-fringe--hide-overlay (type win)
  "hide the overlay of type in window win.
this deletes the overlay but doesn't remove the window-parameter."
  (let ((ov (hl-line-fringe--get-overlay-in-window type win)))
    (when (overlayp ov) (delete-overlay ov))))

;; *** delete overlays

;; a deleted overlay continues to exist as a lisp object, and its property list
;; is unchanged, but it ceases to be attached to the buffer it belonged to, and
;; ceases to have any effect on display. therefore this mode stores deleted
;; overlays in `hl-line-fringe--deleted-overlays' to be able to reause them.

(defun hl-line-fringe--delete-all-overlays ()
  "delete all overlays, move to deleted overlays and set window-params to nil."
  (hl-line-fringe--delete-overlay 'line t)
  (hl-line-fringe--delete-overlay 'indicator t))

(defun hl-line-fringe--delete-unused-overlays ()
  "delete unused overlays and move deleted overlays."
  (hl-line-fringe--delete-overlay 'line nil)
  (hl-line-fringe--delete-overlay 'indicator nil))

(defun hl-line-fringe--delete-overlay (type all)
  "delete overlays and move to deleted overlays.
type can be 'line or 'indicator. if all is non-nil all overlays are deleted and
moved else only unused. Window-parameters are set to nil if appropriate."
  (dolist (item hl-line-fringe--overlays)
    ;; TODO test
    (when (eq (overlay-get item 'hl-line-fringe) type)
      (let* ((win (overlay-get item 'window))
             (live (window-live-p win)))
        (when (or all (not live))
          ;; Remove overlay from list.
          (setq hl-line-fringe--overlays (delq item hl-line-fringe--overlays))
          ;; Delete the overlay.
          (delete-overlay item)
          ;; Remove window-param if window still exists (only applies if all).
          (when live
            ;; TODO check kind of overlay and
            (set-window-parameter win (hl-line-fringe--get-win type) nil))
          ;; Add to recycle bin.
          (push item hl-line-fringe--deleted-overlays))))))

;; *** Get overlays

(defun hl-line-fringe--get-overlay-in-window (type win)
  "Get the overlay of TYPE 'line or 'indicator in window WIN.
TYPE can be 'line or 'indicator."
  (window-parameter win (hl-line-fringe--get-win type)))

;; ** Auxiliary

(defun hl-line-fringe--get-win (type)
  "Get the symbol to use for window-parameter depending on TYPE.
TYPE can be 'line or 'indicator."
  (cond ((equal type 'line) 'hl-line-fringe-line)
        ((equal type 'indicator) 'hl-line-fringe-indicator)
        (t (error "TYPE is not valid."))))

(defun hl-line-fringe--set-tracking-variables (win buf line-beg)
  (unless (window-minibuffer-p (selected-window))
    (when win
      (setq hl-line-fringe--selected-window (selected-window)))
    (when buf
      (setq hl-line-fringe--window-buffer (window-buffer (selected-window))))
    (when line-beg
      (setq hl-line-fringe--line-beginning-position
            (hl-line-fringe--get-line-beginning-position (selected-window))))))

(defun hl-line-fringe--get-line-beginning-position (win &optional next-line)
  "Get the `line-beginning-position' of buffer visible in window WIN.
If NEXT-LINE is non-nil get the beginning of the next line."
  (save-excursion
    (goto-char (window-point win))
    (if next-line
        (line-beginning-position 2)
      (line-beginning-position))))

;; * Events

;; Functions triggered by hooks or advising.

;; TODO needs to be tested, especially `select-window'.
;; https://emacs.stackexchange.com/questions/13842/how-to-get-the-window-for-mode-line-format

;; TODO test if this is not needed
;; In case `kill-all-local-variables' is called.
;; (add-hook 'change-major-mode-hook #'hl-line-fringe--delete nil t)

(defun hl-line-fringe--on-post-command-hook ()
  "Update the overlays on global `post-command-hook'."
  (unless (window-minibuffer-p (selected-window))
    (let* ((sel-win (selected-window))
           (sel-buf (window-buffer sel-win))
           (sel-line-beg (hl-line-fringe--get-line-beginning-position sel-win))
           (hl-win hl-line-fringe--selected-window)
           (hl-buf hl-line-fringe--window-buffer)
           (hl-line-beg hl-line-fringe--line-beginning-position))
      (cond
       ;; Selected window has changed.
       ((not (eq sel-win hl-win))
        (with-current-buffer sel-buf
          (hl-line-fringe--update-both-overlays sel-win))
        (with-current-buffer hl-buf
          (hl-line-fringe--update-both-overlays hl-win))
        (hl-line-fringe--set-tracking-variables t t t))
       ;; Buffer has changed.
       ((not (eq sel-buf hl-buf))
        (with-current-buffer sel-buf
          (hl-line-fringe--update-both-overlays sel-win))
        (hl-line-fringe--set-tracking-variables nil t t))
       ;; Current line has changed.
       ((not (equal sel-line-beg hl-line-beg))
        (hl-line-fringe--move-both-overlays sel-win)
        (hl-line-fringe--set-tracking-variables nil nil t))))))

(defun hl-line-fringe--on-window-configuration-change-hook ()
  "Update the overlays on global `window-configuration-change-hook'."
  (hl-line-fringe--make-and-update-all-overlays)
  (hl-line-fringe--set-tracking-variables t t t))

;; * Minor modes

;; ** Local

;;;###autoload
(define-minor-mode hl-line-fringe-mode
  "Toggle line highlighting and/or indication in the fringe in current buffer."
  :global nil
  :group 'hl-line-fringe
  :lighter " hlf"
  (if hl-line-fringe-mode
      (hl-line-fringe--init-local)
    (hl-line-fringe--destroy-local)))

(defun hl-line-fringe--init-local ()
  "Initialize `hl-line-fringe-mode'."
  (push (current-buffer) hl-line-fringe--buffers)
  (hl-line-fringe--init))

(defun hl-line-fringe--destroy-local ()
  "Destroy `hl-line-fringe-mode'"
  (setq hl-line-fringe--buffers (delq (current-buffer) hl-line-fringe--buffers))
  (hl-line-fringe--maybe-destroy))

;; ** Global

;;;###autoload
(define-minor-mode global-hl-line-fringe-mode
  "Toggle line highlighting and/or indication in the fringe in all buffers."
  :global t
  :group 'hl-line-fringe
  :lighter " g-hlf"
  (if global-hl-line-fringe-mode
      (hl-line-fringe--init-global)
    (hl-line-fringe--destroy-global)))

(defun hl-line-fringe--init-global ()
  "Initialize `global-hl-line-fringe-mode'."
  (hl-line-fringe--init))

(defun hl-line-fringe--destroy-global ()
  "Destroy `global-hl-line-fringe-mode'"
  (hl-line-fringe--maybe-destroy))

;; ** Auxiliary

(defun hl-line-fringe--init ()
  "Initialize the mode."
  (hl-line-fringe--set-tracking-variables t t t)
  (hl-line-fringe--make-and-update-all-overlays)
  (add-hook 'post-command-hook #'hl-line-fringe--on-post-command-hook)
  (add-hook 'window-configuration-change-hook
            #'hl-line-fringe--on-window-configuration-change-hook))

(defun hl-line-fringe--maybe-destroy ()
  "Destroy the mode if appropriate else update the overlays."
  ;; Remove killed buffers from list.
  (dolist (item hl-line-fringe--buffers)
    (unless (buffer-live-p item)
      (setq hl-line-fringe--buffers (delq item hl-line-fringe--buffers))))
  ;; Update all overlays if `global-hl-line-fringe-mode' global mode is on or
  ;; there are still buffers with `hl-line-fringe-mode' turned on. Else delete
  ;; all and remove the hooks.
  (if (or global-hl-line-fringe-mode hl-line-fringe--buffers)
      (hl-line-fringe--update-all-overlays)
    (hl-line-fringe--delete-all-overlays)
    (remove-hook 'post-command-hook #'hl-line-fringe--on-post-command-hook)
    (remove-hook 'window-configuration-change-hook
                 #'hl-line-fringe--on-window-configuration-change-hook)))

;; * Footer

(provide 'hl-line-fringe)

;;; hl-line-fringe.el ends here
