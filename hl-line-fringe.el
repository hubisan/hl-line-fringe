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

(defcustom hl-line-fringe-indicator-bitmap 'hl-line-fringe-indicator-bitmap-1
  "Fringe bitmap to use for the indicator.
See `fringe-bitmaps' for other available ones, create one with
`define-fringe-bitmap' or use package `fringe-helper' to create one."
  :type '(restricted-sexp :match-alternatives (fringe-bitmap-p)))

(defcustom hl-line-fringe-indicator-bitmap-inactive
  'hl-line-fringe-indicator-bitmap-2
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

(defvar-local hl-line-fringe--line-overlay nil
  "Overlay used to highlight the current line.")

(defvar-local hl-line-fringe--indicator-overlay nil
  "Overlay used to indicate the current line in the fringe.")

(defvar hl-line-fringe--previous-buffer nil
  "Previous visited buffer. Needed to update overlays in inactive buffers.")

;; * Fringe bitmaps

(define-fringe-bitmap 'hl-line-fringe-indicator-bitmap-1
  (vector #b10000000
          #b11000000
          #b11100000
          #b11110000
          #b11100000
          #b11000000
          #b10000000)
  nil nil 'center)

(define-fringe-bitmap 'hl-line-fringe-indicator-bitmap-2
  (vector #b10000000
          #b11000000
          #b10100000
          #b10010000
          #b10100000
          #b11000000
          #b10000000)
  nil nil 'center)

;; * Functions

;; ** Make overlays

(defun hl-line-fringe--line-make ()
  "Make the line highlight overlay if it doesn't exist."
  (unless (overlayp hl-line-fringe--line-overlay)
    (let ((ov (make-overlay (point) (point))))
      (overlay-put ov 'priority hl-line-fringe-line-overlay-priority)
      (overlay-put ov 'face 'hl-line-fringe-line)
      (setq hl-line-fringe--line-overlay ov))))

(defun hl-line-fringe--indicator-make ()
  "Make the fringe indicator overlay if it doesn't exist."
  (unless (overlayp hl-line-fringe--indicator-overlay)
    (let ((ov (make-overlay (point) (point))))
      (overlay-put ov 'priority hl-line-fringe-indicator-overlay-priority)
      (overlay-put ov 'before-string
                   (propertize hl-line-fringe-indicator-char
                               'display
                               `(left-fringe
                                 ,hl-line-fringe-indicator-bitmap
                                 hl-line-fringe-indicator)))
      (setq hl-line-fringe--indicator-overlay ov))))

;; ** Move overlays

(defun hl-line-fringe--line-move ()
  "Move the line highlight overlay to current line."
  (move-overlay hl-line-fringe--line-overlay
                (line-beginning-position)
                (line-beginning-position 2)))

(defun hl-line-fringe--indicator-move ()
  "Move the fringe indicator overlay to current line."
  (let ((beg (line-beginning-position)))
    (move-overlay hl-line-fringe--indicator-overlay beg beg)))

;; ** Delete overlays

(defun hl-line-fringe--delete ()
  "Delete both overlays if they exist."
  (hl-line-fringe--line-delete)
  (hl-line-fringe--indicator-delete))

(defun hl-line-fringe--delete-if-exists (overlay)
  "Delete the OVERLAY if it exists."
  (when (overlayp overlay) (delete-overlay overlay)))

(defun hl-line-fringe--line-delete ()
  "Delete the line highlight overlay if it exists."
  (hl-line-fringe--delete-if-exists hl-line-fringe--line-overlay))

(defun hl-line-fringe--indicator-delete ()
  "Delete the fringe indicator overlay if it exists."
  (hl-line-fringe--delete-if-exists hl-line-fringe--indicator-overlay))

;; ** Update overlays

(defun hl-line-fringe--update ()
  "Update the line highlight and the fringe indicator."
  (hl-line-fringe--line-update)
  (hl-line-fringe--indicator-update))

(defun hl-line-fringe--line-update ()
  "Update the line highlight in the current buffer."
  (when (bound-and-true-p hl-line-fringe-mode)
    (if hl-line-fringe-line
        (progn
          ;; Create the overlay if it doesn't exist.
          (hl-line-fringe--line-make)
          ;; Apply active face.
          (hl-line-fringe--line-change-face nil)
          ;; Move to the current line.
          (hl-line-fringe--line-move))
      ;; If `hl-line-fringe-line' is nil delete existing overlay.
      (hl-line-fringe--line-delete))))

(defun hl-line-fringe--indicator-update ()
  "Update the fringe indicator in the current buffer."
  (when (bound-and-true-p hl-line-fringe-mode)
    (if hl-line-fringe-indicator
        (progn
          ;; Create the overlay if it doesn't exist.
          (hl-line-fringe--indicator-make)
          ;; Apply active face and indicator bitmap.
          (hl-line-fringe--indicator-change-face-and-indicator nil)
          ;; Move to the current line.
          (hl-line-fringe--indicator-move))
      ;; If `hl-line-fringe-indicator' is nil delete existing overlay.
      (hl-line-fringe--indicator-delete))))

;; ** Update previous overlays

(defun hl-line-fringe--update-previous ()
  "Update the line highlight and fringe indicator in previous buffer.
This either deletes the overlay or changes it's face to inactive."
  (unless (minibufferp)
    (let ((buf hl-line-fringe--previous-buffer)
          (cur (current-buffer)))
      ;; Update overlays if current buffer is not the previous one.
      (when (and (buffer-live-p buf) ; Check if exists
                 (not (eq buf cur))) ; Check if not the same as current
        (hl-line-fringe--line-update-previous)
        (hl-line-fringe--indicator-update-previous)))
    ;; Update the previous buffer.
    (setq hl-line-fringe--previous-buffer (current-buffer))))

(defun hl-line-fringe--line-update-previous ()
  "Update the highlight in previous buffer if mode is/was active.
If `hl-line-fringe-line-sticky' is non-nil this changes the face to inactive.
Else the overlay is deleted."
  (with-current-buffer hl-line-fringe--previous-buffer
    (when (overlayp hl-line-fringe--line-overlay)
      (if hl-line-fringe-line-sticky
          ;; Set face to inactive.
          (hl-line-fringe--line-change-face t)
        (hl-line-fringe--line-delete)))))

(defun hl-line-fringe--indicator-update-previous ()
  "Update indicator in the previous buffer if mode is/was active.
If `hl-line-fringe-indicator-sticky' is non-nil this changes the face to
inactive. Else the overlay is deleted."
  (with-current-buffer hl-line-fringe--previous-buffer
    (when (overlayp hl-line-fringe--indicator-overlay)
      (if hl-line-fringe-indicator-sticky
          ;; Set face and indicator to inactive.
          (hl-line-fringe--indicator-change-face-and-indicator t)
        (hl-line-fringe--indicator-delete)))))

;; ** Change overlay faces and indicator

(defun hl-line-fringe--line-change-face (inactive)
  "Change the face of line highlight to INACTIVE if non-nil.
Else to default face."
  (let ((ov hl-line-fringe--line-overlay)
        (fa 'hl-line-fringe-line))
    (when inactive (setq fa 'hl-line-fringe-line-inactive))
    (overlay-put ov 'face fa)))

(defun hl-line-fringe--indicator-change-face-and-indicator (inactive)
  "Change the face of fringe indicator and the bitmap to INACTIVE if non-nil.
Else to defaults."
  (let ((ov hl-line-fringe--indicator-overlay)
        (fa 'hl-line-fringe-indicator)
        (ind hl-line-fringe-indicator-bitmap))
    (when inactive
      (setq fa 'hl-line-fringe-indicator-inactive)
      (setq ind hl-line-fringe-indicator-bitmap-inactive))
    (overlay-put ov 'before-string
                 (propertize hl-line-fringe-indicator-char
                             'display
                             `(left-fringe
                               ,ind
                               ,fa)))))

;; * Minor mode

;;;###autoload
(define-minor-mode hl-line-fringe-mode
  "Toggle highlighting current line or/and indication in the fringe."
  :group 'hl-line-fringe
  :lighter " hlf"
  (if hl-line-fringe-mode
      (progn
        ;; In case `kill-all-local-variables' is called.
        (add-hook 'change-major-mode-hook #'hl-line-fringe--delete nil t)
        (hl-line-fringe--update)
        (add-hook 'post-command-hook #'hl-line-fringe--update nil t)
        (add-hook 'post-command-hook #'hl-line-fringe--update-previous nil nil))
    ;; Remove all hooks.
    (remove-hook 'post-command-hook #'hl-line-fringe--update t)
    (remove-hook 'post-command-hook #'hl-line-fringe--update-previous nil)
    (hl-line-fringe--delete)))

;; * Global minor mode

(defun hl-line-fringe--global-init ()
  "Turn on `hl-line-fringe-mode' if appropriate."
  (unless (or (minibufferp)
              (eq major-mode 'fundamental-mode)
              (memq major-mode hl-line-fringe-global-ignored-major-modes))
    (hl-line-fringe-mode 1)
    (hl-line-fringe--line-change-face t)
    (hl-line-fringe--indicator-change-face-and-indicator t)))

;;;###autoload
(define-globalized-minor-mode global-hl-line-fringe-mode hl-line-fringe-mode
  hl-line-fringe--global-init :require 'hl-line-fringe)

(provide 'hl-line-fringe)

;;; hl-line-fringe.el ends here
