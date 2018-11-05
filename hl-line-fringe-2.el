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

(defvar hl-line-fringe--line-overlays nil
  "List of overlays used to highlight the current line.")

(defvar hl-line-fringe--indicator-overlays nil
  "List of overlays used to indicate the current line in the fringe.")

(defvar hl-line-fringe--line-beginning-position nil
  "Stored `line-beginning-position' in the active buffer.")

(defvar hl-line-fringe--buffer nil
  "Active buffer.")

;; * Functions

;; ** Make overlays

(defun hl-line-fringe--make-overlays (win)
  ""
  (when hl-line-fringe-line
    (hl-line-fringe--make-line-overlay win))
  (when hl-line-fringe-indicator
    (hl-line-fringe--make-indicator-overlay win)))

(defun hl-line-fringe--make-line-overlay (win)
  ""
  (unless (overlayp (window-parameter win 'hl-line-fringe-line))
    (with-current-buffer (window-buffer win)
      (let ((ov (make-overlay
                 (line-beginning-position)
                 (line-beginning-position 2)))
            (fa 'hl-line-fringe-line))
        (overlay-put ov 'priority hl-line-fringe-line-overlay-priority)
        (overlay-put ov 'window win)
        (unless (equal (hl-line-fringe--buffer-status (current-buffer))
                       'active)
          (setq fa 'hl-line-fringe-line-inactive))
        (overlay-put ov 'face fa)
        (set-window-parameter win 'hl-line-fringe-line ov)))))

(defun hl-line-fringe--make-indicator-overlay (win)
  ""
  (unless (overlayp (window-parameter win 'hl-line-fringe-indicator))
    (with-current-buffer (window-buffer win)
      (let* ((line-beg (line-beginning-position))
             (ov (make-overlay line-beg line-beg))
             (fa 'hl-line-fringe-indicator)
             (ind hl-line-fringe-indicator-bitmap))
        (overlay-put ov 'priority hl-line-fringe-indicator-overlay-priority)
        (overlay-put ov 'window win)
        (unless (equal (hl-line-fringe--buffer-status (current-buffer))
                       'active)
          (setq fa 'hl-line-fringe-indicator-inactive)
          (setq ind hl-line-fringe-indicator-bitmap-inactive))
        (overlay-put ov 'before-string
                     (propertize hl-line-fringe-indicator-char
                                 'display
                                 `(left-fringe
                                   ,ind
                                   ,fa)))
        (set-window-parameter win 'hl-line-fringe-indicator ov)))))

;; ** Move overlays

(defun hl-line-fringe--move-overlays (win)
  ""
  (hl-line-fringe--move-line-overlay win)
  (hl-line-fringe--move-indicator-overlay win))

(defun hl-line-fringe--move-indicator-overlay (win)
  ""
  (with-current-buffer (window-buffer win)
    (let ((beg (line-beginning-position))
          (ov (window-parameter win 'hl-line-fringe-indicator)))
      (unless (overlayp ov)
        (setq ov (hl-line-fringe--make-indicator-overlay win)))
      (move-overlay ov beg beg))))

(defun hl-line-fringe--move-indicator-overlay (win)
  ""
  (with-current-buffer (window-buffer win)
    (let ((beg (line-beginning-position))
          (ov (window-parameter win 'hl-line-fringe-indicator)))
      (unless (overlayp ov)
        (setq ov (hl-line-fringe--make-indicator-overlay win)))
      (move-overlay ov beg beg))))

;; ** Update overlays

;; * Auxiliary

(defun hl-line-fringe--buffer-status (buf)
  "Get the status of buffer BUF."
  (cond ((eq buf (window-buffer (selected-window))) 'active)
        ((get-buffer-window buf) 'visible)
        (t 'hidden)))

;; * Init

(defun hl-line-fringe--init ()
  "Initialize the overlays in each live window."
  (walk-windows (lambda (win)
                  (with-current-buffer (window-buffer win)
                    ())))
  )

;; * Destroy


;; * Footer

(provide 'hl-line-fringe)

;;; hl-line-fringe.el ends here
