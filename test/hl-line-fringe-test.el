;;; test-hl-line-fringe.el --- Tests for hl-line-fringe -*- lexical-binding:t -*-

;; Copyright (C) 2018 Daniel Hubmann

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

;; Testing of hl-line-fringe package.

;;; Code:

(require 'hl-line-fringe)
(require 'ert)

;; * Helpers

;; None.

;; * Minor mode

;; ** One buffer

(describe "After turning mode on in a buffer"
  :var (buf ol-line ol-indi)

  (before-each
    ;; Create buffer, insert text, turn on minor-mode.
    (setq buf (switch-to-buffer "file.txt"))
    (text-mode)
    (insert "1
---------------------
2--------------------
---------------------
----------3----------
---------------------
--------------------4
---------------------
5")
    (hl-line-fringe-mode)
    (setq ol-line hl-line-fringe--line-overlay)
    (setq ol-indi hl-line-fringe--indicator-overlay))

  (after-each
    ;; Kill created buffer.
    (kill-buffer buf))

  (it "both overlays exist in that buffer and mode is on."
    (expect (bound-and-true-p hl-line-fringe-mode) :to-be t)
    (expect (overlayp hl-line-fringe--line-overlay) :to-be t)
    (expect (overlayp hl-line-fringe--indicator-overlay) :to-be t))

  (it "both overlays are placed at the current line in that buffer."
    (let ((check
           (lambda (oll oli)
             ;; Looks like not called in batch mode.
             (run-hooks 'post-command-hook)
             (expect (overlay-start oll) :to-equal (line-beginning-position))
             (expect (overlay-end oll) :to-equal (line-beginning-position 2))
             (expect (overlay-start oli) :to-equal (line-beginning-position))
             (expect (overlay-end oli) :to-equal (line-beginning-position)))))
      (goto-char (point-min))
      (funcall check ol-line ol-indi)
      (search-forward "2")
      (beginning-of-line)
      (funcall check ol-line ol-indi)
      (search-forward "3")
      (funcall check ol-line ol-indi)
      (search-forward "4")
      (funcall check ol-line ol-indi)
      (search-forward "5")
      (funcall check ol-line ol-indi)))

  (it "both overlays have the correct properties."
    (expect (overlay-get ol-line 'face) :to-be 'hl-line-fringe-line)
    (expect (overlay-get ol-line 'priority)
            :to-be hl-line-fringe-line-overlay-priority)
    (expect (overlay-get ol-indi 'before-string)
            :to-equal hl-line-fringe-indicator-char)
    (expect
     (get-text-property 0 'display (overlay-get ol-indi 'before-string))
     :to-equal
     `(left-fringe ,hl-line-fringe-indicator-bitmap hl-line-fringe-indicator))
    (expect (overlay-get ol-indi 'priority)
            :to-be hl-line-fringe-indicator-overlay-priority))

  (it "the mode is off if the major mode is switched."
    (emacs-lisp-mode)
    (expect (bound-and-true-p hl-line-fringe-mode) :to-be nil))

  (it "both overlays are deleted in that buffer after deactivating the mode."
    (hl-line-fringe-mode -1)
    ;; A deleted overlay is not permanently disconnected. You can give it a
    ;; position in a buffer again by calling move-overlay. The overlay continues
    ;; to exist as a Lisp object. A deleted overlay has no position.
    (expect (overlay-start ol-line) :to-be nil)
    (expect (overlay-start ol-indi) :to-be nil)))

;; ** Multiple buffers

(describe "After turning mode on in two buffers"
  :var (buf-a buf-i)
  (before-all
    (setq hl-line-fringe-indicator-bitmap-inactive 'vertical-bar)
    (setq buf-i (switch-to-buffer "inactive.txt"))
    (text-mode)
    (hl-line-fringe-mode)
    (run-hooks 'post-command-hook)
    (setq buf-a (switch-to-buffer "active.txt"))
    (text-mode)
    (hl-line-fringe-mode)
    (run-hooks 'post-command-hook))
  (after-all
    (setq hl-line-fringe-indicator-bitmap-inactive
          'hl-line-fringe-indicator-bitmap-1)
    (kill-buffer buf-a)
    (kill-buffer buf-i))

  (it "the line overlay in the current and inactive buffer has the correct properties."
    (with-current-buffer buf-a
      (expect (overlay-get hl-line-fringe--line-overlay 'face)
              :to-be 'hl-line-fringe-line))
    (with-current-buffer buf-i
      (expect (overlay-get hl-line-fringe--line-overlay 'face)
              :to-be 'hl-line-fringe-line-inactive)))

  (it "the indicator overlay in the current and inactive buffer has the correct properties."
    (with-current-buffer buf-a
      (expect (get-text-property
               0 'display
               (overlay-get
                hl-line-fringe--indicator-overlay
                'before-string))
              :to-equal
              `(left-fringe
                ,hl-line-fringe-indicator-bitmap
                hl-line-fringe-indicator)))
    (with-current-buffer buf-i
      (expect (get-text-property
               0 'display
               (overlay-get
                hl-line-fringe--indicator-overlay
                'before-string))
              :to-equal
              `(left-fringe
                vertical-bar
                hl-line-fringe-indicator-inactive)))))

;; * Global mode

(describe "After turning the global mode on"
  :var (buf-a buf-txt-i buf-js)
  (before-each
    (setq buf-txt-i (switch-to-buffer "inactive.txt"))
    (text-mode)
    (js-mode)
    (setq buf-a (switch-to-buffer "active.el"))
    (emacs-lisp-mode)
    (spy-on 'hl-line-fringe--global-init :and-call-through))

  (after-each
    (global-hl-line-fringe-mode -1)
    (kill-buffer buf-a)
    (kill-buffer buf-txt-i)
    (kill-buffer buf-js))

  (it "the mode is called as many times as expected."
    (global-hl-line-fringe-mode)
    (expect 'hl-line-fringe--global-init :to-have-been-called-times 7))

  (it "the mode is activated in all appropriate buffers."
    (global-hl-line-fringe-mode)
    (mapcar (lambda (buf)
              (with-current-buffer buf
                (if (or (minibufferp)
                        (eq major-mode 'fundamental-mode))
                    (expect (bound-and-true-p hl-line-fringe-mode) :to-be nil)
                  (expect (bound-and-true-p hl-line-fringe-mode) :to-be t))))
            (buffer-list)))

  (it "the line overlays in active and inactive buffers have the correct properties."
    ;; Only properties that can be active or inactive.
    (global-hl-line-fringe-mode)
    (run-hooks 'post-command-hook)
    (let ((cbuf (current-buffer)))
      (mapcar (lambda (buf)
                (with-current-buffer buf
                  (when (bound-and-true-p hl-line-fringe-mode)
                    (if (equal buf cbuf)
                        (expect (overlay-get hl-line-fringe--line-overlay 'face)
                                :to-be 'hl-line-fringe-line)
                      (expect (overlay-get hl-line-fringe--line-overlay 'face)
                              :to-be 'hl-line-fringe-line-inactive)))))
              (buffer-list))))

  (it "the indicator overlays in active and inactive buffers have the correct properties."
    ;; Only properties that can be active or inactive.
    (let ((cbuf (current-buffer))
          (hl-line-fringe-indicator-bitmap-inactive 'vertical-bar))
      (global-hl-line-fringe-mode)
      (run-hooks 'post-command-hook)
      (mapcar (lambda (buf)
                (with-current-buffer buf
                  (when (bound-and-true-p hl-line-fringe-mode)
                    (if (equal buf cbuf)
                        (expect
                         (get-text-property
                          0 'display
                          (overlay-get
                           hl-line-fringe--indicator-overlay
                           'before-string))
                         :to-equal
                         `(left-fringe
                           ,hl-line-fringe-indicator-bitmap
                           hl-line-fringe-indicator))
                      (expect
                       (get-text-property
                        0 'display
                        (overlay-get
                         hl-line-fringe--indicator-overlay
                         'before-string))
                       :to-equal
                       `(left-fringe
                         vertical-bar
                         hl-line-fringe-indicator-inactive))))))
              (buffer-list))))

  (it "the mode is not turned on in ignored major modes."
    (let ((hl-line-fringe-global-ignored-major-modes '(js-mode)))
      (setq buf-js (switch-to-buffer "ignored.js"))
      (js-mode)
      (global-hl-line-fringe-mode)
      (with-current-buffer buf-js
        (expect (bound-and-true-p hl-line-fringe-mode) :to-be nil))))

  (it "when creating a new buffer the mode is turned on if buffer is appropriate."
    (let (buf1
          buf2
          (hl-line-fringe-global-ignored-major-modes '(js-mode)))
        (global-hl-line-fringe-mode)
        ;; Using find-file to create a buffer with the correct major mode.
        (setq buf1 (find-file "on.txt"))
        (expect (bound-and-true-p hl-line-fringe-mode) :to-be t)
        (setq buf1 (find-file "off.js"))
        (expect (bound-and-true-p hl-line-fringe-mode) :to-be nil)
        (kill-buffer buf1)
        (kill-buffer buf2)))

  (it "after switching major mode the mode is on if appropriate."
    (global-hl-line-fringe-mode)
    (with-current-buffer buf-a
      (expect (bound-and-true-p hl-line-fringe-mode) :to-be t)
      (emacs-lisp-mode)
      (expect (bound-and-true-p hl-line-fringe-mode) :to-be t)
      (fundamental-mode)
      (expect (bound-and-true-p hl-line-fringe-mode) :to-be nil)
      (emacs-lisp-mode)
      (expect (bound-and-true-p hl-line-fringe-mode) :to-be t))))

;; ** Customization

(describe "(Customization) If setting"
  :var (buf ol-line ol-indi)
  (before-each
    (setq buf (switch-to-buffer "file.txt"))
    (text-mode))
  (after-each
    (kill-buffer buf))

  (it "`hl-line-fringe-line' to `nil' the overlay is not created."
    (let ((hl-line-fringe-line nil))
      (hl-line-fringe-mode)
      (expect (overlayp hl-line-fringe--line-overlay) :to-be nil)
      (expect (overlayp hl-line-fringe--indicator-overlay) :to-be t)))

  (it "`hl-line-fringe-indicator' to `nil' the overlay is not created."
    (let ((hl-line-fringe-indicator nil))
      (hl-line-fringe-mode)
      (expect (overlayp hl-line-fringe--line-overlay) :to-be t)
      (expect (overlayp hl-line-fringe--indicator-overlay) :to-be nil)))

  (it "`hl-line-fringe-indicator-char' the char is changed."
    (let ((hl-line-fringe-indicator-char "Y"))
      (hl-line-fringe-mode)
      (expect (overlay-get hl-line-fringe--indicator-overlay 'before-string)
              :to-match hl-line-fringe-indicator-char)))

  (it "`hl-line-fringe-indicator-bitmap' the bitmap is changed."
    (let ((hl-line-fringe-indicator-bitmap 'vertical-bar))
      (hl-line-fringe-mode)
      (expect (get-text-property
               0 'display
               (overlay-get
                hl-line-fringe--indicator-overlay
                'before-string))
              :to-equal
              '(left-fringe
                vertical-bar
                hl-line-fringe-indicator))))

  (it "`hl-line-fringe-line-overlay-priority' the priority is changed."
    (let ((hl-line-fringe-line-overlay-priority 100))
      (hl-line-fringe-mode)
      (expect (overlay-get hl-line-fringe--line-overlay 'priority)
              :to-equal hl-line-fringe-line-overlay-priority)))

  (it "`hl-line-fringe-indicator-overlay-priority' the priority is changed."
    (let ((hl-line-fringe-indicator-overlay-priority 100))
      (hl-line-fringe-mode)
      (expect (overlay-get hl-line-fringe--indicator-overlay 'priority)
              :to-equal hl-line-fringe-indicator-overlay-priority)))

  (describe "`hl-line-fringe-line-sticky' to `nil' the overlay"
    :var (buf-1)
    (before-each
      (setq buf-1 (switch-to-buffer "file-1.txt"))
      (text-mode)
      (switch-to-buffer buf))
    (after-each
      (kill-buffer buf-1))

    (it "is deleted after deactivating the buffer."
      (let ((hl-line-fringe-line-sticky nil))
        (hl-line-fringe-mode)
        (run-hooks 'post-command-hook)
        (switch-to-buffer buf-1)
        (run-hooks 'post-command-hook)
        (expect
         (overlay-start (buffer-local-value 'hl-line-fringe--line-overlay buf))
         :to-be nil)))

    (it "and show again after activating the buffer."
      (let ((hl-line-fringe-line-sticky nil))
        (hl-line-fringe-mode)
        (switch-to-buffer buf-1)
        (run-hooks 'post-command-hook)
        (switch-to-buffer buf)
        (run-hooks 'post-command-hook)
        (expect (overlay-start hl-line-fringe--line-overlay) :not :to-be nil))))

  (describe "`hl-line-fringe-indicator-sticky' to `nil' the overlay"
    :var (buf-1)
    (before-each
      (setq buf-1 (switch-to-buffer "file-1.txt"))
      (text-mode)
      (switch-to-buffer buf))
    (after-each
      (kill-buffer buf-1))

    (it "is deleted after deactivating the buffer."
      (let ((hl-line-fringe-indicator-sticky nil))
        (hl-line-fringe-mode)
        (run-hooks 'post-command-hook)
        (switch-to-buffer buf-1)
        (run-hooks 'post-command-hook)
        (expect
         (overlay-start
          (buffer-local-value 'hl-line-fringe--indicator-overlay buf))
         :to-be nil)))

    (it "and shown again after activating the buffer."
      (let ((hl-line-fringe-indicator-sticky nil))
        (hl-line-fringe-mode)
        (run-hooks 'post-command-hook)
        (switch-to-buffer buf-1)
        (run-hooks 'post-command-hook)
        (switch-to-buffer buf)
        (run-hooks 'post-command-hook)
        (expect (overlay-start hl-line-fringe--indicator-overlay)
                :not :to-be nil))))

  (describe "`hl-line-fringe-indicator-bitmap-inactive' the"
    :var (buf-1)
    (before-each
      (setq buf-1 (switch-to-buffer "test-1.txt"))
      (text-mode)
      (switch-to-buffer buf))
    (after-each
      (kill-buffer buf-1))

    (it "correct bitmap is shown after deactivating the buffer."
      (let ((hl-line-fringe-indicator-bitmap-inactive 'vertical-bar))
        (hl-line-fringe-mode)
        (run-hooks 'post-command-hook)
        (switch-to-buffer buf-1)
        (run-hooks 'post-command-hook)
        (expect
         (get-text-property
          0 'display
          (overlay-get
           (buffer-local-value 'hl-line-fringe--indicator-overlay buf)
           'before-string))
         :to-equal
         `(left-fringe
           ,hl-line-fringe-indicator-bitmap-inactive
           hl-line-fringe-indicator-inactive))))

    (it "correct bitmap is shown after activating the buffer again."
      (let ((hl-line-fringe-indicator-bitmap-inactive 'vertical-bar))
        (hl-line-fringe-mode)
        (run-hooks 'post-command-hook)
        (switch-to-buffer buf-1)
        (run-hooks 'post-command-hook)
        (switch-to-buffer buf)
        (run-hooks 'post-command-hook)
        (expect
         (get-text-property
          0 'display
          (overlay-get
           hl-line-fringe--indicator-overlay
           'before-string))
         :to-equal
         `(left-fringe
           ,hl-line-fringe-indicator-bitmap
           hl-line-fringe-indicator))))))
