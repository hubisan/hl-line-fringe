;; ** Window-configuration-change-hook


;; Local

(defun my-config-hook-local ()
  (unless (minibufferp)
    (unless (window-minibuffer-p)
      (message "win local: %s %s %s" (selected-window) (window-buffer) (current-buffer)))))

(add-hook 'window-configuration-change-hook #'my-config-hook-local nil t)
(remove-hook 'window-configuration-change-hook #'my-config-hook-local t)

;; findings:
;; - impossible to check if minibuffer window
;; - not 100% when this is run

;; Global
(defun my-config-hook-global ()
  (unless (minibuffer-window-active-p (frame-selected-window))
    (message "win global: %s %s %s" (selected-window) (window-buffer) (current-buffer))))

(defun my-config-hook-global ()
  (unless (window-minibuffer-p)
    (message "win global: %s %s %s" (selected-window) (window-buffer) (current-buffer))))

(add-hook 'window-configuration-change-hook #'my-config-hook-global)
(remove-hook 'window-configuration-change-hook #'my-config-hook-global)

(setq test-ov (make-overlay (point) (point) (current-buffer)))
(overlay-put test-ov 'window (selected-window))
(overlay-put test-ov 'face 'hl-line)
(move-overlay test-ov (point-min) (point-max))

;; **

(defun my-post-command-global ()
  (unless (window-minibuffer-p)
    (message "pc global: %s %s %s" (selected-window) (window-buffer) (current-buffer))))


(add-hook 'post-command-hook #'my-post-command-global)
(remove-hook 'post-command-hook #'my-post-command-global)

(overlay-properties test-ov)
(window-live-p (overlay-get test-ov 'window))
