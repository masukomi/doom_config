;; -*- lexical-binding: t; -*-
;;; tts.el --- macOS Text-to-Speech Integration for Emacs -*- lexical-binding: t; -*-

(defvar macos--say-process nil
  "Current macOS 'say' process.")
(defvar macos--continuous-mode nil
  "Whether continuous reading is active.")
(defvar macos--highlight-overlay nil
  "Overlay used to highlight the current spoken text.")

(defun macos--clear-highlight ()
  "Remove any existing highlight overlay."
  (when (overlayp macos--highlight-overlay)
    (delete-overlay macos--highlight-overlay)
    (setq macos--highlight-overlay nil)))

(defun macos--highlight-region (beg end)
  "Highlight the region from BEG to END and recenter the view."
  (macos--clear-highlight)
  (setq macos--highlight-overlay (make-overlay beg end))
  (overlay-put macos--highlight-overlay 'face '(:background "lemonchiffon2"))
  (goto-char beg)
  (recenter))

(defun macos--say-stop ()
  "Stop any ongoing macOS speech and clear highlight."
  (interactive)
  (setq macos--continuous-mode nil)
  (when (process-live-p macos--say-process)
    (delete-process macos--say-process))
  (ignore-errors (call-process "killall" nil nil nil "SpeechSynthesisServer"))
  (macos--clear-highlight)
  (message "🔇 Speech stopped."))

(defun macos--next-chunk-range ()
  "Return (BEG . END) of current paragraph or region."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (save-excursion
      (let (beg end)
        (backward-paragraph)
        (setq beg (point))
        (forward-paragraph)
        (setq end (point))
        (cons beg end)))))

(defun macos--advance-point (end)
  "Move point just after END to the start of the next readable section."
  (goto-char end)
  (skip-syntax-forward " >")
  (unless (eobp) (forward-word))
  (when (eobp)
    (message "✅ Reached end of buffer.")
    (setq macos--continuous-mode nil)))

(defun macos--say (text callback)
  "Speak TEXT asynchronously, then call CALLBACK when finished."
  ;; Stop any running speech first
  (when (process-live-p macos--say-process)
    (delete-process macos--say-process))
  (setq macos--say-process
        (start-process "macos-say" nil "say" text))
  (let ((cb callback))
    (set-process-sentinel
     macos--say-process
     (lambda (proc event)
       (when (and (memq (process-status proc) '(exit signal))
                  (string-match-p "finished" event))
         (setq macos--say-process nil)
         (when (functionp cb)
           (funcall cb)))))))

(defun macos-say-region-or-paragraph-and-advance (&optional continuous)
  "Speak the current paragraph or region, highlight it, and optionally continue."
  (interactive "P")
  (setq macos--continuous-mode continuous)
  (let* ((range (macos--next-chunk-range))
         (beg (car range))
         (end (cdr range))
         (text (string-trim
                (buffer-substring-no-properties beg end))))
    (if (string-empty-p text)
        (message "No text to speak.")
      (macos--highlight-region beg end)
      (macos--say text
                  (lambda ()
                    ;; Once speech ends
                    (macos--clear-highlight)
                    (macos--advance-point end)
                    (when macos--continuous-mode
                      (run-at-time
                       0.1 nil
                       (lambda ()
                         (when macos--continuous-mode
                           (macos-say-region-or-paragraph-and-advance t))))))))))

(global-set-key (kbd "C-c s") #'macos-say-region-or-paragraph-and-advance)
(global-set-key (kbd "C-c S")
                (lambda () (interactive)
                  (macos-say-region-or-paragraph-and-advance t)))
(global-set-key (kbd "C-c x") #'macos--say-stop)

(provide 'tts)
;;; tts.el ends here
