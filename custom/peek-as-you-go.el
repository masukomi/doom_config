;;; ../../workspace/doom_config/custom/peek-as-you-go.el -*- lexical-binding: t; -*-

;; Import dependency to check the highlight at point
(autoload 'hi-lock--regexps-at-point "hi-lock" nil t)
; found here
; https://www.reddit.com/r/emacs/comments/1qaqu7t/peek_as_you_go/
; by /u/OutOfCharm
; https://www.reddit.com/user/OutOfCharm/
;;;###autoload
(defun masu/space-command ()
  "Set mark on single space, highlight symbol on double space."
  (interactive)
  (require 'hi-lock) ; only want to require if we need it
  (set-mark-command nil)  ; Set mark first
  (let ((key (read-key "Mark set. Press SPC again to highlight symbol.")))
    (if (eq key ?\s)
        (progn
          (deactivate-mark) ; Cancel the mark if the next read is also SPC
	  (if (hi-lock--regexps-at-point)
	      (unhighlight-regexp t)	; Remove the current highlight of symbol at point
	    (progn
	      (unhighlight-regexp t)    ; Remove all previous highlights
	      (message "Highlighting symbol ...")
	      (highlight-symbol-at-point)))
	  )
      ;; If another key was pressed, execute that key
      (setq unread-command-events (list key)))))

(global-set-key (kbd "C-SPC") 'masu/space-command)
