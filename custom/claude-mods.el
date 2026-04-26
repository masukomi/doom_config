;;; ../../workspace/doom_config/custom/claude-mods.el -*- lexical-binding: t; -*-

(defun masu/claude-code--org-region-in-markdown-block-p (beg)
  "Return t if BEG is inside a markdown src or export block in org-mode."
  (save-excursion
    (goto-char beg)
    (let* ((element (org-element-at-point))
           (type (org-element-type element)))
      (or (and (eq type 'src-block)
               (string= (org-element-property :language element) "markdown"))
          (and (eq type 'export-block)
               (string= (org-element-property :type element) "MARKDOWN"))))))

(defun masu/claude-code--org-to-markdown (text)
  "Convert org-mode TEXT string to a markdown string."
  (require 'ox-md)
  (string-trim
   (with-temp-buffer
     (insert text)
     (org-mode)
     (org-export-as 'md nil nil t))))

(defun masu/claude-code--build-region-cmd (beg end)
  "Build the command string for sending region BEG to END with a prompt.
In org-mode buffers, converts the region to markdown first unless the
region is already inside a markdown src or export block."
  (let* ((region-text (buffer-substring-no-properties beg end))
         (content (if (and (derived-mode-p 'org-mode)
                           (not (masu/claude-code--org-region-in-markdown-block-p beg)))
                      (masu/claude-code--org-to-markdown region-text)
                    region-text))
         (prompt (read-string "Prompt for Claude: " nil 'claude-code-command-history)))
    (format "```\n%s\n```\n\n%s" content prompt)))

(defun masu/claude-code--format-file-reference (&optional filename start-line end-line)
  "Format a file @-reference for Claude Code.
FILENAME defaults to the current buffer's file.  If START-LINE and
END-LINE are provided, appends the line range."
  (when-let ((file (or filename (buffer-file-name))))
    (if (and start-line end-line)
        (format "@%s:%d-%d" file start-line end-line)
      (format "@%s" file))))

(defun masu/claude-code-send-region (beg end)
  "Send region to Claude without a prompt."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region selected"))
  (let ((region-text (buffer-substring-no-properties beg end)))
    (claude-code-ide-send-prompt (format "```\n%s\n```" region-text))))

(defun masu/claude-code-send-region-with-prompt (beg end)
  "Send region to Claude wrapped in delimiters, then ask for a prompt."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region selected"))
  (claude-code-ide-send-prompt (masu/claude-code--build-region-cmd beg end)))


;---
(defun masu/claude-code-send-command-with-context-select-instance ()
  "Send a command with file context to a selected Claude instance."
  (interactive)
  (let* ((cmd (read-string "Claude command: " nil 'claude-code-command-history))
         (file-ref (if (use-region-p)
                       (masu/claude-code--format-file-reference
                        nil
                        (line-number-at-pos (region-beginning) t)
                        (line-number-at-pos (region-end) t))
                     (masu/claude-code--format-file-reference)))
         (cmd-with-context (if file-ref (format "%s\n%s" cmd file-ref) cmd)))
    (masu/claude-code--send-to-instance cmd-with-context)))


(defun masu/claude-code--send-to-instance (cmd)
  "Prompt for a Claude instance and send CMD to it."
  (claude-code-ide--cleanup-dead-processes)
  (let ((sessions '()))
    (maphash (lambda (directory _)
               (push (cons (abbreviate-file-name directory) directory) sessions))
             claude-code-ide--processes)
    (unless sessions
      (user-error "No active Claude Code sessions"))
    (let* ((choice (completing-read "Send to Claude instance: " sessions nil t))
           (directory (alist-get choice sessions nil nil #'string=))
           (buffer-name (funcall claude-code-ide-buffer-name-function directory))
           (target (get-buffer buffer-name)))
      (if target
          (with-current-buffer target
            (claude-code-ide--terminal-send-string cmd)
            (sit-for 0.1)
            (claude-code-ide--terminal-send-return)
            (display-buffer target))
        (user-error "No Claude instance found for selected session")))))

(defun masu/claude-code-send-region-select-instance (beg end)
  "Like `masu/claude-code-send-region-with-prompt' but prompt for Claude instance."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region selected"))
  (masu/claude-code--send-to-instance (masu/claude-code--build-region-cmd beg end)))
