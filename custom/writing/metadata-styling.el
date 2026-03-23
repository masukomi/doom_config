;;; -*- lexical-binding: t; -*-

(defface masu/org-meta-heading
  '((t :foreground "#c0abfe" :weight bold :height 1.1))
  "Face for headings tagged :noexport:.")

(defface masu/org-meta-content
  '((t :foreground "#c0abfe"
       :background "#333333"
     ))
  "Face for content under :noexport: headings.")

(defun masu/org-match-special-subtree (limit)
  "Font-lock matcher for subtrees whose heading has the :noexport: tag."
  (let ((case-fold-search nil))
    (when (re-search-forward "^\\(\\*+\\).*:noexport:" limit t)
      (let* ((level (length (match-string 1)))
             (start (line-beginning-position))
             ;; Find end: next heading of same or higher level, or limit
             (end (save-excursion
                    (forward-line 1)
                    (if (re-search-forward
                         (concat "^\\*\\{1," (number-to-string level) "\\}[^*]")
                         limit t)
                        (line-beginning-position)
                      limit))))
        (set-match-data (list start end))
        t))))

(defun masu/org-extend-region ()
  "Extend font-lock region backward to include any enclosing :noexport: heading."
  (save-excursion
    (goto-char font-lock-beg)
    (when (re-search-backward "^\\*+.*:noexport:" nil t)
      (when (< (point) font-lock-beg)
        (setq font-lock-beg (point))
        t))))

(defun masu/org-add-special-keywords ()
  (font-lock-add-keywords
   nil
   `((masu/org-match-special-subtree 0 'masu/org-meta-content t)
     ("^\\*+.*:noexport:.*$" 0 'masu/org-meta-heading t))
   t)
  (add-to-list 'font-lock-extend-region-functions #'masu/org-extend-region))

(add-hook 'org-mode-hook #'masu/org-add-special-keywords)
