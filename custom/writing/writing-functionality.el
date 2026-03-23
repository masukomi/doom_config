;;; -*- lexical-binding: t; -*-

; cause NOTES drawers to never be exported
(setq org-export-with-drawers '(not "NOTES"))

(defun writing/add-to-property (prop-name value)
  "Add VALUE to PROP-NAME (or PROP-NAME+) on the current org heading.
Uses PROP-NAME for the first entry and PROP-NAME+ for subsequent ones."
  (let* ((prop+ (concat prop-name "+"))
         (props (org-entry-properties nil 'standard))
         (entries (seq-filter (lambda (p)
                                (member (car p) (list prop-name prop+)))
                              props))
         (existing (mapcar #'cdr entries)))
    (if (member value existing)
        (message "'%s' is already listed under %s" value prop-name)
      (if (null entries)
          (org-entry-put nil prop-name value)
        (save-excursion
          (org-back-to-heading t)
          (let ((bound (save-excursion (outline-next-heading) (point))))
            (if (re-search-forward "^\\([ \t]*\\):END:" bound t)
                (progn
                  (beginning-of-line)
                  (insert (format "%s:%s: %s\n" (match-string 1) prop+ value)))
              (user-error "Could not find :END: in PROPERTIES drawer"))))))))

(defun writing/add-character ()
  "Add a character name to the CHARACTERS property of the current org heading."
  (interactive)
  (let ((name (string-trim (read-string "Character name: "))))
    (when (string-empty-p name)
      (user-error "Character name cannot be empty"))
    (writing/add-to-property "CHARACTERS" name)))

(defun writing/add-location ()
  "Add a location name to the LOCATIONS property of the current org heading."
  (interactive)
  (let ((name (string-trim (read-string "Location name: "))))
    (when (string-empty-p name)
      (user-error "Location name cannot be empty"))
    (writing/add-to-property "LOCATIONS" name)))

(defun writing/org-mode-setup ()
  (local-set-key (kbd "C-c w c") #'writing/add-character)
  (local-set-key (kbd "C-c w l") #'writing/add-location))

(add-hook 'org-mode-hook #'writing/org-mode-setup)
