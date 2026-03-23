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

(defun writing/make-character-id (name)
  "Derive a unique character ID from NAME.
Strips leading/trailing non-word characters, lowercases, and replaces
runs of non-word characters with a single hyphen."
  (let* ((s (replace-regexp-in-string "\\`[^[:word:]]+" "" name))
         (s (replace-regexp-in-string "[^[:word:]]+\\'" "" s))
         (s (downcase s))
         (s (replace-regexp-in-string "[^[:word:]]+" "-" s)))
    s))

(defun writing/ensure-character-in-registry (display-name character-id)
  "Ensure a heading for CHARACTER-ID exists in characters.org.
Creates the file if absent. Adds a heading with DISPLAY-NAME and
a :character-id: property if no matching heading is found."
  (let* ((dir (file-name-directory (buffer-file-name)))
         (registry (expand-file-name "characters.org" dir)))
    (unless (file-exists-p registry)
      (with-temp-file registry
        (insert "#+TITLE: Characters\n\n")))
    (with-current-buffer (find-file-noselect registry)
      (let ((found (org-map-entries
                    (lambda ()
                      (string= (org-entry-get nil "character-id") character-id)))))
        (unless (seq-some #'identity found)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format "* %s\n  :PROPERTIES:\n  :character-id: %s\n  :END:\n"
                          display-name character-id))
          (save-buffer))))))

(defun writing/add-character ()
  "Add a character to the CHARACTERS property of the current org heading.
Prompts for a display name, derives a character-id from it, stores the
character-id in the property, and ensures an entry exists in characters.org."
  (interactive)
  (let ((name (string-trim (read-string "Character name: "))))
    (when (string-empty-p name)
      (user-error "Character name cannot be empty"))
    (let ((id (writing/make-character-id name)))
      (writing/add-to-property "CHARACTERS" id)
      (writing/ensure-character-in-registry name id))))

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
