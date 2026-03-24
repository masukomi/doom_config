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

(defun writing/make-entity-id (name)
  "Derive a unique entity ID from NAME.
Strips leading/trailing non-word characters, lowercases, and replaces
runs of non-word characters with a single hyphen."
  (let* ((s (replace-regexp-in-string "\\`[^[:word:]]+" "" name))
         (s (replace-regexp-in-string "[^[:word:]]+\\'" "" s))
         (s (downcase s))
         (s (replace-regexp-in-string "[^[:word:]]+" "-" s)))
    s))

(defun writing/ensure-in-registry (display-name entity-id registry-filename id-property title)
  "Ensure a heading for ENTITY-ID exists in REGISTRY-FILENAME in the current directory.
Creates the file with TITLE if absent. Uses ID-PROPERTY to match existing headings.
Adds a heading with DISPLAY-NAME and ID-PROPERTY set to ENTITY-ID if not found."
  (let* ((dir (file-name-directory (buffer-file-name)))
         (registry (expand-file-name registry-filename dir)))
    (unless (file-exists-p registry)
      (with-temp-file registry
        (insert (format "#+TITLE: %s\n\n" title))))
    (with-current-buffer (find-file-noselect registry)
      (let ((found (org-map-entries
                    (lambda ()
                      (string= (org-entry-get nil id-property) entity-id)))))
        (unless (seq-some #'identity found)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format "* %s\n  :PROPERTIES:\n  :%s: %s\n  :END:\n"
                          display-name id-property entity-id))
          (save-buffer))))))

(defun writing/known-ids-for-property (prop-name)
  "Return a deduplicated list of IDs stored under PROP-NAME and PROP-NAME+ in the buffer."
  (delete-dups
   (apply #'append
          (mapcar (lambda (v) (split-string v nil t))
                  (org-property-values prop-name)))))

(defun writing/read-entity-input (prompt prop-name)
  "Prompt with PROMPT, offering existing PROP-NAME values for completion.
Returns (ID . INPUT) where ID is the derived entity ID and INPUT is what the user typed."
  (let ((input (string-trim (completing-read prompt (writing/known-ids-for-property prop-name) nil nil))))
    (when (string-empty-p input)
      (user-error "Name cannot be empty"))
    (cons (writing/make-entity-id input) input)))

(defun writing/add-character ()
  "Add a character to the CHARACTERS property of the current org heading.
Presents existing character IDs from the buffer for completion. Accepts
either an existing ID or a new display name (which is converted to an ID).
Ensures an entry exists in characters.org."
  (interactive)
  (let* ((result (writing/read-entity-input "Character: " "CHARACTERS"))
         (id (car result))
         (input (cdr result)))
    (writing/add-to-property "CHARACTERS" id)
    (writing/ensure-in-registry input id "characters.org" "character-id" "Characters")))

(defun writing/add-location ()
  "Add a location to the LOCATIONS property of the current org heading.
Presents existing location IDs from the buffer for completion. Accepts
either an existing ID or a new display name (which is converted to an ID).
Ensures an entry exists in locations.org."
  (interactive)
  (let* ((result (writing/read-entity-input "Location: " "LOCATIONS"))
         (id (car result))
         (input (cdr result)))
    (writing/add-to-property "LOCATIONS" id)
    (writing/ensure-in-registry input id "locations.org" "location-id" "Locations")))

(defun writing/set-point-of-view ()
  "Set the POV property on the current org heading to a single character ID.
Replaces any existing POV value. Presents existing character IDs for
completion and ensures the character exists in characters.org."
  (interactive)
  (let* ((result (writing/read-entity-input "POV character: " "CHARACTERS"))
         (id (car result))
         (input (cdr result)))
    (org-entry-put nil "POV" id)
    (writing/ensure-in-registry input id "characters.org" "character-id" "Characters")))

(defun writing/org-mode-setup ()
  (local-set-key (kbd "C-c w c") #'writing/add-character)
  (local-set-key (kbd "C-c w l") #'writing/add-location)
  (local-set-key (kbd "C-c w p") #'writing/set-point-of-view))

(add-hook 'org-mode-hook #'writing/org-mode-setup)
