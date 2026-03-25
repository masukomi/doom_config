;;; -*- lexical-binding: t; -*-

; cause NOTES drawers to never be exported
(defface writing/dialogue-face
  '((t :foreground "#9ab9e2"))
  "Face for quoted dialogue in prose.")

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
         (s (replace-regexp-in-string "['\"]" "-" s))
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

(defun writing/add-notes ()
  "Open the NOTES drawer of the current heading, creating it if absent.
If a PROPERTIES drawer exists, NOTES is inserted after it.
Always adds a new line inside the drawer and moves point there."
  (interactive)
  (org-back-to-heading t)
  (let* ((bound (save-excursion (outline-next-heading) (point)))
         (indent (save-excursion
                   (if (re-search-forward "^\\([ \t]*\\):PROPERTIES:" bound t)
                       (match-string 1)
                     ""))))
    (if (save-excursion
          (re-search-forward
           (concat "^" (regexp-quote indent) ":NOTES:") bound t))
        ;; NOTES drawer exists — add a line before its :END: and go there
        (progn
          (re-search-forward
           (concat "^" (regexp-quote indent) ":NOTES:") bound t)
          (unless (re-search-forward
                   (concat "^" (regexp-quote indent) ":END:") bound t)
            (user-error "Malformed NOTES drawer: missing :END:"))
          (beginning-of-line)
          (insert (concat indent "\n"))
          (forward-line -1))
      ;; NOTES drawer absent — insert after PROPERTIES :END: or after heading line
      (re-search-forward
       (concat "^" (regexp-quote indent) ":END:") bound t)
      (end-of-line)
      (insert (format "\n%s:NOTES:\n\n%s:END:" indent indent))
      (forward-line -1))))

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

; STYLIZATION STUFF
; To enable this functionality add the following to your document
;
; #+WRITING_STYLIZATION: t
;
; You can toggle this with the following key combo: C-c w s
(defun writing/dialogue-font-lock-matcher (limit)
  "Font-lock matcher for quoted dialogue.
Handles multi-paragraph speeches per English grammar convention:
intermediate paragraphs end without a closing quote when the next
paragraph begins with one.  Each such paragraph is a separate match.

Matches from an opening double-quote to either:
- a closing double-quote, or
- the newline before a blank-line/opening-quote continuation (\\n\\n\")."
  (catch 'found
    (while (search-forward "\"" limit t)
      (let ((start (match-beginning 0)))
        (unless (eolp) ; " at end of line is never an opening quote (e.g. 4'2")
          (when (re-search-forward "\"\\|\n\n\"" limit t)
            (let* ((mstart (match-beginning 0))
                   (mend   (match-end 0))
                   (continuation (> (- mend mstart) 1)))
              (if continuation
                  ;; Highlight up to (not including) the \n\n, then rewind so
                  ;; the " that opens the next paragraph is found next iteration.
                  (progn
                    (set-match-data (list start mstart))
                    (goto-char mstart))
                (set-match-data (list start mend)))
              (throw 'found t))))))
    nil))

(defconst writing/font-lock-dialogue-keywords
  '((writing/dialogue-font-lock-matcher (0 'writing/dialogue-face prepend)))
  "Font-lock keyword spec for dialogue highlighting.")

(defun writing/stylization-enabled-p ()
  "Return t if #+WRITING_STYLIZATION: t is present in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward "^#\\+WRITING_STYLIZATION:[ \t]*\\(.*\\)$" nil t)
         (string= (string-trim (match-string 1)) "t"))))

(defun writing/enable-dialogue-highlighting ()
  "Enable dialogue font-lock highlighting in the current buffer."
  (setq-local font-lock-multiline t)
  (font-lock-add-keywords nil writing/font-lock-dialogue-keywords 'append)
  (font-lock-flush))

(defun writing/disable-dialogue-highlighting ()
  "Disable dialogue font-lock highlighting in the current buffer."
  (font-lock-remove-keywords nil writing/font-lock-dialogue-keywords)
  (font-lock-flush))

(defun writing/set-stylization-keyword (value)
  "Set #+WRITING_STYLIZATION: to VALUE, inserting the keyword if absent."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+WRITING_STYLIZATION:.*$" nil t)
        (replace-match (concat "#+WRITING_STYLIZATION: " value))
      (goto-char (point-min))
      (while (looking-at-p "^#\\+")
        (forward-line 1))
      (insert (format "#+WRITING_STYLIZATION: %s\n" value)))))

(defun writing/toggle-stylization ()
  "Toggle #+WRITING_STYLIZATION: between t and nil, updating highlighting."
  (interactive)
  (if (writing/stylization-enabled-p)
      (progn
        (writing/set-stylization-keyword "nil")
        (writing/disable-dialogue-highlighting)
        (message "Writing stylization disabled"))
    (writing/set-stylization-keyword "t")
    (writing/enable-dialogue-highlighting)
    (message "Writing stylization enabled")))

(defun writing/org-mode-setup ()
  (when (writing/stylization-enabled-p)
    (writing/enable-dialogue-highlighting))
  (local-set-key (kbd "C-c w c") #'writing/add-character)
  (local-set-key (kbd "C-c w l") #'writing/add-location)
  (local-set-key (kbd "C-c w n") #'writing/add-notes)
  (local-set-key (kbd "C-c w p") #'writing/set-point-of-view)
  (local-set-key (kbd "C-c w s") #'writing/toggle-stylization))

(add-hook 'org-mode-hook #'writing/org-mode-setup)

(which-key-add-major-mode-key-based-replacements 'org-mode
  "C-c w"   "writing"
  "C-c w c" "add character"
  "C-c w l" "add location"
  "C-c w n" "add notes"
  "C-c w p" "set point-of-view"
  "C-c w s" "toggle stylization")
