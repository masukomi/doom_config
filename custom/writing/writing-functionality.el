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

(defun writing/add-date ()
  "Set the DATE and DAY-OF-WEEK properties on the current heading.
Defaults the calendar picker to the nearest preceding DATE property if found."
  (interactive)
  (let* ((prev-date-string
          (save-excursion
            (when (re-search-backward "^[ \t]*:DATE:[ \t]*\\(.+\\)$" nil t)
              (string-trim (match-string 1)))))
         (org-overriding-default-time
          (when prev-date-string
            (org-time-string-to-time prev-date-string)))
         (date   (org-read-date))
         (parsed (org-parse-time-string date))
         (dow    (aref calendar-day-name-array
                       (calendar-day-of-week
                        (list (nth 4 parsed)
                              (nth 3 parsed)
                              (nth 5 parsed))))))
    (org-entry-put nil "DATE" date)
    (org-entry-put nil "DAY-OF-WEEK" dow)))

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
  (string= "t" (cadr (assoc "WRITING_STYLIZATION"
                             (org-collect-keywords '("WRITING_STYLIZATION"))))))

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

; TOC STUFF
; Usage:
;   From the story file, invoke C-c w t to generate / refresh the toc file.
;   The toc file is named like the story file but with _toc before the extension.
;   e.g. ji_woos_story.org -> ji_woos_story_toc.org
;
;   To auto-regenerate the toc on every save, add to the story file:
;   #+WRITING_GENERATE_TOC_ON_SAVE: t
;
;   In the toc file, links look like: [[toc:#custom-id][Heading Text]]
;   Following a link jumps to that heading in whichever window shows the story file,
;   opening it in the next window first if needed.

(defun writing/toc-filename (story-path)
  "Derive the toc file path from STORY-PATH, appending _toc before the extension."
  (let ((dir  (file-name-directory story-path))
        (base (file-name-base story-path))
        (ext  (file-name-extension story-path)))
    (expand-file-name (format "%s_toc.%s" base ext) dir)))

(defun writing/buffer-title ()
  "Return the #+TITLE value from the current buffer, or the file base name."
  (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
      (file-name-base (buffer-file-name))))

(defun writing/make-unique-id (base existing-ids)
  "Return BASE or BASE-2, BASE-3, ... — whichever is not in EXISTING-IDS."
  (let ((id base) (n 2))
    (while (member id existing-ids)
      (setq id (format "%s-%d" base n))
      (setq n (1+ n)))
    id))

(defun writing/heading-drawers-text (bound)
  "Return text of all drawers directly under the current heading, or nil.
Narrows to the content between the heading line and the first sub-heading
(or BOUND if none), so the parsed region contains no headlines and
org-element-map finds all drawers without needing no-recursion logic."
  (save-restriction
    (save-excursion
      (forward-line 1)
      (let ((content-end (or (save-excursion
                               (when (re-search-forward "^\\*+ " bound t)
                                 (match-beginning 0)))
                             bound)))
        (narrow-to-region (point) content-end)
        (let ((parts (org-element-map (org-element-parse-buffer)
                         '(drawer property-drawer)
                       (lambda (d)
                         (buffer-substring-no-properties
                          (org-element-property :begin d)
                          (org-element-property :end d))))))
          (when parts
            (apply #'concat parts)))))))

(defun writing/ensure-custom-ids-and-collect ()
  "Ensure every heading in the current buffer has a CUSTOM_ID.
Adds one derived from the heading text where absent.
Returns a list of (level heading-text custom-id drawers) for all headings,
where drawers is the text of any non-PROPERTIES drawers, or nil."
  (let (seen-ids entries)
    (org-map-entries
     (lambda ()
       (let* ((level   (org-current-level))
              (text    (org-get-heading t t t t))
              (id      (org-entry-get nil "CUSTOM_ID"))
              (bound   (save-excursion (outline-next-heading) (point)))
              (drawers (writing/heading-drawers-text bound)))
         (unless id
           (setq id (writing/make-unique-id
                     (writing/make-entity-id text) seen-ids))
           (org-entry-put nil "CUSTOM_ID" id))
         (push id seen-ids)
         (push (list level text id drawers) entries)))
     nil 'file)
    (nreverse entries)))

(defun writing/generate-toc ()
  "Generate or refresh the TOC file for the current story file.
Adds CUSTOM_ID properties to any headings that lack them, then writes
a toc file (replacing it if it exists) containing a heading-mirrored
list of toc: links.  The toc filename is the story filename with _toc
appended before the extension."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer has no file"))
  (let* ((story-path (buffer-file-name))
         (toc-path   (writing/toc-filename story-path))
         (story-name (file-name-nondirectory story-path))
         (title      (writing/buffer-title))
         (headings   (writing/ensure-custom-ids-and-collect)))
    (with-temp-file toc-path
      (insert (format "#+TITLE: TOC: %s\n" title))
      (insert (format "#+TOC_TARGET: %s\n" story-name))
      (insert "#+STARTUP: fold\n\n")
      (dolist (h headings)
        (insert (format "%s [[toc:#%s][%s]]\n"
                        (make-string (nth 0 h) ?*)
                        (nth 2 h)
                        (nth 1 h)))
        (when (nth 3 h)
          (insert (nth 3 h)))))
    (message "TOC written to %s" (file-name-nondirectory toc-path))))

(defun writing/maybe-generate-toc ()
  "Regenerate the TOC on save when #+WRITING_GENERATE_TOC_ON_SAVE: t is set."
  (when (string= "t" (cadr (assoc "WRITING_GENERATE_TOC_ON_SAVE"
                                  (org-collect-keywords '("WRITING_GENERATE_TOC_ON_SAVE")))))
    (writing/generate-toc)))

(defun writing/toc-target-file ()
  "Return the expanded path of #+TOC_TARGET in the current buffer, or nil."
  (when-let ((target (cadr (assoc "TOC_TARGET"
                                  (org-collect-keywords '("TOC_TARGET"))))))
    (expand-file-name target (file-name-directory (buffer-file-name)))))

(defun writing/toc-follow-link (search-term)
  "Follow a toc: link by jumping to SEARCH-TERM in the TOC_TARGET file.
Uses the window already showing the target if one exists; otherwise
displays it in the next window, splitting if only one window is open."
  (let ((target-path (writing/toc-target-file)))
    (unless target-path
      (user-error "No #+TOC_TARGET: keyword in this buffer"))
    (let* ((target-buf (or (get-file-buffer target-path)
                           (find-file-noselect target-path)))
           (target-win (or (get-buffer-window target-buf)
                           (if (one-window-p)
                               (split-window-right)
                             (next-window)))))
      (select-window target-win)
      (switch-to-buffer target-buf)
      (org-link-search search-term))))

(with-eval-after-load 'org
  (org-link-set-parameters "toc"
    :follow    #'writing/toc-follow-link
    :help-echo "Jump to this heading in the target document"))

(defun writing/org-mode-setup ()
  (when (string-match-p "_toc\\.org\\'" (or (buffer-file-name) ""))
    (nlinum-mode -1)
    (display-line-numbers-mode -1))
  (when (writing/stylization-enabled-p)
    (writing/enable-dialogue-highlighting))
  (add-hook 'after-save-hook #'writing/maybe-generate-toc nil t)
  (local-set-key (kbd "C-c w c") #'writing/add-character)
  (local-set-key (kbd "C-c w d") #'writing/add-date)
  (local-set-key (kbd "C-c w l") #'writing/add-location)
  (local-set-key (kbd "C-c w n") #'writing/add-notes)
  (local-set-key (kbd "C-c w p") #'writing/set-point-of-view)
  (local-set-key (kbd "C-c w s") #'writing/toggle-stylization)
  (local-set-key (kbd "C-c w t") #'writing/generate-toc))

(add-hook 'org-mode-hook #'writing/org-mode-setup)

(which-key-add-major-mode-key-based-replacements 'org-mode
  "C-c w"   "writing"
  "C-c w c" "add character"
  "C-c w d" "add date"
  "C-c w l" "add location"
  "C-c w n" "add notes"
  "C-c w p" "set point-of-view"
  "C-c w s" "toggle stylization"
  "C-c w t" "generate toc")
