;;; note.el -*- lexical-binding: t; -*-
(after! org
  (setq org-confirm-babel-evaluate t)
  (setq org-babel-no-eval-on-ctrl-c-ctrl-c t)
  (setq org-babel-default-header-args
        (cons '(:eval . "never")
              (assq-delete-all :eval org-babel-default-header-args)))

  (defun my/org-attach-clipboard ()
  "Attach image, file path, or URL from clipboard in Org-mode."
  (interactive)
  (require 'org-download)
  (require 'org-attach)
  (let* ((clipboard-content (gui-selection-value))
         (file-path (when (and clipboard-content (file-exists-p clipboard-content))
                      clipboard-content)))
    (cond
     ;; Handle file attachment (like drag-and-drop)
     (file-path
      (let ((file-name (file-name-nondirectory file-path)))
        (org-attach-attach file-path nil 'cp)
        ;; Insert attachment link
        (insert (format "[[attachment:%s]]" file-name))))
     ;; Handle URL
     ((and clipboard-content (string-match-p "^https?://" clipboard-content))
      (+org/attach-file-and-insert-link clipboard-content))
     ;; Default: handle clipboard image (e.g., screenshot)
     (t
      (org-download-clipboard)))))

  ;; Keybindings
  (map! :map org-mode-map
        :i "C-s-v" #'my/org-attach-clipboard
        :localleader
        (:prefix "a"
                 "p" #'my/org-attach-clipboard)))

(after! org-attach
  (setq org-attach-id-dir "./.attach/")
  (setq org-attach-use-inheritance t)
  (setq org-attach-dir-relative t))

(after! (deft org-roam)
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
                "\\)"))
  (setq deft-directory org-roam-directory)

  (defun my/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
          (string-trim
           (substring contents begin (match-end 0))
           "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
        (deft-base-filename file))))

  (advice-add 'deft-parse-title :override #'my/deft-parse-title))

(after! org-journal
  (setq org-journal-date-prefix "#+TITLE: ")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%A, %d %B %Y")
  (setq org-journal-enable-agenda-integration t))

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t))))

(after! org-noter
  ;; Enable auto-saving of last location to reduce prompts
  (setq org-noter-auto-save-last-location t)
  ;; Use default title rather than asking for user input
  (setq org-noter-insert-note-no-questions t)

  ;; Function to derive notes filename from PDF
  (defun my/org-noter-default-notes-file (document-path)
    "Return the notes filename based on the PDF's name."
    (let ((pdf-name (file-name-nondirectory document-path)))
      (concat (file-name-sans-extension pdf-name) ".org")))

  ;; Function to set search path to PDF's directory
  (defun my/org-noter-set-notes-path (document-path)
    "Return the directory of the PDF as the notes search path."
    (list (file-name-directory document-path)))

  ;; Hook to dynamically set variables when org-noter starts
  (defun my/org-noter-setup ()
    "Set notes filename and search path dynamically."
    (when (and org-noter--doc-file org-noter--session)
      (setq-local org-noter-default-notes-file-names
                  (my/org-noter-default-notes-file org-noter--doc-file))
      (setq-local org-noter-notes-search-path
                  (my/org-noter-set-notes-path org-noter--doc-file))))

  ;; Add hook to run setup when org-noter starts
  (add-hook 'org-noter--start-hook #'my/org-noter-setup))

(after! pdf-tools
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (map! :map pdf-view-mode-map
                    :n "C-u" #'pdf-view-scroll-down-or-previous-page
                    :n "C-d" #'pdf-view-scroll-up-or-next-page
                    :n "C-f" #'pdf-view-next-page-command
                    :n "C-b" #'pdf-view-previous-page-command
                    :n "c" #'pdf-annot-add-highlight-markup-annotation))))

(after! (pdf-tools org-noter)
  (setq org-noter-highlight-selected-text t)

  (defun my/org-noter-kill-session (&optional session)
    (interactive)
    (if (and org-noter--session (not session))
        (setq session org-noter--session))
    (when session
      (let* ((doc-buffer (org-noter--session-doc-buffer session)))
        (save-buffer doc-buffer)))
    (org-noter-kill-session session))

  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (map! :map pdf-view-mode-map
                    :n "i" #'org-noter-insert-note
                    :n "q" #'my/org-noter-kill-session))))

;; lift frequent keymap for notes
(map! :leader
      (:when (modulep! :ui deft)
        :desc "Open deft"                   "d" #'deft)
      (:when (modulep! :lang org +noter)
        :desc "Org noter"                   "e" #'org-noter)

      ;; should sync with the following page
      ;; https://github.com/doomemacs/doomemacs/blob/master/modules/config/default/%2Bevil-bindings.el
      (:when (modulep! :lang org +journal)
        (:prefix ("j" . "journal")
         :desc "New Entry"           "j" #'org-journal-new-entry
         :desc "New Scheduled Entry" "J" #'org-journal-new-scheduled-entry
         :desc "Search Forever"      "s" #'org-journal-search-forever))

      ;; should sync with the following page
      ;; https://github.com/doomemacs/doomemacs/blob/master/modules/config/default/%2Bevil-bindings.el
      (:when (modulep! :lang org +roam2)
        (:prefix ("r" . "roam")
         :desc "Open random node"           "a" #'org-roam-node-random
         :desc "Find node"                  "f" #'org-roam-node-find
         :desc "Find ref"                   "F" #'org-roam-ref-find
         :desc "Show graph"                 "g" #'org-roam-graph
         :desc "Insert node"                "i" #'org-roam-node-insert
         :desc "Capture to node"            "n" #'org-roam-capture
         :desc "Toggle roam buffer"         "r" #'org-roam-buffer-toggle
         :desc "Launch roam buffer"         "R" #'org-roam-buffer-display-dedicated
         :desc "Sync database"              "s" #'org-roam-db-sync
         (:prefix ("d" . "by date")
          :desc "Goto previous note"        "b" #'org-roam-dailies-goto-previous-note
          :desc "Goto date"                 "d" #'org-roam-dailies-goto-date
          :desc "Capture date"              "D" #'org-roam-dailies-capture-date
          :desc "Goto next note"            "f" #'org-roam-dailies-goto-next-note
          :desc "Goto tomorrow"             "m" #'org-roam-dailies-goto-tomorrow
          :desc "Capture tomorrow"          "M" #'org-roam-dailies-capture-tomorrow
          :desc "Capture today"             "n" #'org-roam-dailies-capture-today
          :desc "Goto today"                "t" #'org-roam-dailies-goto-today
          :desc "Capture today"             "T" #'org-roam-dailies-capture-today
          :desc "Goto yesterday"            "y" #'org-roam-dailies-goto-yesterday
          :desc "Capture yesterday"         "Y" #'org-roam-dailies-capture-yesterday
          :desc "Find directory"            "-" #'org-roam-dailies-find-directory)))
      :desc "List roam" "l" #'org-roam-node-find)
