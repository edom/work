(require 'package)

(package-initialize)

;; Load customizations.

(defun load-optional (path)
  "load but ignore non-existent files"
  (load path t))

; Emacs manual "Saving Customizations"
(setq custom-file "~/.emacs.d/custom.el")
(load-optional custom-file)

; Editor settings
(setq indent-tabs-mode nil) ; use spaces instead of tabs
(setq show-trailing-whitespace t)
(show-paren-mode 1) ; highlight matching parentheses
(setq show-paren-delay 0) ; immediately

; Emacs manual "Saving Emacs Sessions"
(desktop-save-mode 1)

;;;;;;;; Configurations specific to my workflow.

;;;;;;;; Emacs 24.3.1 Org Mode 8.2.4

(require 'ox)
(require 'ox-html)

(setq org-publish-project-alist
      '(
        ("org-edom"
         :base-directory "/home/erik/work/org/"
         :base-extension "org"
         :publishing-directory "/tmp/orgtest/"
         :recursive t
         :body-only t
         ; TODO org-myhtml-publish-to-myhtml
         :publishing-function org-html-publish-to-html
         )
        ))

;;;; Custom org mode backend for prepending YAML front matter to the output HTML document/fragment.
;;;; This is because I have old contents (almost 200 markdown files) in Jekyll, as of 2018-09-10.

; https://orgmode.org/worg/dev/org-export-reference.html

(org-export-define-derived-backend 'myhtml 'html
  :translate-alist '(
    (template . myhtml-template)
    (inner-template . myhtml-inner-template)
    )
  )

(defun myhtml-template (contents info)
  (concat (my-front-matter info) (org-html-template contents info)))

(defun myhtml-inner-template (contents info)
  (concat (my-front-matter info) (org-html-inner template contents info)))

; TODO escape YAML string
(defun my-front-matter (info) "
Generate YAML front matter from org export data plist.

Convert in-buffer options to YAML front matter.
For example, #+TITLE should map to title.

Complication: #+TITLE can have markup, but YAML front matter expects title to be string.
"
  (let* (
         (get (lambda (key) (org-export-data (plist-get info key) info)))
         (title (funcall get ':title))
         (date (funcall get ':date))
         (input-path (funcall get ':input-file))
         (permalink (funcall get ':permalink))
         )
    (concat
     "---\n"
     "title: " title "\n"
     "date: " date "\n"
     "permalink: " permalink "\n"
     "---\n"
     )
    )
  )

; This is for testing purposes only.
(defun my-org-export-for-jekyll () "
Export current org-mode buffer as HTML for edom.github.io.
This assumes Ubuntu 14.04, Emacs 24.3.1, and Org Mode 8.2.4.
"
  (interactive)
  (let
      (
       (async nil)
       (subtree-only nil)
       (visible-only nil)
       (body-only t)
       (ext-plist nil)
       (org-buffer-name "*Org HTML Export*")
       )
    (progn
      (org-export-to-buffer 'myhtml org-buffer-name
        async subtree-only visible-only body-only ext-plist)
      )))

(defun my-org-publish () "
Publish a website?
"
  (interactive)
  (let (
        (force t)
        )
    (org-publish "org-edom" force)
    )
  )

;;;; Custom Org Mode key bindings

; TODO make these keybindings buffer-local according to the mode
(defun my-org-key-bindings ()
  (global-set-key (kbd "s-e") 'my-org-export-for-jekyll)
  (global-set-key (kbd "s-p") 'my-org-publish)
  )

(add-hook 'org-mode-hook #'my-org-key-bindings t)
