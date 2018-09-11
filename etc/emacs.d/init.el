;; -*- lexical-binding: t -*-

; See the documentation of the 'my-main' function.

; See Emacs manual "Saving Customizations"
; Don't save the customization in emacs init file.
; The custom file may have sensitive data such as IRC password.
(setq custom-file "~/.emacs.d/custom.el")

(defun my-main () "
Run 'SPACEMACS=1 emacs' in bash to run spacemacs.
"
  (if (getenv "SPACEMACS") (my-run-spacemacs) (my-customizations))
  )

(defun append-to-list (dst src)
  (dolist (elm src) (add-to-list dst elm t))
  )

(defun my-customizations () "
Entry point.
Called at the end of init.el.
"
  (require 'package)
  ; https://melpa.org/#/getting-started
  (append-to-list 'package-archives '(
    ("melpa" . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ))
  (package-initialize)

  ; use-package, for lazy loading to speed up emacs start-up when using many packages
  (package-install 'use-package)
  (require 'use-package)

  (load custom-file t) ; optional; custom-file is set somewhere else in this file
  ; Editor settings
  ; Dealing with whitespaces.
  (setq-default indent-tabs-mode nil) ; use spaces instead of tabs
  (setq-default tab-width 4)
  (setq-default show-trailing-whitespace t)
  ; Show matching parentheses
  (show-paren-mode 1) ; highlight matching parentheses
  (setq-default show-paren-delay 0) ; immediately
  ; Emacs manual "Saving Emacs Sessions"
  (desktop-save-mode 1)
  ; Strip trailing whitespaces on save
  (add-hook 'before-save-hook #'delete-trailing-whitespace t)
  ; Other customizations
  (use-package magit :ensure t :pin melpa-stable)
  (my-org-customizations)
  )

;;;;;;;; Configurations specific to my workflow.

;;;;;;;; Emacs 24.3.1 Org Mode 8.2.4
;;;;;;;; Emacs 26.1 Org Mode 9.1.9

(defun my-org-customizations ()
  (require 'ox)
  (require 'ox-html)
  (use-package org-ref
    :ensure t
    :config (let
                ((bibfiles '("~/work/org/bib.bib")))
              (setq reftex-default-bibliography bibfiles
                    org-ref-default-bibliography bibfiles
                    bibtex-completion-bibliography bibfiles
                    )
              )
    )
  ; https://emacs.stackexchange.com/questions/41220/org-mode-disable-indentation-when-promoting-and-demoting-trees-subtrees
  (setq-default org-adapt-indentation nil)
  (setq-default org-agenda-files '("~/work/org"))
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
        )
      )

  ;;;; Custom org mode backend for prepending YAML front matter to the output HTML document/fragment.
  ;;;; This is because I have old contents (almost 200 markdown files) in Jekyll, as of 2018-09-10.

  ; https://orgmode.org/worg/dev/org-export-reference.html

  (org-export-define-derived-backend 'myhtml 'html
    :translate-alist '(
      (template . myhtml-template)
      (inner-template . myhtml-inner-template)
      )
    )

  (add-hook 'org-mode-hook #'my-org-key-bindings t)

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

;;;;;;;; End

;;;;;;;; 2018-09-11 Spacemacs

(defun my-run-spacemacs () "
See https://github.com/syl20bnr/spacemacs#alternate-installations
"
  (progn
    (setq spacemacs-start-directory "~/.emacs.d/spacemacs/") ; replace this with whither you git-cloned spacemacs
    (load-file (concat spacemacs-start-directory "init.el"))
    )
  )

(my-main)
