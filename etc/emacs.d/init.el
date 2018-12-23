;; -*- lexical-binding: t -*-

;; See the documentation of the 'my-main' function.

;; See Emacs manual "Saving Customizations"
;; Don't save the customization in emacs init file.
;; The custom file may have sensitive data such as IRC password.
;; https://stackoverflow.com/questions/26312317/wrong-indentation-of-comments-in-emacs
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
  ;; https://melpa.org/#/getting-started
  (append-to-list 'package-archives '(
    ("melpa" . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ))
  (package-initialize)

  ;; use-package, for lazy loading to speed up emacs start-up when using many packages
  (package-install 'use-package)
  (require 'use-package)

  (load custom-file t) ; optional; custom-file is set somewhere else in this file
  ;; Editor settings
  ;; Dealing with whitespaces.
  (setq-default indent-tabs-mode nil) ; use spaces instead of tabs
  (setq-default tab-width 4)
  (setq-default show-trailing-whitespace t)
  ;; Show matching parentheses
  (show-paren-mode 1) ; highlight matching parentheses
  (setq-default show-paren-delay 0) ; immediately
  ;; Emacs manual "Saving Emacs Sessions"
  (desktop-save-mode 1)
  ;; Strip trailing whitespaces on save
  (add-hook 'before-save-hook #'delete-trailing-whitespace t)
  ;; Other customizations
  (use-package magit :ensure t :pin melpa-stable)
  (my-org-customizations)
  )

;;;; Configurations specific to my workflow.

;; Emacs 24.3.1 Org Mode 8.2.4
;; Emacs 26.1 Org Mode 9.1.9

(defun my-org-customizations ()
  (use-package org
    :config (progn
      ;; Remove day name from inserted dates.
      ;; This affects 'C-c .'.
      ;; This also affects the dates in the exported HTML.
      ;; https://emacs.stackexchange.com/questions/27952/make-org-mode-not-to-insert-day-name-in-timestamps
      ;; Unfortunately org-time-stamp-formats is defconst.
      (setq org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
      ))
  (require 'ox)
  (require 'ox-html)
  (use-package org-ref
    :ensure t
    :config (let
                ((bibfiles '("~/work/edom.github.io/bib.bib")))
              (setq reftex-default-bibliography bibfiles
                    org-ref-default-bibliography bibfiles
                    bibtex-completion-bibliography bibfiles
                    )
              )
    )
  ;; https://emacs.stackexchange.com/questions/41220/org-mode-disable-indentation-when-promoting-and-demoting-trees-subtrees
  (setq-default org-adapt-indentation nil)
  ;; agenda
  (setq-default org-agenda-files '("~/work/edom.github.io/"))
  ;; publishing
  (setq org-publish-project-alist
      '(
        ("org-edom"
         :base-directory "~/work/edom.github.io/"
         :base-extension "org"
         :publishing-directory "~/work/edom.github.io/"
         :recursive t
         :body-only t
         :publishing-function org-myhtml-publish-to-myhtml
         )
        )
      )

  ;; Custom org mode backend for prepending YAML front matter to the output HTML document/fragment.
  ;; This is because I have old contents (almost 200 markdown files) in Jekyll, as of 2018-09-10.

  ;; https://orgmode.org/worg/dev/org-export-reference.html

  (org-export-define-derived-backend 'myhtml 'html
    :translate-alist '(
      (template . myhtml-template)
      (inner-template . myhtml-inner-template)
      )
    ;; See the documentation of the variable 'org-export-options-alist'.
    :options-alist '(
      (:permalink "PERMALINK" "permalink" nil t)
      (:mathjax "MATHJAX" "mathjax" nil t) ;; whether the document has math markup
      )
    )

  (add-hook 'org-mode-hook #'my-org-key-bindings t)

  )

(defun org-get-html-extension (plist)
  (or (plist-get plist :html-extension) org-html-extension "html"))

(defun org-myhtml-publish-to-myhtml (plist filename pub-dir)
  "See the function 'org-html-publish-to-html' in 'ox-html.el'."
  (org-publish-org-to 'myhtml filename (concat "." (org-get-html-extension plist)) plist pub-dir)
  )

;; 'contents' is transcoded string.
;; 'info' is plist of export options.

(defun myhtml-template (contents info)
  (concat (my-front-matter info) (org-html-template contents info)))

(defun myhtml-inner-template (contents info)
  (concat (my-front-matter info) (org-html-inner-template contents info)))

;; TODO escape YAML string
(defun my-front-matter (info) "
Generate YAML front matter from org export data plist.

Convert in-buffer options to YAML front matter.
For example, #+TITLE should map to title.

Complication: #+TITLE can have markup, but YAML front matter expects title to be string.
"
  (lexical-let* (
         (get (lambda (key) (org-export-data (plist-get info key) info)))
         ;; XXX This is a hack; base-dir should come from publishing config.
         ;;
         ;; The proper solution is something like (file-relative-name input-path base-dir).
         ;;
         ;; How does org HTML exporter translate a link to that.org into a link to that.html?
         ;; If this.org has a link to that.org, then this.html has a link to that.html.
         ;; When exporting, org replaces every link to that.org with a link to that.html.
         ;; How does org do that?
         ;;
         ;; See also https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Names.html#File-Names
         (base-dir (expand-file-name "~/work/edom.github.io/"))
         (title (funcall get ':title))
         (date (org-export-data (funcall get ':date) info))
         (input-path (funcall get ':input-file))
         (permalink (funcall get ':permalink))
         (mathjax (not (equal "" (funcall get ':mathjax))))
         )
    (concat
     "---\n"
     "title: " title "\n"
     "date: " date "\n"
     "permalink: " permalink "\n"
     (if mathjax "mathjax: yes\n" "")
     "---\n"
     )
    )
  )

(defun my-org-publish () "
Publish a website?
"
  (interactive)
  ;; Should we force?
  ;; Do we risk any staleness?
  (lexical-let* (
        (force nil)
        )
    (save-mark-and-excursion
      (org-publish "org-edom" force)
      )
    )
  )

(defun my-org-publish-current-file ()
  "See 'org-publish-current-file'.
Use this instead of 'C-c C-e P f'."
  (interactive)
  (save-mark-and-excursion
    (org-publish-current-file t)
    )
  )


(defun my-org-publish-current-file-with-pandoc ()
  (interactive)
  (let* ((absolute-path (buffer-file-name (current-buffer)))
         (relative-path (file-relative-name absolute-path "/home/erik/work/edom.github.io")))
    (call-process "/home/erik/work/sh/pandoc-to-jekyll-html"
                  nil
                  "*pandoc*"
                  nil
                  relative-path)
    (message "converted %s (%s)" relative-path absolute-path)
    ))

;;;; Custom Org Mode key bindings

;; TODO make these keybindings buffer-local according to the mode
(defun my-org-key-bindings ()
  (global-set-key (kbd "C-x g") 'magit) ; 2018-12-22 20:52 +0700 suddenly the shortcut has to be explicitly bound
  (global-set-key (kbd "<f2>") 'save-buffer)
  ;; We have replaced Org Exporter with Pandoc and Makefile.
  ;;(global-set-key (kbd "<f5>") 'my-org-publish)
  ;;(global-set-key (kbd "<f6>") 'my-org-publish-current-file)
  (global-set-key (kbd "<f5>") 'goto-last-change) ; akin to vim C-o
  (global-set-key (kbd "<f6>") 'goto-last-change-reverse) ; akin to vim C-i
  (global-set-key (kbd "<f7>") 'my-org-publish-current-file-with-pandoc)
  (global-set-key (kbd "<f8>") 'switch-to-buffer) ; my left pinky is tired of pressing C-x b
  )

;;;; End of customizations

;;;; 2018-09-11 Spacemacs

(defun my-run-spacemacs () "
See https://github.com/syl20bnr/spacemacs#alternate-installations
"
  (progn
    (setq spacemacs-start-directory "~/.emacs.d/spacemacs/") ; replace this with whither you git-cloned spacemacs
    (load-file (concat spacemacs-start-directory "init.el"))
    )
  )

(my-main)
