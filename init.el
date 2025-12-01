;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex-latexmk color-theme-modern company dap-mode evil
					evil-cleverparens evil-easymotion evil-goggles
					evil-mc evil-mc-extras evil-org fennel-mode flymd
					go-mode latex-preview-pane light-soap-theme lsp-ui
					lua-mode magit odin odin-mode org-pomodoro
					ox-typst pdf-tools projectile realgud-lldb rustic
					typst-preview typst-ts-mode zig-mode zig-ts-mode))
 '(package-vc-selected-packages
   '((odin-mode :vc-backend Git :url
				"https://git.sr.ht/~mgmarlow/odin-mode"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ~~~~~~~~~~~~~~~~~~~~ Packages ~~~~~~~~~~~~~~~~~~~~~~~~
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialiaze package manager
(defun sync ()
  (package-initialize)
  (package-refresh-contents))

;; Use package module
(unless (package-installed-p 'use-package)
  (sync)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; Colorthemes
(use-package color-theme-modern)
(load-theme 'aalto-light t t)
(enable-theme 'aalto-light)

;; Config LSP
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . (lambda ()
			(unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
			  (lsp-deferred)))))
  :init
  (with-eval-after-load 'lsp-mode
    (setq lsp-keymap-prefix "C-c l"))
  :config
  (with-eval-after-load 'which-key
    (lsp-enable-which-key-integration t)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics))


(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-lldb)
  
  ;; Set the debug program path
  (setq dap-lldb-debug-program '("/usr/sbin/lldb-dap"))
  
  ;; Register your debug template
  (dap-register-debug-template
   "monk"
   (list :type "lldb-vscode"
         :request "launch"
         :name "monk"
         :program "${workspaceFolder}/build/monk"
         :args ["adder.exe"]
         :cwd "${workspaceFolder}"
		 :stopAtEntry t)))

;; UI enhancements
(use-package dap-ui
  :ensure nil
  :after dap-mode
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1))

;; Autocompletion
(use-package company
  :hook (after-init . global-company-mode)
  :custom (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package which-key
  :config (which-key-mode))

;; Config Projectile (It's like telescope)
(use-package projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Config Magit
(use-package magit
  :defer t)

;; Config Latex
(use-package auctex
  :defer t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(use-package reftex
  :defer t
  :hook (LaTeX-mode . turn-on-reftex))

(use-package pdf-tools
  :config
  (pdf-tools-install))

(use-package latex-preview-pane
  :defer t)
(latex-preview-pane-enable)

(use-package org-pomodoro
  :defer t)

;; markdown
(use-package flymd
  :defer t)

;; VIM is the way
(use-package evil)
(require 'evil)
(evil-mode 1)
(use-package evil-easymotion)
(use-package evil-cleverparens)
(use-package evil-goggles)
(use-package evil-mc)
(use-package evil-mc-extras)
(use-package evil-org)
(use-package undo-tree)

;; Config languages
(use-package fennel-mode
  :defer t)

;; ~~~~~~~~~~~~~~~~~~~~~~~~ END of Packages ~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~ Keysets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun project-view ()
  (interactive)
  (let ((dir (file-name-directory (or (buffer-file-name) default-directory))))
    (if-let ((dired-buf (get-buffer (dired-normalize-subdir dir))))
	(switch-to-buffer dired-buf)
      (dired dir))))

;; Global keysets
(global-set-key (kbd "C-x C-r") 'restart-emacs)
(global-set-key (kbd "C-c f") (lambda ()
								(interactive)
								(search-forward
								 (string (read-char "f: "))
								 (line-end-position)
								 nil)))
(global-set-key (kbd "C-x d") 'dired)
(global-set-key (kbd "C-c C-v") (lambda ()
				(interactive)
				(let ((dir (file-name-directory (or (buffer-file-name) default-directory))))
				  (if-let ((dired-buf (get-buffer (dired-normalize-subdir dir))))
				      (switch-to-buffer dired-buf)
				    (dired dir)))))

(global-set-key (kbd "C-c C-s") (lambda ()
								  (interactive)
								  (package-initialize)
								  (package-refresh-contents)))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Latex
(global-set-key (kbd "M-P") 'latex-preview-update)

;; ~~~~~~~~~~~~~~~~~~~~ END of Keysets ~~~~~~~~~~~~~~~~~~~~~~~~`

;; ~~~~~~~~~~~~~~~~~~~~~~~ INITS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Autocompletion
(with-eval-after-load 'company
  (define-key company-active-map (kbd "RET") #'newline)
  (define-key company-active-map (kbd "<return>") #'newline)
  (define-key company-active-map (kbd "C-y") #'company-complete-selection))

;; ~~~~~~~~~~~~~~~~~~~ END of INITS ~~~~~~~~~~~~~~~~~~~

;; Misc
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(tool-bar-mode -1)

(setq evil-insert-state-cursor 'box)
(define-key evil-normal-state-map (kbd "SPC f s") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC p v") 'project-view)
(define-key evil-normal-state-map (kbd "SPC s f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC s g") 'projectile-ripgrep)
(setq-default tab-width 4)
(define-key evil-normal-state-map (kbd "SPC e") 'flymake-show-diagnostic)
(define-key evil-normal-state-map (kbd "SPC E") 'flymake-show-buffer-diagnostics)
(define-key evil-normal-state-map (kbd "[ d") 'flymake-goto-prev-error)
(define-key evil-normal-state-map (kbd "] d") 'flymake-goto-next-error)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "K") 'lsp-ui-doc-glance)
(define-key evil-normal-state-map (kbd "g c") 'comment-dwim)
(add-hook 'c++-mode-hook 'lsp)

(define-key evil-normal-state-map (kbd "SPC d b") 'dap-breakpoint-toggle)
(define-key evil-normal-state-map (kbd "C-n") 'dap-next)
(define-key evil-normal-state-map (kbd "C-i") 'dap-step-in)
(define-key evil-normal-state-map (kbd "M-c") 'dap-continue)
(define-key evil-normal-state-map (kbd "SPC d r") (lambda ()
													(interactive)
													(projectile-run-project "ninja")
													(dap-debug "monk")))
(define-key evil-normal-state-map (kbd "SPC f f") 'lsp-format-buffer)

;; (add-to-list 'default-frame-alist
;;              '(font . "JetBrainsMonoNL Nerd Font Propo Regular-11"))
