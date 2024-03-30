;; basic key bind
(keyboard-translate ?\C-h ?\C-?)
(global-unset-key "\C-z")
(define-key global-map [?¥] [?\\])
(global-auto-revert-mode 1)

(defvar browse-url-galeon-program nil)
(defvar browse-url-mosaic-program nil)
(defvar browse-url-netscape-program nil)

(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode))


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'dracula-theme)

(straight-use-package 'helm)
(straight-use-package 'helm-git-grep)
(straight-use-package 'helm-gtags)
(straight-use-package 'helm-ls-git)
(straight-use-package 'helm-ghq)

(straight-use-package 'company)
(straight-use-package 'elscreen)

(straight-use-package 'flymake-easy)
(straight-use-package 'flycheck)

(straight-use-package 'quickrun)
(straight-use-package 'popwin)
(straight-use-package 'powerline)

(straight-use-package 'reveal-in-osx-finder)

;;;; git ;;;;
(straight-use-package 'git)
(straight-use-package 'gist)
(straight-use-package 'gh)

(straight-use-package 'git-gutter)

;;;; prog modes ;;;;
(straight-use-package 'haskell-mode)
(straight-use-package 'ghc)
(straight-use-package 'ghci-completion)

(straight-use-package 'io-mode)
(straight-use-package 'jade-mode)
(straight-use-package '(js2-mode :type git :host github :repo "mooz/js2-mode"))
(straight-use-package 'json-mode)
(straight-use-package 'web-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'ruby-mode)
(straight-use-package 'slim-mode)
(straight-use-package 'haml-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'swift-mode)
(straight-use-package 'typescript-mode)
;(straight-use-package 'tree-sitter)
;(straight-use-package '(tsi :type git :host github :repo "orzechowskid/tsi.el"))
(straight-use-package 'lsp-mode)
(straight-use-package 'yasnippet)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'helm-lsp)
(straight-use-package 'projectile)
(straight-use-package 'hydra)
(straight-use-package 'hydra)
(straight-use-package 'avy)
(straight-use-package 'which-key)
(straight-use-package 'helm-xref)
(straight-use-package 'dap-mode)
(straight-use-package 'zenburn-theme)
(straight-use-package 'json-mode)
;;(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))

(straight-use-package 'php-mode)
(straight-use-package 'tide)
(straight-use-package 'scss-mode)

;;;; ruby ;;;;
(straight-use-package 'rubocop)
(straight-use-package 'enh-ruby-mode)

(straight-use-package 'rainbow-mode)
(straight-use-package 'exec-path-from-shell)

;(straight-use-package 'tramp-container)


;; path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; replace Command and Option
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; character-code
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; fonts
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :family "Ricty"
                      :height 140)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (cons "Ricty" "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0212
                    (cons "Ricty" "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
                    'katakana-jisx0201
                    (cons "Ricty" "iso10646-1"))
  )

;; theme
(require 'dracula-theme)
(load-theme 'dracula t)

;;;; elscreen ;;;;
(setq elscreen-prefix-key "\C-j")
(global-set-key [(C-tab)] 'elscreen-next)
(global-set-key [(C-S-tab)] 'elscreen-previous)
(elscreen-start)
; disable ctrl-j on lisp-interaction-mode
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (progn
              (local-unset-key "\C-j")
              (local-set-key "\C-J" 'eval-print-last-sexp))))
;(add-hook 'php-mode-hook
;          (lambda ()
;            (setq tab-width 2)
;            (setq c-basic-offset 2)
;            (setq indent-tabs-mode t)))
(add-hook 'php-mode-hook 'php-enable-wordpress-coding-style)
(add-hook 'php-mode-hook 'my-php-mode-hook)
(defun my-php-mode-hook ()
  "My PHP mode configuration."
  (setq indent-tabs-mode t
        tab-width 4
        c-basic-offset 4))

(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
            ))

(when window-system
  (add-hook 'after-init-hook
            '(lambda ()
               (run-with-idle-timer
                0.1
                nil
                '(lambda ()
                   (set-frame-parameter nil 'fullscreen 'maximized))))))

;;;; global ;;;;
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))
(add-hook 'c-mode-common-hook
          '(lambda()
             (gtags-mode 1)
             (gtags-make-complete-list)
             ))

;;;;;; company-mode ;;;;
(global-company-mode +1)

;;;; helm ;;;;
(require 'helm-files)
(require 'helm-for-files)
(global-set-key (kbd "C-l") 'helm-mini)
(helm-mode 1)
(define-key global-map (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
;;;;;; helm-ghq ;;;;;;
(add-to-list 'helm-for-files-preferred-list 'helm-source-ghq)
(define-key global-map (kbd "C-'") 'helm-ghq)

;;;;;; helm-git-grep ;;;;;;
(require 'compile)
(global-set-key (kbd "C-c g") 'helm-git-grep)
(global-set-key (kbd "C-c i") 'helm-git-grep-at-point)
;; Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
;; Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))

;;;; git-gutter-fringe ;;;;
(global-git-gutter-mode +1)
;(when (display-graphic-p)
;  (global-git-gutter-mode))


;;;; indent ;;;;
(setq-default c-basic-offset 2
              tab-width 2
              indent-tabs-mode nil)

;(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; javascript ;;
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

(add-hook 'typescript-mode-hook (lambda () (setq typescript-indent-level 2)))

(eval-after-load 'flycheck
  '(progn
     (flycheck-add-mode 'javascript-eslint 'web-mode)
     (flycheck-add-mode 'javascript-eslint 'js2-mode)
     (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
     '(custom-set-variables
       '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs javascript-gjslint))
       )))

(setq-default flycheck-disabled-checkers '(javascript-jshint))
(setq-default flycheck-disabled-checkers '(javascript-jscs))
(setq-default flycheck-disabled-checkers '(javascript-gjslint))

(setq js2-strict-trailing-comma-warning nil)
(setq js2-strict-missing-semi-warning t)
(setq js2-missing-semi-one-line-override t)
(setq js2-strict-inconsistent-return-warning nil)
(setq js-switch-indent-offset 2)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-javascript-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset)
)

(add-hook 'web-mode-hook  'my-web-mode-hook)

;; scss mode
(defun scss-custom ()
  "scss-mode-hook"
  (and
   (set (make-local-variable 'css-indent-offset) 2)
   (set (make-local-variable 'scss-compile-at-save) nil)
   )
  )
(add-hook 'scss-mode-hook
  '(lambda() (scss-custom)))


;; ruby-mode ;;
(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(setq ruby-insert-encoding-magic-comment nil)

;; prolog-mode ;;
(setq auto-mode-alist
      (append '(("\\.pl" . prolog-mode))
              auto-mode-alist))
(setq prolog-program-name "gprolog")
(setq prolog-consult-string "[%f].\n")

;; haskell
(add-hook 'haskell-mode-hook
          (lambda ()
            (progn
;              (turn-on-haskell-simple-indent)
              (turn-on-haskell-indent)
              (c-basic-offset 4)
              (tab-width 4)
              )))

;; swift
(add-hook 'swift-mode-hook
          (lambda ()
            (setq-local tab-width 4)
            (defvar swift-indent-offset)
            (setq-local swift-indent-offset 4)))


;; rainbow mode
(require 'rainbow-mode)
(add-hook 'css-mode-hook  'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;; TODO customize whitespace-mode config
;; see http://www.emacswiki.org/emacs/WhiteSpace
(global-whitespace-mode 1)

;(line-number-mode t)
;(column-number-mode t)

(show-paren-mode 1)

(defun update-alpha () (set-frame-parameter nil 'alpha frame-alpha))
(defun up-alpha ()
  "set frame parameter 'alpha"
  (interactive)
  (set 'frame-alpha (min (+ frame-alpha 5) 100))
  (update-alpha))
(defun down-alpha ()
  "set frame parameter 'alpha"
  (interactive)
  (set 'frame-alpha (max (- frame-alpha 5) 0))
  (update-alpha))
(set 'frame-alpha 95)
(update-alpha)

(global-set-key (kbd "C-x C-p") 'up-alpha)
(global-set-key (kbd "C-x C-n") 'down-alpha)

(setq inhibit-startup-message t)
(cd "~/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dracula-theme yaml-mode web-mode tide swift-mode slim-mode scss-mode rust-mode rubocop rainbow-mode quickrun powerline popwin php-mode php-completion markdown-mode json-mode js2-mode jade-mode io-mode helm-ls-git helm-gtags helm-git-grep helm-ghq haml-mode git gist ghci-completion ghc flymake-easy exec-path-from-shell erlang enh-ruby-mode elscreen company-go color-theme coffee-mode clojure-mode alchemist))
 '(warning-suppress-types '((lsp-mode) (lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;(require 'docker-tramp-compat)

(require 'reveal-in-osx-finder)
(global-set-key (kbd "C-c o") 'reveal-in-osx-finder)


(require 'helm-xref)
(which-key-mode)
(add-hook 'prog-mode-hook #'lsp)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      create-lockfiles nil) ;; lock files will kill `npm start'
(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))
