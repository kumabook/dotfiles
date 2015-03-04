; basic key bind
(keyboard-translate ?\C-h ?\C-?)
(global-unset-key "\C-z")
(define-key global-map [?¥] [?\\])

;; Cask
(require 'cask "/usr/local/opt/cask/cask.el")
(cask-initialize)

;; replace Command and Option
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; character-code
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; fonts
(cond (window-system
       (set-default-font "M+ 1mn-13")
       (set-fontset-font (frame-parameter nil 'font)
                         'japanese-jisx0208
                         '("M+1MN+IPAG" . "unicode-bmp"))))

;; add package site
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

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

(set-frame-position (selected-frame) 0 0)
(set-frame-size (selected-frame) 200 100)
(when window-system
  (add-hook 'after-init-hook
            '(lambda ()
               (run-with-idle-timer
                0.1
                nil
                '(lambda ()
                   (set-frame-parameter nil 'fullscreen 'maximized))))))


;;;; color-theme ;;;;
(require 'color-theme)
(color-theme-initialize)
(color-theme-classic)


;;;; linum ;;;;
(global-linum-mode t)
(setq linum-format "%5d ")

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

;;;; auto-complete ;;;;
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)

;;;; helm ;;;;
(require 'helm-config)
(global-set-key (kbd "C-l") 'helm-mini)
(helm-mode 1)
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))

;;;; git-gutter-fringe ;;;;
(require 'git-gutter-fringe)
(global-git-gutter-mode)

;;;; indent ;;;;
(setq-default c-basic-offset 2
              tab-width 2
              indent-tabs-mode nil)

;; js2-mode ;;
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; ruby-mode ;;
(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

;; ruby-block ;;
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

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

;; rainbow mode
(require 'rainbow-mode)
(add-hook 'css-mode-hook  'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;; TODO customize whitespace-mode config
;; see http://www.emacswiki.org/emacs/WhiteSpace
(global-whitespace-mode 1)

(line-number-mode t)
(column-number-mode t)

(show-paren-mode 1)

(setq inhibit-startup-message t)
(cd "~/")
