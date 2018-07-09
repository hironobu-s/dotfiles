(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
	("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(defvar packages
  '(
    ;; general
    bind-key
    use-package
    window-number
    yasnippet
    bm
    flycheck
    flycheck-tip
    pos-tip
    magit
    popwin
    undo-tree
    auto-complete
    go-autocomplete
    helm-go-package
    
    ;; helm
    helm

    ;; swiper
    ;; swiper
    ;; swiper-helm

    ;; ivy/counsel
    ;; counsel

    ;; program-mode
    go-mode
    json-mode
    markdown-mode
    yaml-mode
    php-mode
    ac-php
    web-mode
    scss-mode
    sql-indent
    rainbow-mode
    ))

(defun install-packages (packages)
  (let ((refreshed nil))
    (dolist (pack packages)
      (unless (package-installed-p pack)
	(unless refreshed
	  (package-refresh-contents)
	  (setq refreshed t))
	(package-install pack)))))

(install-packages packages)


;; -------------

;; C-hでバックスペース
(bind-key "\C-h" 'backward-delete-char-untabify)

;; M-g でgoto-line
(bind-key "M-g" 'goto-line)

;; C-r で replace-string
(bind-key "C-c C-s" 'replace-string)
(bind-key "C-c C-r" 'replace-regexp)

;; ; 別ウインドウのスクロール
;; (global-set-key (kbd "C-M-m") 'scroll-other-window-down)
;; (global-set-key (kbd "C-M-v") 'scroll-other-window)

;; ; インデント
(bind-key "C-M-¥" 'indent-region)

;; ----------------------------------------------------------

;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;;;バックアップ・オートセーブファイルの作成をやめる
(setq make-backup-files nil)
(setq auto-save-default nil)

;; ベルを鳴らさない
(setq ring-bell-function 'ignore)

;; 質問に yes と入力するのを y に変更する
(fset 'yes-or-no-p 'y-or-n-p)

;; kill-line の時、行頭にカーソルがある場合、改行文字も消す
(setq kill-whole-line 1)
(put 'downcase-region 'disabled nil)

;; 表示しない
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Window mode専用の設定
(if window-system
        (progn
          (scroll-bar-mode -1)))

;; theme
(load-theme 'deeper-blue t)
;;; 背景を黒に
(custom-theme-set-faces
 'deeper-blue
 '(default ((t (:background "#000000" :foreground "#FFFFFF")))))

;; ------------------------------------------------------------

;; Emacs server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  (global-set-key (kbd "C-\\") 'undo-tree-redo)
  (global-set-key (kbd "C-¥") 'undo-tree-redo))

;; bm
(use-package bm 
  :bind (
	("M-SPC" . 'bm-toggle)
	("M-p" . 'bm-previous)
	("M-n" . 'bm-next)
	("M-s" . 'bm-show)))

;; helm
(use-package helm
  :bind(
	("C-;" . helm-mini)
	("M-x" . 'helm-M-x)
	;; ("C-x C-f") 'helm-find-files)
	("C-x C-r" . 'helm-recentf)
	("M-y" . 'helm-show-kill-ring)
	("C-c i" . 'helm-imenu)
	("C-x b" . 'helm-buffers-list)
	("C-x c" . 'helm-find-files)
	("C-x C-f" . 'helm-find-files)   

	:map helm-map
	("C-h" . delete-backward-char)
	("C-j" . helm-maybe-exit-minibuffer)
	("C-k" . kill-line)
	("TAB" . helm-execute-persistent-action)
	
	:map helm-find-files-map
	("TAB" . helm-execute-persistent-action)
	
	:map helm-read-file-map
	("TAB" . helm-execute-persistent-action))
  
  :config
  (helm-mode 1))

;; window-number
(use-package window-number
  :config
  (window-number-meta-mode))

;; swiper-helm
;; (use-package swiper-helm
;;   :bind (("C-s" . swiper-helm)))

;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; magit
(use-package magit
  :config
  (setq magit-auto-revert-mode nil)
  (set-face-background 'magit-section-highlight "#303033")
  (global-set-key (kbd "C-c C-g") 'magit-status))

;; auto-complete
(use-package auto-complete
  :config
  (ac-config-default)
  (setq ac-delay 0.01)
  (setq ac-auto-show-menu 0.01)
  ;(setq ac-use-menu-map t)
  (setq ac-use-quick-help nil) 
  ;(setq ac-ignore-case nil)
  (setq ac-dwim t)
  (setq ac-fuzzy-enable t)
  (setq ac-use-comphist t)

  :bind (
	 :map ac-completing-map
	      ("M-n" . ac-next)
	      ("M-p" . ac-previous)
	      ("TAB" . ac-complete)))


;; ʕ◔ϖ◔ʔ < GO
(use-package go-mode
  :init
  (setq gofmt-command "goimports")

  :bind (
	 ("M-." . godef-jump)
	 ("M--" . pop-tag-mark)
	 ("C-j" . newline-and-indent))

  ;; :mode
  :hook (go-mode . (lambda ()
		     (setq tab-width 4)
		     (subword-mode)
		     (show-paren-mode)
		     (auto-complete-mode)
		     (flycheck-mode)
		     (window-number-meta-mode)
		     (highlight-regexp "\\_<err\\_>" 'hi-red-b)
		     (highlight-regexp "\\_<errCh\\_>" 'hi-red-b)
		     (substitute-key-definition 'go-import-add 'helm-go-package go-mode-map)))

  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (use-package go-autocomplete))

;; php-mode
(use-package php-mode
  :config
  (use-package ac-php)
  :hook (php-mode . (lambda ()
		      (auto-complete-mode t)
                      (subword-mode)
		      (setq ac-sources  '(ac-source-php ) )
		      (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
		      (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
		      )))

;; web-mode
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
	 ("\\.tpl\\'" . web-mode)
         ("\\.vue\\'"   . web-mode))
  
  :hook (web-mode . (lambda ()
		      (subword-mode)
		      (rainbow-mode)
		      ))
  
  :config
  (bind-key "C-c C-s" 'replace-string)
  (bind-key "C-c C-r" 'replace-regexp)

  (setq web-mode-markup-indent-offset    2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2)
  (setq indent-tabs-mode nil)

  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-auto-close-style 1)
  (setq web-mode-tag-auto-close-style t)

  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-current-column-highlight t)

  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-css-property))
	  ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rainbow-mode "rainbow-mode" sql-indent scss-mode web-mode php-mode yaml-mode markdown-mode json-mode counsel swiper-helm swiper helm helm-go-package go-autocomplete auto-complete undo-tree popwin pos-tip flycheck-tip flycheck bm yasnippet window-number use-package bind-key magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

