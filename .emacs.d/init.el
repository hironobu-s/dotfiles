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
    flymake-easy
    pos-tip
    magit
    popwin
    undo-tree
    ;auto-complete
    go-autocomplete
    helm-go-package
    avy
    
    ;eglot
    company
    lsp-mode
    company-lsp
    lsp-ui
    
    ;; helm
    helm

    ;; theme
    color-theme-modern

    ;; swiper
    ;; swiper
    ;; swiper-helm

    ;; ivy/counsel
    ;; counsel

    ;; program-mode
    go-mode
    dap-mode
    js2-mode
    json-mode
    markdown-mode
    yaml-mode
    php-mode
    ac-php
    web-mode
    scss-mode
    sql-indent
    rainbow-mode
    dockerfile-mode
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

;; forward-to-word, backward-to-word
(bind-key "M-f" 'forward-word)
(bind-key "M-b" 'backward-word)

;; ファンクションキーを無効にする
(define-key global-map [f1] nil)
(define-key global-map [f2] nil)
(define-key global-map [f3] nil)

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
;(load-theme 'subdued t)

;; 背景を黒に
(custom-theme-set-faces
 'deeper-blue
 '(default ((t (:background "#000000":foreground "#FFFFFF")))))

;; 現在の行をハイライト
;; (global-hl-line-mode t)

;; (custom-set-faces
;; '(hl-line ((t (:background "#111111"))))
;; )
;; (setq hl-line-face 'underline)

;; ヤンクした内容ををクリップボードに送信
(require 'osc52e)
(osc52-set-cut-function)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(create-lockfiles nil)
;;  '(custom-safe-themes
;;    (quote
;;     ("595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1" "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "5c5de678730ceb4e05794431dd65f30ffe9f1ed6c016fa766cdf909ba03e4df4" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" default)))
;;  '(lsp-inhibit-message t t)
;;  '(lsp-message-project-root-warning t t)
;;  '(lsp-ui-doc-enable nil)
;;  '(lsp-ui-doc-header t)
;;  '(lsp-ui-doc-include-signature t)
;;  '(lsp-ui-doc-max-height 30)
;;  '(lsp-ui-doc-max-width 150)
;;  '(lsp-ui-doc-position (quote at-point))
;;  '(lsp-ui-doc-use-childframe t)
;;  '(lsp-ui-doc-use-webkit t)
;;  '(lsp-ui-flycheck-enable nil)
;;  '(lsp-ui-imenu-enable nil)
;;  '(lsp-ui-imenu-kind-position (quote top))
;;  '(lsp-ui-peek-enable t)
;;  '(lsp-ui-peek-fontify (quote on-demand))
;;  '(lsp-ui-peek-list-width 50)
;;  '(lsp-ui-peek-peek-height 20)
;;  '(lsp-ui-sideline-enable nil)
;;  '(osc52-multiplexer (quote tmux))
;;  '(package-selected-packages
;;    (quote
;;     (dap-mode lsp-mode avy dockerfile-mode rainbow-mode "rainbow-mode" sql-indent scss-mode web-mode php-mode yaml-mode markdown-mode json-mode counsel swiper-helm swiper helm helm-go-package go-autocomplete auto-complete undo-tree popwin pos-tip flycheck-tip flycheck bm yasnippet window-number use-package bind-key magit))))
;; ;;(custom-set-variables '(osc52-multiplexer 'screen)) ;; screenを使っている場合はこっち

;; ;; osc52e自体はリージョンを送る関数は提供していないので、自分で定義する
;; (defun osc52-send-region-to-clipboard (START END)
;;   "Copy the region to the system clipboard using the OSC 52 escape sequence."
;;   (interactive "r")
;;   (osc52-interprogram-cut-function (buffer-substring-no-properties
;;                            START END)))

;; ;; 適当にバインドする
;; (global-set-key (kbd "C-x M-w") 'osc52-send-region-to-clipboard)


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

;; fly-check
(use-package flycheck
  :bind (
	 ("M-N" . 'flycheck-next-error)
	 ("M-P" . 'flycheck-previous-error)))

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

;; window-number (switch window)
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
;; (use-package auto-complete
;;   :config
;;   (ac-config-default)
;;   (setq ac-delay 0.01)
;;   (setq ac-auto-show-menu 0.01)
;;   ;(setq ac-use-menu-map t)
;;   (setq ac-use-quick-help nil) 
;;   ;(setq ac-ignore-case nil)
;;   (setq ac-dwim t)
;;   (setq ac-fuzzy-enable t)
;;   (setq ac-use-comphist t)

;;   :bind (
;; 	 :map ac-completing-map
;; 	      ("M-n" . ac-next)
;; 	      ("M-p" . ac-previous)
;; 	      ("TAB" . ac-complete)))


;; (use-package eglot
;;   :config
;;   (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
;;   (define-key eglot-mode-map (kbd "M--") 'pop-tag-mark)
;;   (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
;;   (add-hook 'go-mode-hook 'eglot-ensure))


;; (use-package lsp-go
;;   :after (lsp-mode go-mode)
;;   :custom (lsp-go-language-server-flags '(
;;     "-gocodecompletion"
;;     "-diagnostics"
;;     "-lint-tool=golint"))
;;   :hook (go-mode . lsp-go-enable)
;;   :commands lsp-go-enable)

(use-package lsp-mode
  :custom ((lsp-inhibit-message t)
         (lsp-message-project-root-warning t)
         (create-lockfiles nil))
  :hook   (prog-major-mode . lsp-prog-major-mode-enable))

(use-package lsp-ui
  :after lsp-mode
  :custom (scroll-margin 0)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-position (quote at-point))
  (lsp-ui-doc-use-childframe nil)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-flycheck-enable nil)
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position (quote top))
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-fontify (quote on-demand))
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-sideline-enable nil)
  
  :hook   (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :after (lsp-mode company yasnippet)
  :defines company-backends
  ;; :functions company-backend-with-yas
  ;; :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends))
)

;; company-mode
(use-package company
  ;; :bind (
  ;; 	 ("C-i" . company-complete-selection)
  ;; 	 ("C-s" . company-filter-candidates)
  ;; 	 ("C-h" . nil))
  
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t)

  (define-key company-active-map (kbd "TAB") 'company-complete-selection) ;; TABで候補を設定
  (define-key company-active-map (kbd "C-h") nil) ;; C-hはバックスペース割当のため無効化
  (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer) ;; ドキュメント表示はC-Shift-h
  
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40")
)

;; ʕ◔ϖ◔ʔ < GO
(use-package go-mode
  :init
  (add-hook 'go-mode-hook #'lsp)
  
  :bind (
  	 ("M-." . lsp-ui-peek-find-definitions)
  	 ("M--" . pop-tag-mark)
	 ("C-c i" . lsp-ui-imenu)
  	 ("C-j" . newline-and-indent))
  	 ;("C-c C-c" . avy-goto-char-timer))

  ;; :mode
  :hook (go-mode . (lambda ()
		     (setq tab-width 4)
		     ;; (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change new-line))
		     ;; (setq flycheck-idle-change-delay 1)
		     (subword-mode)
		     (show-paren-mode)
		     ;(auto-complete-mode)
		     (flycheck-mode)
		     
		     ;; (setq flycheck-go-build-tags " -o  *.go")
		     ;;(window-number-meta-mode)
		     (highlight-regexp "\\_<err\\_>" 'hi-red-b)
		     (highlight-regexp "\\_<errCh\\_>" 'hi-red-b)
		     ;(setq lsp-enable-completion-at-point t)
		     
		     (substitute-key-definition 'go-import-add 'helm-go-package go-mode-map)
		     ))

  :config 
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  )

;; php-mode
(use-package php-mode
  :config
  (use-package ac-php)
  :hook (php-mode . (lambda ()
		      ;(auto-complete-mode t)
                      (subword-mode)
		      (setq ac-sources  '(ac-source-php ) )
		      (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
		      (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
		      )))

;; js2-mode
(use-package js2-mode
  :mode (
	 ("\\.js?\\'" . js2-mode)
	 )
  
  :hook (js2-mode . (lambda ()
			     (subword-mode)
			     (show-paren-mode)
			     ;(auto-complete-mode)
			     ))
  
  :config 
	 (bind-key "C-c C-s" 'replace-string)
	 (bind-key "C-c C-r" 'replace-regexp)
)

;; web-mode
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
	 ("\\.tpl\\'" . web-mode)
         ("\\.vue\\'"   . web-mode))
  
  :hook (web-mode . (lambda ()
		      (subword-mode)
		      (rainbow-mode)
		      ;(auto-complete-mode)
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


;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; )

;; ;(put 'upcase-region 'disabled nil)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (lsp-go yaml-mode window-number web-mode use-package undo-tree sql-indent scss-mode rainbow-mode pos-tip popwin magit lsp-ui json-mode js2-mode helm-go-package helm go-autocomplete flymake-easy flycheck-tip dockerfile-mode dap-mode company-lsp color-theme-modern bm avy ac-php))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dockerfile-mode rainbow-mode sql-indent scss-mode web-mode ac-php php-mode yaml-mode json-mode js2-mode dap-mode color-theme-modern helm lsp-ui company-lsp lsp-mode company avy helm-go-package go-autocomplete undo-tree popwin magit pos-tip flymake-easy flycheck-tip flycheck bm yasnippet window-number use-package bind-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
