;; path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; ----------------------------------------------------------------------------------
;; el-get
;; ----------------------------------------------------------------------------------

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle elpa:bind-key)
(el-get-bundle auto-complete)
;;(el-get-bundle popwin)
(el-get-bundle window-number)
(el-get-bundle smyx-theme)

;; (el-get-bundle rainbow-delimiters)
(el-get-bundle align)
(el-get-bundle yasnippet)
(el-get-bundle open-junk-file)
(el-get-bundle undo-tree)
(el-get-bundle direx)
;;(el-get-bundle tramp)
(el-get-bundle go-mode)
(el-get-bundle go-autocomplete)
(el-get-bundle go-direx)
(el-get-bundle go-eldoc)
(el-get-bundle js2-mode)
(el-get-bundle json-mode)
(el-get-bundle markdown-mode)
(el-get-bundle yaml-mode)
(el-get-bundle typescript)
(el-get-bundle tss)
(el-get-bundle php-mode)
(el-get-bundle gtags)
(el-get-bundle dockerfile-mode)

(require 'gtags)
(el-get-bundle helm)
(el-get-bundle helm-gtags)
(el-get-bundle magit)


;; ----------------------------------------------------------------------------------
;; Keybindings
;; ----------------------------------------------------------------------------------

;; bind-key
(require 'bind-key)

;; リージョンをバックスペースで消す
(delete-selection-mode 1)
(defun delete-windows-like ()
  (interactive)
  (if mark-active
      (progn
        (delete-region (point) (mark)))
    (progn
      (backward-delete-char 1)
      )
    )
  )
(global-set-key "\C-h" 'delete-windows-like)
(setq transient-mark-mode t)

; M-g でgoto-line
(global-set-key (kbd "M-g") 'goto-line)

; C-r で replace-string
(global-set-key (kbd "C-c C-s") 'replace-string)
(global-set-key (kbd "C-c C-r") 'replace-regexp)
(global-set-key (kbd "C-c C-a") 'align)

; 別ウインドウのスクロール
(global-set-key (kbd "C-M-m") 'scroll-other-window-down)
(global-set-key (kbd "C-M-v") 'scroll-other-window)

; インデント
(global-set-key (kbd "C-M-¥") 'indent-region)

; カーソル移動などの一部を camel case に変更
(autoload 'camelCase-mode "camelCase-mode" nil t)
;; (global-set-key "\M-f" 'camelCase-forward-word)
;; (global-set-key "\M-f" 'camelCase-forward-word)
;; (global-set-key "\M-b" 'camelCase-backward-word)
;; (global-set-key "\M-d" 'camelCase-forward-kill-word)

;; 行末のwhitespaceを削除
;; http://pokutuna.hatenablog.com/entry/20111117/1321523457
(setq delete-trailing-whitespace-exclude-patterns (list "\\.md$" "\\.markdown$"))

(defun delete-trailing-whitespace-with-exclude-pattern ()
  (interactive)
  (cond ((equal nil (loop for pattern in delete-trailing-whitespace-exclude-patterns
                          thereis (string-match pattern buffer-file-name)))
         (delete-trailing-whitespace))))

(add-hook 'before-save-hook 'delete-trailing-whitespace-with-exclude-pattern)


;; ファイル末尾の改行を削除
;; http://www.emacswiki.org/emacs/DeletingWhitespace
(defun my-delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

(add-hook 'before-save-hook 'my-delete-trailing-blank-lines)

;; ----------------------------------------------------------------------------------
;; Lisp
;; ----------------------------------------------------------------------------------

;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-delay 0.1)  ;; n秒後に補完開始
(setq ac-use-fuzzy t)  ;; 曖昧マッチ有効
(setq ac-use-comphist t)  ;; 補完推測機能有効
(setq ac-auto-start 3)
(setq ac-auto-show-menu 0.1)  ;; n秒後に補完メニューを表示
(define-key ac-completing-map (kbd "M-n") 'ac-next)      ; M-nで次候補選択
(define-key ac-completing-map (kbd "M-p") 'ac-previous)  ; M-pで前候補選択
(define-key ac-completing-map (kbd "TAB") 'ac-complete)  ; TABで決定
(setq ac-dwim t)  ; 空気読んでほしい

;; (setq ac-quick-help-delay 0.5)  ;; n秒後にクイックヘルプを表示

;(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))


;; 日本語設定
;(setq default-input-method "MacOSX")
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

; Macの修飾キーを変更する
;(setq mac-option-modifier 'meta)
;(setq mac-command-modifier nil)
;(setq mac-option-modifier 'meta)
;(setq mac-command-modifier 'control)

; ミニバッファ移動時の日本語自動OFF
;(mac-auto-ascii-mode 1)

;; popwin.el
;;(require 'popwin)
;; おまじない（よく分かってない、、）
;;(setq display-buffer-function 'popwin:display-buffer)

;; ポップアップを画面下に表示
;;(setq popwin:popup-window-position 'bottom)
;(setq popwin:popup-window-position 'right)

;; popwinを閉じる
;(global-set-key (kbd "C-c C-c") 'popwin:close-popup-window)

;; window-number
(require 'window-number)
(window-number-meta-mode)

;; theme
(require 'smyx-theme)
(load-theme 'smyx t)


;;; 背景を黒に
;(custom-theme-set-faces
;  'smyx
;  '(default ((t (:background "#000000" :foreground "#FFFFFF")))))


;; フォントの設定
(set-face-attribute 'default nil
                    ;:family "Source Code Pro"
                    ;:family "Terminus"
                    ;:family "Ricty Discord"
                    :family "Source Han Code JP"
                    ;:size 9
                    :height 125)

;; rainbow-delimiters
;; (require 'rainbow-delimiters)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; 現在行を目立たせる
;(global-hl-line-mode)

;; align
(require 'align)
;(add-to-list 'align-rules-list
;             '(equal-assignment
;               (regexp . "\\( *\\)=")    ; 基準を定義する正規表現
;               (modes  . '(php-mode))))

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; helm
(require 'helm-config)
(bind-key "C-;" 'helm-mini)
(helm-mode 1)

; 候補数を表示しない(パフォーマンスアップ)
(fset 'helm-show-candidate-number 'ignore)

;; ;; C-h でバックスペース
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

;; C-j
(define-key helm-map (kbd "C-j") 'helm-maybe-exit-minibuffer)

;;;; tabで補完
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

(define-key helm-map (kbd "C-k") 'kill-line)

(define-key global-map (kbd "M-x")     'helm-M-x)
;; (define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)


;; open-junk-file
(require 'open-junk-file)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "C-\\") 'undo-tree-redo)
(global-set-key (kbd "C-¥") 'undo-tree-redo)

;; psession
;(autoload 'psession-mode "psession.el")
;(psession-mode 1)

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;; ----------------------------------------------------------------------------------
;; Program mode
;; ----------------------------------------------------------------------------------

;; Go
; http://unknownplace.org/archives/golang-editing-with-emacs.htmlを参考にした

(require 'go-mode)
(require 'go-autocomplete)
(require 'auto-complete-config)
(require 'go-direx)

; フック
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook (lambda ()
			  (auto-complete-mode)
			  (camelCase-mode 1)
                          (local-set-key (kbd "C-c C-o") 'go-goto-imports)
                          (local-set-key (kbd "C-c C-u") 'go-remove-unused-imports)
                          (local-set-key (kbd "C-c C-k") 'godoc)
			  (local-set-key (kbd "C-j") 'newline-and-indent)
			  (local-set-key (kbd "C-c C-j") 'go-direx-pop-to-buffer)
			  (highlight-regexp "\\<err\\>" 'hi-red-b)
			  (setq tab-width 4)))

(eval-after-load "go-mode"
  '(progn
     ;; key bindings
     (define-key go-mode-map (kbd "C-c C-e") 'go-tmp-run)
     (define-key go-mode-map (kbd "M-.") 'godef-jump)
     (define-key go-mode-map (kbd "M--") 'pop-tag-mark)))

;; (eval-after-load 'go-mode
;;     '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map))

(add-hook 'go-mode-hook 'go-eldoc-setup)

;; helper function
(defun go ()
  "run current buffer"
  (interactive)
  (compile (concat "go run \"" (buffer-file-name) "\"")))

(defun go-run-dir ()
  "run current buffer"
  (interactive)
  (compile (concat "go run *.go")))

;; helper function
(defun go-build ()
  "build current buffer"
  (interactive)
  (compile (concat "go build  \"" (buffer-file-name) "\"")))

;; helper function
(defun go-build-dir ()
  "build current directory"
  (interactive)
  (compile "go build ."))


;; JavaScript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook
        (lambda ()
	    (auto-complete-mode)
	    (local-set-key (kbd "C-j") 'newline-and-indent)
	    (local-set-key (kbd "C-c C-s") 'replace-string)
	    (local-set-key (kbd "C-c C-r") 'replace-regexp)

            (camelCase-mode 1)
            (tern-mode t)))

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))


;; TypeScript
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(require 'tss)


;; キーバインド
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")

;; 推奨設定を行う
(tss-config-default)


;; PHP
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php5$" . php-mode))

(add-hook 'php-mode-hook
          (lambda ()
;	    (php-enable-psr2-coding-style)
	    (camelCase-mode 1)
            (local-set-key (kbd "C-c C-s") 'replace-string)
            (local-set-key (kbd "C-c C-r") 'replace-regexp)
            (local-set-key (kbd "C-c C-a") 'align)
	    (local-set-key (kbd "C-j") 'newline-and-indent)
            ;; (c-set-style "linux")
	    ;; (setq tab-width 4)
	    ;; (setq c-basic-offset 4)
	    ;; (setq indent-tabs-mode nil)
	    ;; (c-set-offset 'arglist-close 0)
))


;; WEB-mode
(add-hook 'web-mode-hook
          (lambda ()
	    "Hooks for Web mode."
	    (local-set-key (kbd "C-c C-s") 'replace-string)
	    (local-set-key (kbd "C-c C-r") 'replace-regexp)

	    (setq web-mode-markup-indent-offset    4)
	    (setq web-mode-css-offset    4)
	    (setq web-mode-script-offset 4)
	    (setq web-mode-php-offset    4)
	    (setq web-mode-java-offset   4)
	    (setq web-mode-asp-offset    4)
	    (setq indent-tabs-mode t)
	    (setq tab-width 2)
))


;; gtags
;; (require 'gtags)

;; (setq gtags-mode-hook
;;       '(lambda ()
;;          (local-set-key "\M-." 'helm-gtags-find-tag)
;;          (local-set-key (kbd "M-C-r") 'helm-gtags-find-rtag)
;;          (local-set-key "\M-s" 'helm-gtags-find-symbol)
;;          (local-set-key (kbd "M-C-.") 'helm-gtags-find-tag-from-here)
;;          (local-set-key "\M--" 'helm-gtags-pop-stack)

;; ;         (setq gtags-path-style 'root)
;;          ))

;; (add-hook 'c-mode-common-hook
;;           '(lambda()
;;              (helm-gtags-mode 1)
;;              (gtags-make-complete-list)
;;              ))

(require 'helm-gtags)

(add-hook 'c-mode-hook 'helm-gtags-mode)

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
              (local-set-key (kbd "M-.") 'helm-gtags-find-tag)
;              (local-set-key (kbd "M-C-r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
              (local-set-key "\M--" 'helm-gtags-pop-stack)))

(setq helm-gtags-use-input-at-cursor t)

;; (define-key gtags-select-mode-map "\C-t" 'gtags-pop-stack)
;; (define-key gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
;; (define-key gtags-select-mode-map "\C-j" 'gtags-select-tag)

;; magit
(require 'magit)
(setq magit-auto-revert-mode nil)
(global-set-key (kbd "C-c C-g") 'magit-status)
;; (global-set-key (kbd "C-c C-g C-c") 'magit-commit)
;; (global-set-key (kbd "C-c C-g C-d") 'magit-diff)
;; (global-set-key (kbd "C-c C-g C-l") 'magit-log)

(set-face-background 'magit-section-highlight "#303033")

;; ----------------------------------------------------------------------------------
;; Misc
;; ----------------------------------------------------------------------------------

;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; メニューを表示しない
(tool-bar-mode 0)
(menu-bar-mode 0)

;; 日本語
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(setq default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(blink-cursor-mode 1)

;; 一行ずつスクロール
;; (setq scroll-conservatively 20
;;       scroll-margin 10
;;       scroll-step 1)

;; スクロールバーを表示しない
(if window-system
        (progn
	  (scroll-bar-mode -1)))

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

;; Emacs server
;; (setq server-host "192.168.34.146")
;; (setq server-use-tcp t)
(unless (server-running-p)
   (server-start))
