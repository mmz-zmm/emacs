(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line

(add-to-list 'load-path "~/.emacs.d/lisp/")

(global-auto-revert-mode t)                  ; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer

(global-hl-line-mode t)			     ; highlight current line
;; cc style
(setq c-default-style "linux"
      c-basic-offset 8
      tab-width 8
      intent-tabs-mode t)

(setq confirm-kill-emacs #'yes-or-no-p)      ; 在关闭 Emacs 前询问是否确认关闭，防止误触
(electric-pair-mode t)                       ; 自动补全括号
(add-hook 'prog-mode-hook #'show-paren-mode) ; 编程模式下，光标在括号上时高亮另一个括号
(column-number-mode t)                       ; 在 Mode line 上显示列号
;;(delete-selection-mode t)                    ; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
;;(setq inhibit-startup-message t)             ; 关闭启动 Emacs 时的欢迎界面
;;(setq make-backup-files nil)                 ; 关闭文件自动备份
(add-hook 'prog-mode-hook #'hs-minor-mode)   ; 编程模式下，可以折叠代码块
(global-display-line-numbers-mode 1)         ; 在 Window 显示行号
;;(tool-bar-mode -1)                           ; （熟练后可选）关闭 Tool bar
;;(when (display-graphic-p) (toggle-scroll-bar -1)) ; 图形界面时关闭滚动条

(savehist-mode 1)                            ; （可选）打开 Buffer 历史记录保存
;;(setq display-line-numbers-type 'relative)   ; （可选）显示相对行号

;; global hotkey
(global-set-key (kbd "M-/") 'hippie-expand) ; 补全

;; ggtags always at bottom
(add-to-list 'display-buffer-alist
             '("ggtags-global"
               (display-buffer-in-side-window)
               (side . bottom)))
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
;; packages
(use-package counsel
  :ensure t)

(defun my-counsel-etags-grep (search-term)
  "调用 counsel-etags-grep 来执行查询操作"
  (interactive "sEnter search term (default at point): ")
  (unless (string= search-term "")
    (counsel-etags-grep search-term))
  (when (string= search-term "")
    (setq search-term (thing-at-point 'symbol))
    (counsel-etags-grep search-term)))


(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-re-builders-alist '(
                                (t . ivy--regex-ignore-order)
				(t . ivy--regex-fuzzy)
                                ))
  (setq ivy-height 10)
  (setq counsel-find-file-at-point t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x g" . 'counsel-rg)
   ("C-x C-g" . 'my-counsel-etags-grep)
   ;;("C-x g" . 'counsel-etags-grep)
   ("C-x C-@" . 'counsel-mark-ring); 在某些终端上 C-x C-SPC 会被映射为 C-x C-@，比如在 macOS 上，所以要手动设置
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(require 'find-file-in-project)

(use-package tramp
  :defer t
  :config
  (setq tramp-inline-compress-start-size 1000)
  (setq tramp-copy-size-limit 10000)
  (setq vc-handled-backends '(Git))
  (setq tramp-verbose 1)
  (setq tramp-default-method "scp")
  (setq tramp-use-ssh-controlmaster-options nil)
  (setq tramp-verbose 1))
(use-package amx
  :ensure t
  :init (amx-mode))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

;; (use-package counsel-etags
;;   :ensure t
;;   :bind (("M-." . counsel-etags-find-tag-at-point))
;;   :init
;;   (add-hook 'prog-mode-hook
;;             (lambda ()
;;               (add-hook 'after-save-hook
;; 			'counsel-etags-virtual-update-tags 'append 'local)))
;;   :config
;;   (setq counsel-etags-update-interval 10)
;;   (push "build" counsel-etags-ignore-directories))

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1))))

;; (use-package counsel-gtags
;;   :ensure t
;;   :bind-keymap ("C-c g" . counsel-gtags-command-map)
;;   :init
;;   (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (counsel-gtags-mode 1))))
;;  )
(use-package ggtags
  :ensure t
  :bind  (:map ggtags-navigation-mode-map)
  :init
  (add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))
  ;;:config
  ;;(setq ggtags-oversize-limit 1000000000)
  )


(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 2) ; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.3)
  (setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil) ;; 区分大小写
  (setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序，读者如果不喜欢可以去掉

(use-package company-ctags
  :ensure t)
(with-eval-after-load 'company
  (company-ctags-auto-setup))

(setq company-backends
      '(
        (company-capf company-dabbrev-code company-keywords company-files)
        ))

;; git
(use-package magit
  :ensure t)

(use-package vc-msg
  :ensure t)
(setq vc-msg-git-show-commit-function 'magit-show-commit)

;; keyfreq
(use-package keyfreq
  :ensure t
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  :config
  (setq keyfreq-excluded-commands
	'(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line))
  )

;; format
;; (use-package format-all
;;   :commands format-all-mode
;;   ;;:hook (prog-mode . format-all-mode)
;;   :bind (("C-c \\" . format-all-buffer))
;;   :config
;;   (setq-default format-all-formatters
;;                 '(("C"     (astyle "--mode=c"))
;;                   ("Shell" (shfmt "-i" "4" "-ci"))
;; 		  ("Python" (ruff))
;; 		  ("C++" (astyle))
;; 		  )))

;; clang format
(use-package clang-format
  :ensure t
  )

;; rust mode

(use-package rust-mode
  :ensure t
  )

(use-package xclip
  :ensure t
  :init
  (xclip-mode 1))

;; ;; Default light theme
;; (set-background-color "white")
;; (set-foreground-color "black")

;; ;; ;; initial window settings
;; ;; (setq initial-frame-alist
;; ;;       '((background-color . "honeydew")))

;; ;; ;; subsequent window settings
;; ;; (setq default-frame-alist
      ;; '((background-color . "honeydew")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(tango-dark))
 '(custom-safe-themes
   '("8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378" "5aedf993c7220cbbe66a410334239521d8ba91e1815f6ebde59cecc2355d7757" "5a0ddbd75929d24f5ef34944d78789c6c3421aa943c15218bac791c199fc897d" "18a1d83b4e16993189749494d75e6adb0e15452c80c431aca4a867bcc8890ca9" "75b371fce3c9e6b1482ba10c883e2fb813f2cc1c88be0b8a1099773eb78a7176" "51fa6edfd6c8a4defc2681e4c438caf24908854c12ea12a1fbfd4d055a9647a3" "8dbbcb2b7ea7e7466ef575b60a92078359ac260c91fe908685b3983ab8e20e3f" "42abd324628cb258bb8bbb1fc8ebcd4920f6681f616eb1ac80c6f8853258c595" default))
 '(font-use-system-font t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(benchmark-init xclip gruvbox-theme monokai-theme counsel-gtags projectile citre rust-mode wgrep clang-format company-ctags yasnippet which-key use-package-hydra hydra ace-window keyfreq magit vc-msg molokai-theme company counsel-etags amx markdown-mode counsel))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Maple Mono Normal NL" :foundry "    " :slant normal :weight medium :height 113 :width normal)))))

(desktop-save-mode 1)
(put 'narrow-to-region 'disabled nil)
