
(ivy-mode 1)
;;(load-theme 'monokai t)

(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

(if window-system
    (progn
      ;;      (set-font "Cascadia Mono" "Microsoft Yahei UI" 14 16)
      (set-font "DejaVu Sans Mono" "WenQuanYi Micro Hei" 16 20)
     ;; (set-font "DejaVu Sans Mono" "WenQuanYi Micro Hei" 24 28)
      ))

;;(require-package 'cnfonts)
;;(cnfonts-enable)

(setq org-export-with-sub-superscripts nil)
(set-default 'truncate-lines t)
;;(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
(setq org-footnote-auto-adjust t)
(defun org-export-docx ()
  (interactive)
  (shell-command (format "/home/zmm/Win-Share/Emacs/org2word.sh %s" buffer-file-name ))
  (message "Convert finish: %s" buffer-file-name))

(defun previous-multilines ()
  "scroll down multiple lines"
  (interactive)
  (scroll-down (/ (window-body-height) 3))
  )
(defun next-multilines ()
  "scroll up multiple lines"
  (interactive)
  (scroll-up (/ (window-body-height) 3))
  )

(global-set-key "\M-n" 'next-multilines)
(global-set-key "\M-p" 'previous-multilines)

(add-hook 'org-mode-hook
	  (lambda ()
	    (require 'org-tempo)
        (setq word-wrap nil)))


(setq-default org-html-mathjax-options
  '((path "https://cdnjs.loli.net/ajax/libs/mathjax/3.2.0/es5/tex-mml-chtml.min.js")
    ;;(scale "100")
    (align "center")
    (indent "2em")
    ;;(mathml t)
    ))

(global-set-key (kbd "C-]") 'counsel-etags-find-tag-at-point)
