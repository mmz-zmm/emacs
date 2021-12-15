;; set language environment
(set-language-environment 'UTF-8)
(set-locale-environment "UTF-8")

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line
(load-theme 'monokai t)
;;中文与外文字体设置
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
      ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" default)))
 '(package-selected-packages
   (quote
    (rust-mode monokai-theme ox-latex-subfigure htmlize unicad company markdown-mode))))

;; stop the fucking sound
(setq visible-bell 0)
;; truncate line to avoid too long text
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
;; when export, don't treate underline as subscript
(setq org-export-with-sub-superscripts nil)
;; auto indent org file
;;(setq org-startup-indented 1）
;;(setq line-spacing 8)
;; show line num
(global-linum-mode 1)
;; global auto complete
(global-company-mode 1)
;; delete selection
(delete-selection-mode 1)
;; highlight current line
;;(global-hl-line-mode 1)
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; for export latex
(require 'ox-latex)
(add-to-list 'org-latex-classes
	     '("ctexart"
	       "\\documentclass[UTF8,a4paper]{ctexart}
                \\setCJKmainfont{WenQuanYi Micro Hei}
                \\CTEXsetup[format={\\Large\\bfseries}]{section}
                "
	       ;;"\\documentclass[fontset=WenQuanYi Micro Hei,UTF8,a4paper,zihao=-4]{ctexart}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
	       )
	     )


(add-to-list 'org-latex-classes
	       '("ctexrep"
		"\\documentclass[UTF8,a4paper]{ctexrep}"
		("\\part{%s}" . "\\part*{%s}")
		("\\chapter{%s}" . "\\chapter*{%s}")
		("\\section{%s}" . "\\section*{%s}")
		("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       )
	       )

(add-to-list 'org-latex-classes
	       '("ctexbook"
		"\\documentclass[UTF8,a4paper]{ctexbook}"
		;;("\\part{%s}" . "\\part*{%s}")
		("\\chapter{%s}" . "\\chapter*{%s}")
		("\\section{%s}" . "\\section*{%s}")
		("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       )
	       )

(add-to-list 'org-latex-classes
	       '("beamer"
		"\\documentclass{beamer}
               \\usepackage[fontset=none,UTF8,a4paper,zihao=-4]{ctex}"
	       org-beamer-sectioning)
	       )
	       

(setq org-latex-default-class "ctexart")

(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
       "xelatex -interaction nonstopmode -output-directory %o %f"
       "xelatex -interaction nonstopmode -output-directory %o %f"))
