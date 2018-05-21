;;; init-local.el --- Local settings

;;; Commentary:
;; 

;;; Code:

(set-default-font "Inconsolata 18")
(setq evil-want-C-u-scroll t)
(setq explicit-shell-file-name "/bin/zsh")
(setq path-to-ctags "/usr/local/bin/ctags") ;; <- your ctags path here
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(use-package evil)
(use-package flx-ido)

(evil-mode t)

(define-key evil-normal-state-map (kbd "M-.")
  `(menu-item "" evil-repeat-pop :filter
              ,(lambda (cmd) (if (eq last-command 'evil-repeat-pop) cmd))))


(dolist (mode '(git-rebase-mode
                flycheck-error-list-mode
                elm-package-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "RET") nil))


(provide 'init-local)

;;; init-local.el ends here
