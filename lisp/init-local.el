;;; init-local.el --- Local settings

;;; Commentary:
;; 

;;; Code:

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'")))

(set-default-font "Inconsolata 18")
(setq explicit-shell-file-name "/bin/zsh")
(setq notmuch-search-oldest-first nil)
(setq path-to-ctags "/usr/local/bin/ctags") ;; <- your ctags path here
(setq dired-dwim-target t)

(setq message-kill-buffer-on-exit t)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq send-mail-function 'sendmail-send-it)
(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)
(setq sendmail-program "/usr/local/bin/msmtp")

(defun bash (buffer-name)
  "Start a terminal with BUFFER-NAME."
  (interactive "sbuffer name: ")
  (ansi-term "/bin/bash")
  (rename-buffer buffer-name t))

(defun create-tags (dir-name)
  "Create tags file with argument DIR-NAME."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )


;; read gpg-agent environment
(defun read-env-line (line)
  "Read a env LINE and post to environment."
  (let ((key-value-pair (split-string line "=" t)))
    (setenv (car key-value-pair) (car (last key-value-pair))))
  )
(defvar gpg-agent-info-file)
(setq gpg-agent-info-file (concat (getenv "HOME") "/.gpg-agent-info"))
(when
    (file-exists-p gpg-agent-info-file)
  (with-temp-buffer
    (progn
      (insert-file-contents gpg-agent-info-file)
      (mapc 'read-env-line (split-string (buffer-string) "\n" t)))
    ))

(define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)


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

(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(require 'yarn)

(autoload 'notmuch "notmuch" "notmuch mail" t)

(setq jiralib-url "https://wead2c.atlassian.net")

(provide 'init-local)

;;; init-local.el ends here
