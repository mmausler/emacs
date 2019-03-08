;;; init-preload-local.el --- Init Preload

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

;;; Load org mode early to ensure that the orgmode ELPA version gets picked up, not the
;;; shipped version
(use-package org
  :ensure org-plus-contrib
  :pin org)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package flx-ido)

(use-package web-mode)

(use-package nvm)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(provide 'init-preload-local)

;;; init-preload-local.el ends here
