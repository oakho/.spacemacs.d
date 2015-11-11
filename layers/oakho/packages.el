;;; packages.el --- oakho Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq oakho-packages
    '(
      helm
      tabbar
      magit
      ))

;; List of packages to exclude.
(setq oakho-excluded-packages '())

;; Helm
(defun oakho/pre-init-helm ()
  (use-package helm
    :config
    (progn
      (define-key oakho-minor-mode-map (kbd "C-x C-f") 'helm-find-files)
      (define-key oakho-minor-mode-map (kbd "C-x C-g") 'helm-mini)
      (define-key oakho-minor-mode-map (kbd "C-x C-b") 'helm-buffers-list)
      (define-key oakho-minor-mode-map (kbd "s-F") 'helm-projectile-ag)
      (define-key oakho-minor-mode-map (kbd "s-p") 'helm-projectile-find-file)

      (setq helm-recentf-fuzzy-match t)
      (setq helm-buffers-fuzzy-matching t)
      (setq helm-recentf-fuzzy-match t)
      (setq helm-M-x-fuzzy-match t)
      (setq helm-semantic-fuzzy-match t)
      (setq helm-imenu-fuzzy-match t)
      (setq helm-apropos-fuzzy-match t)
      (setq helm-lisp-fuzzy-completion t)

      (setq helm-autoresize-max-height 40)
      (setq helm-autoresize-min-height 5)
      (helm-autoresize-mode t))))

;; Tabbar
(defun oakho/init-tabbar ()
  (use-package tabbar
    :config
    (progn
      (define-key oakho-minor-mode-map (kbd "C-à") 'tabbar-forward-tab)
      (define-key oakho-minor-mode-map (kbd "C-ç") 'tabbar-backward-tab)

      (setq tabbar-use-images nil)
      (setq tabbar-buffer-groups-function 'oakho/tabbar-buffer-groups)
      ;; Change padding of the tabs
      ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
      (setq tabbar-separator '(0.5))

      (defcustom tabbar-hide-header-button t
        "Hide header button at left-up corner. Default is t."
        :type 'boolean
        :set (lambda (symbol value)
               (set symbol value)
               (if value
                   (setq
                    tabbar-scroll-left-help-function nil ;don't show help information
                    tabbar-scroll-right-help-function nil
                    tabbar-help-on-tab-function nil
                    tabbar-home-help-function nil
                    tabbar-buffer-home-button (quote (("") "")) ;don't show tabbar button
                    tabbar-scroll-left-button (quote (("") ""))
                    tabbar-scroll-right-button (quote (("") "")))))
        :group 'tabbar)

      (defun tabbar-buffer-tab-label (tab)
        "Return a label for TAB.
That is, a string used to represent it on the tab bar."
        (let ((label (if tabbar--buffer-show-groups
                         (format " [%s] " (tabbar-tab-tabset tab))
                       (format " %s " (tabbar-tab-value tab)))))
          ;; Unless the tab bar auto scrolls to keep the selected tab
          ;; visible, shorten the tab label to keep as many tabs as possible
          ;; in the visible area of the tab bar.
          (if tabbar-auto-scroll-flag
              label
            (tabbar-shorten
             label (max 1 (/ (window-width)
                             (length (tabbar-view
                                      (tabbar-current-tabset)))))))))

      (tabbar-mode t))))

;; ERC
(defun oakho/erc-mode-hook ()
  (tabbar-local-mode))

(defun oakho/pre-init-erc ()
  (use-package erc
    :init
    (add-hook 'erc-mode-hook 'oakho/erc-mode-hook)))

;; Magit
(defun oakho/pre-init-magit ()
  (use-package magit
    :init
    (setq git-magit-status-fullscreen t)))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
