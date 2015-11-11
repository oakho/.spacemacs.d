;;; config.el --- oakho Layer packages File for Spacemacs
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

(defvar oakho-minor-mode-map (make-keymap) "My minor mode keymap.")

(define-minor-mode oakho-minor-mode
  "A minor for all kind of customization"
  t " Oakho" 'oakho-minor-mode-map)

(add-hook 'text-mode-hook 'oakho-minor-mode t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tabbar-default ((t (:background "gray20" :foregroun@d "gray20" :box (:line-width 1 :color "gray20" :style nil)))))
 '(tabbar-unselected ((t (:background "gray30"  :foreground "white" :box (:line-width 1 :color "gray30" :style nil)))))
 '(tabbar-selected ((t (:background "gray75" :foreground "black" :box (:line-width 5 :color "gray75" :style nil)))))
 '(tabbar-highlight ((t (:background "white" :foreground "black" :underline nil :box (:line-width 5 :color "white" :style nil)))))
 '(tabbar-button ((t (:box (:line-width 1 :color "gray20" :style nil)))))
 '(tabbar-separator ((t (:background "#2e3434" :height 0.6))))
 )
