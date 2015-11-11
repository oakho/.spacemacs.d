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

(spacemacs|add-toggle oakho
  :status oakho-minor-mode
  :on (oakho-minor-mode)
  :off (oakho-minor-mode -1)
  :documentation "Toggle Oakho minor mode"
  :evil-leader "to")

(spacemacs|diminish oakho-minor-mode " ⓞ" " o")
