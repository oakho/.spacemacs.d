;;; keybindings.el --- oakho Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2014 Oakho
;; Copyright (c) 2014-2015 Oakho
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Buffer Management
(define-key oakho-minor-mode-map (kbd "s-,") 'oakho/switch-to-previous-buffer)
(define-key oakho-minor-mode-map (kbd "s-k") 'kill-this-buffer)
(define-key oakho-minor-mode-map (kbd "C-c k") 'oakho/kill-other-buffers)

;; Window Management
(define-key oakho-minor-mode-map (kbd "s-ç") 'previous-multiframe-window)
(define-key oakho-minor-mode-map (kbd "s-à") 'next-multiframe-window)
(define-key oakho-minor-mode-map (kbd "s-&") 'delete-other-windows)
(define-key oakho-minor-mode-map (kbd "s-é") 'split-window-below)
(define-key oakho-minor-mode-map (kbd "s-\"") 'split-window-right)

;; Begin/End Buffer
(define-key oakho-minor-mode-map (kbd "C-c C-p") 'beginning-of-buffer)
(define-key oakho-minor-mode-map (kbd "C-c C-n") 'end-of-buffer)

;; French Keyboards
(define-key oakho-minor-mode-map (kbd "M-L") "|")
(define-key oakho-minor-mode-map (kbd "M-n") "~")
(define-key oakho-minor-mode-map (kbd "M-/") "\\")
(define-key oakho-minor-mode-map (kbd "M-(") "{")
(define-key oakho-minor-mode-map (kbd "M-5") "[")
(define-key oakho-minor-mode-map (kbd "M-)") "}")
(define-key oakho-minor-mode-map (kbd "M-°") "]")

(define-key oakho-minor-mode-map (kbd "C-c C-a C-r") 'align-regexp)
(define-key oakho-minor-mode-map (kbd "C-c C-a C-a") (lambda () (interactive)
                                                       (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)=" 1 1 nil)))
;; (key-chord-define-global "AA" 'align-regexp)
;; (key-chord-define-global "aa" (lambda () (interactive)
;;                                 (align-regexp (region-beginning) (region-end) "\\(\\s-*\\) =" 1 1 nil)))


;; Go to line with Feedback
(define-key oakho-minor-mode-map (kbd "s-l") 'oakho/goto-line-with-feedback)

;; Indentation
(define-key oakho-minor-mode-map (kbd "<backtab>") 'oakho/unindent-region)

;; Smart Beginning of Line
(define-key oakho-minor-mode-map (kbd "C-a") 'oakho/smart-beginning-of-line)

;; Expand Region
(define-key oakho-minor-mode-map (kbd "C-@") 'er/expand-region)
(define-key oakho-minor-mode-map (kbd "C-#") 'er/contract-region)
