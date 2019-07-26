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
(define-key oakho-minor-mode-map (kbd "M-ç") 'previous-multiframe-window)
(define-key oakho-minor-mode-map (kbd "M-à") 'next-multiframe-window)
(define-key oakho-minor-mode-map (kbd "M-&") 'delete-other-windows)
(define-key oakho-minor-mode-map (kbd "M-é") 'split-window-below)
(define-key oakho-minor-mode-map (kbd "M-\"") 'split-window-right)

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
(key-chord-define-global "AA" 'align-regexp)
(key-chord-define-global "aa" (lambda () (interactive)
                                (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)  =" 1 1 nil)))


;; Go to line (with Feedback)
(define-key oakho-minor-mode-map (kbd "s-l") 'oakho/goto-line-with-feedback)
(define-key oakho-minor-mode-map (kbd "<C-s-268632076>") 'spacemacs/toggle-line-numbers)

;; Indentation
(define-key oakho-minor-mode-map (kbd "<backtab>") 'oakho/unindent-region)

;; Smart Beginning of Line
(define-key oakho-minor-mode-map (kbd "C-a") 'oakho/smart-beginning-of-line)

;; Expand Region
(define-key oakho-minor-mode-map (kbd "C-@") 'er/expand-region)
(define-key oakho-minor-mode-map (kbd "C-#") 'er/contract-region)

(define-key oakho-minor-mode-map (kbd "M-;") 'comment-or-uncomment-region)

;; OSX sugar https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/layers/%2Bos/osx/keybindings.el
(global-set-key (kbd "s-=") 'spacemacs/scale-up-font)
(global-set-key (kbd "s--") 'spacemacs/scale-down-font)
(global-set-key (kbd "s-0") 'spacemacs/reset-font-size)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'evil-yank)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "s-W") 'delete-frame)
(global-set-key (kbd "s-n") 'make-frame)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-s")
                (lambda ()
                  (interactive)
                  (call-interactively (key-binding "\C-x\C-s"))))
(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "C-s-f") 'spacemacs/toggle-frame-fullscreen)
;; Emacs sometimes registers C-s-f as this weird keycode
(global-set-key (kbd "<C-s-268632070>") 'spacemacs/toggle-frame-fullscreen)

(global-set-key (kbd "s-d") 'mc/mark-next-like-this)
(global-set-key (kbd "s-D") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-D") 'mc/mark-all-like-this)
