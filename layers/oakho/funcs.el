;;; funcs.el --- oakho Layer funcs File for Spacemacs
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

(defun oakho/switch-to-previous-buffer ()
  "Switch to the previous visited buffer"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Go to line with feedback
;; TODO: Find an elegant way to handle cancellation inside minibuffer
(defun oakho/goto-line-with-feedback (&optional line)
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive "P")
  (if line
      (goto-line line)
    (if linum-mode
        (progn (define-key minibuffer-local-map (kbd "s-l") nil)
               (linum-mode -1))
      (unwind-protect
          (progn
            (linum-mode 1)
            (define-key minibuffer-local-map (kbd "s-l") 'top-level)
            (goto-line (read-number "Goto line: " (line-number-at-pos))))
        (oakho/goto-line-with-feedback)))))

(defun oakho/unindent-region ()
  (interactive)
  (indent-region (region-beginning) (region-end) -1))

(defun oakho/switch-to-erc (&optional n)
  (interactive)
  (unless n
    (setq n 1))
  (let ((buffers (and (fboundp 'erc-buffer-list)
                      (erc-buffer-list))))
    (switch-to-buffer
     (if (< n 0)
         (nth (+ (length buffers) n)
              buffers)
       (bury-buffer)
       (nth n buffers)))))

(defun oakho/erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer."
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?

      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "oakho" :full-name "bar"))))

(defun oakho/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
   Move point to the first non-whitespace character on this line.
   If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun oakho/back-to-other-window (&optional kill-current-buffer)
  "Switch to other window and go back to a single frame layout."
  (interactive)
  (if kill-current-buffer (kill-this-buffer))
  (other-window -1)
  (delete-other-windows))

(defun oakho/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun oakho/bells ()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll down up next-line previous-line
                                backward-char forward-char))
    (let ((bleep-sounds '("Bell-1.wav" "Bell-2.wav" "Bell-3.wav" "Bell-4.wav" "Bell-5.wav")))
      (call-process "afplay" nil 0 nil
                    (concat  "sounds/" (nth (random (length bleep-sounds)) bleep-sounds))))))

(defun oakho/tabbar-buffer-groups () ;; customize to show all normal files in one group
  "Return the name of the tab group names the current buffer belongs to.
There are two groups: Emacs buffers (those whose name starts with '*', plus
dired buffers), and the rest.  This works at least with Emacs v24.2 using
tabbar.el v1.7."
  (list (cond ((string-equal "*magit" (substring (buffer-name) 0 6)) "user")
              ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
              ((string-equal "#" (substring (buffer-name) 0 1)) "erc")
              ((eq major-mode 'dired-mode) "emacs")
              (t "user"))))
