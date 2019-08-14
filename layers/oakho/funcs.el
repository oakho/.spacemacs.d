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

(defun oakho/goto-line-with-feedback (&optional line)
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive "P")
  (if line
      (goto-line line)
    (if (spacemacs/toggle-line-numbers-status)
        (progn (define-key minibuffer-local-map (kbd "s-l") nil)
               (spacemacs/toggle-line-numbers-off))
      (unwind-protect
          (progn
            (spacemacs/toggle-line-numbers-on)
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

;; (defun oakho/tabbar-buffer-groups () ;; customize to show all normal files in one group
;;   "Return the name of the tab group names the current buffer belongs to.
;; There are two groups: Emacs buffers (those whose name starts with '*', plus
;; dired buffers), and the rest.  This works at least with Emacs v24.2 using
;; tabbar.el v1.7."
;;   (list (cond ((string-match "magit:" (buffer-name)) "user")
;;               ((string-match "magit-process:|*magit-diff:" (buffer-name)) "emacs")
;;               ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
;;               ((string-equal "#" (substring (buffer-name) 0 1)) "erc")
;;               ((eq major-mode 'dired-mode) "emacs")
;;               (t "user"))))

;; https://www.emacswiki.org/emacs/TabBarMode#toc6
(defun oakho/tabbar-buffer-groups () ;; customize to show all normal files in one group
  "Return the list of group names the current buffer belongs to.
   Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
        ;; Check if the major mode derives from `comint-mode' or
        ;; `compilation-mode'.
        (tabbar-buffer-mode-derived-p
         major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ;; ((member (buffer-name)
    ;;          '("*scratch*" "*Messages*" "*Help*"))
    ;;  "Common"
    ;;  )
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Common"
     )
    ((member (buffer-name)
             '("xyz" "day" "m3" "abi" "for" "nws" "eng" "f_g" "tim" "tmp"))
     "Main"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Common"
     )
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail"
     )
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
            ;; Take care of preserving the match-data because this
            ;; function is called when updating the header line.
            (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

(defun make-xpm-bar (color height width)
  "Create an XPM bitmap of a bar."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (create-image
      (concat
       (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
               (length (car data))
               (length data)
               color
               color)
       (apply #'concat
              (cl-loop with idx = 0
                       with len = (length data)
                       for dl in data
                       do (cl-incf idx)
                       collect
                       (concat "\""
                               (cl-loop for d in dl
                                        if (= d 0) collect (string-to-char " ")
                                        else collect (string-to-char "."))
                               (if (eq idx len) "\"};" "\",\n")))))
      'xpm t :ascent 'center))))

(defun oakho/tabbar-display-tab (tab)
  (let ((label (if tabbar--buffer-show-groups
                   (format "[%s]" (tabbar-tab-tabset tab))
                 (format "%s" (tabbar-tab-value tab))))
        (bar-color "#51afef")
        (bar-height 25)
        (bar-width 3)
        (selected-p (eq tab (tabbar-selected-tab (tabbar-current-tabset)))))
    (concat (when (and (display-graphic-p) selected-p)
              (make-xpm-bar bar-color bar-height bar-width))
            " "
            (if tabbar-auto-scroll-flag
                label
              (tabbar-shorten
               label (max 1 (/ (window-width)
                               (length (tabbar-view
                                        (tabbar-current-tabset)))))))
            " ")))

(defun oakho/toggle-quotes ()
  "Toggle single quoted string to double or vice versa, and
  flip the internal quotes as well.  Best to run on the first
  character of the string."
  (interactive)
  (save-excursion
    (re-search-backward "[\"']")
    (let* ((start (point))
           (old-c (char-after start))
           new-c)
      (setq new-c
            (case old-c
              (?\" "'")
              (?\' "\"")))
      (setq old-c (char-to-string old-c))
      (delete-char 1)
      (insert new-c)
      (re-search-forward old-c)
      (backward-char 1)
      (let ((end (point)))
        (delete-char 1)
        (insert new-c)
        (replace-string new-c old-c nil (1+ start) end)))))
