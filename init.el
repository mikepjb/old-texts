;;; emacs-config --- Editor Setup -*- lexical-binding: t -*-

;;; Commentary:

;; This file bootstraps the configuration of Emacs.

;; Additionally here is a quick reference of common things you may want to do:

;; - Running tasks (e.g 'npm test') use M-S-& to run an async-command
;; - Magit:
;;   - ll : opens log view
;;   - bb : checksout current sha in log view
;;   - Fu : pull
;;   - Pu : push

;;; Code:

(setq gc-cons-threshold 32000000
      garbage-collection-messages t)

(defun update ()
  (interactive)
  (message "Copying texts init.el & rollout-theme.el -> ~/.emacs.d")
  (let ((theme-file (concat user-emacs-directory "rollout-theme.el")))
    (unless (file-directory-p user-emacs-directory)
      (make-directory user-emacs-directory))
    (when (file-exists-p theme-file)
      (delete-file theme-file))
      (copy-file "~/src/texts/rollout-theme.el" theme-file)
    (when (file-exists-p user-init-file)
      (delete-file user-init-file))
      (copy-file "~/src/texts/init.el" user-init-file)))

(setq enabled '(ido-mode show-paren-mode electric-pair-mode
                         electric-indent-mode global-auto-revert-mode
                         savehist-mode))
(setq disabled '(tool-bar-mode menu-bar-mode scroll-bar-mode fringe-mode))

(dolist (m enabled) (funcall m 1))
(dolist (m disabled) (funcall m -1))

(setq-default
 split-height-threshold 1200
 split-width-threshold 2000
 indent-tabs-mode nil
 tab-width 2
 kill-whole-line t
 inhibit-splash-screen t
 vc-follow-symlinks t
 column-number-mode t
 load-prefer-newer t
 ring-bell-function 'ignore
 fill-column 80
 show-trailing-whitespace t ;; only for certain modes?
 mac-command-modifier 'meta
 mac-option-modifier 'none
 compilation-ask-about-save nil
 ido-file-extensions-order '(".org" ".clj" ".cljs" ".cljc" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf")
 backup-directory-alist `(("." . ,(concat user-emacs-directory "saves")))
 custom-file (concat user-emacs-directory "custom.el"))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun set-indent (n)
  (setq-local tab-width n)
  (setq-local c-basic-offset n)
  (setq-local javascript-indent-level n)
  ;; (setq-local js-indent-level n)
  ;; (setq-local js2-basic-offset n)
  (setq-local css-indent-offset n))

;; TODO Doesn't work the first time compile is used.
(with-eval-after-load 'compile
  (require 'ansi-color)
  (defun mikepjb/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'mikepjb/colourise-compilation-buffer))

(defmacro ifn (fn)
  `(lambda () (interactive) ,fn))

(defun git-root ()
  "Return current project root or pwd if VCS not found."
  (shell-command-to-string "echo -ne $(git rev-parse --show-toplevel || echo \".\")"))

(defun compile-in-project ()
  (interactive)
  (let ((default-directory (git-root)))
    (call-interactively #'compile)))

(defun run-tests-for-current-file ()
  (interactive)
  (let ((default-directory (git-root)))
    ;; assume mocha/js for now, should expand this later on.
    (compile (concat "./node_modules/mocha/bin/mocha " buffer-file-name))))

(defmacro include (name &rest args)
  `(progn
     (unless (assoc ',name package-archive-contents)
       (package-refresh-contents))
     (unless (package-installed-p ',name)
       (package-install ',name))
     (require ',name)))

(dolist
    (binding
     `(("M-o" . other-window)
       ("C-c i" . ,(ifn (find-file "~/src/texts/init.el")))
       ("C-c n" . ,(ifn (find-file (concat user-emacs-directory "notes.org"))))
       ("C-c l" . ,(ifn (find-file "~/src")))
       ("C-c C-c" . compile-in-project)
       ("C-c g" . magit)
       ("C-c p" . projectile-find-file)
       ("C-c P" . projectile-grep)
       ("C-h" . delete-backward-char)
       ("M-H" . ,help-map)
       ("M-j" . join-line)
       ("C-;" . company-capf)
       ("M-k" . paredit-forward-barf-sexp)
       ("M-l" . paredit-forward-slurp-sexp)
       ("M-RET" . toggle-frame-fullscreen)))
  (global-set-key (kbd (car binding)) (cdr binding)))

(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word
    backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

(add-hook 'ido-setup-hook
	  (lambda ()
	    (define-key ido-completion-map (kbd "C-w") 'backward-kill-word)
	    (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)))

(defun code-config ()
  (if (version<= "26.0.50" emacs-version)
      (display-line-numbers-mode 1)
    (linum-mode 1))
  (set-indent 2))

(defun default-config ()
  (set-indent 2))

(add-hook 'text-mode-hook 'default-config)
(dolist (hook '(prog-mode-hook css-mode-hook)) (add-hook hook 'code-config))

;; third party code -> we might make IO calls, anything after here is expected to fail.
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(include diminish)

(include company)
(add-hook 'after-init-hook 'global-company-mode)
(diminish 'company-mode)

(include markdown-mode)
(include magit)

(include projectile)

(include paredit)

(when (include js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (include nodejs-repl))

(include json-mode)

(when (include flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode)

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-temp-prefix ".flycheck"))

(when (include web-mode)
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode)))

(when (include clojure-mode)
  (include cider))

(when (include paren-face)
  (global-paren-face-mode t)
  (setq paren-face-regexp "[]{}[()]{()}"))

(load-theme 'rollout t)

(grep-compute-defaults)
(grep-apply-setting 'grep-find-template
  (concat grep-find-template " | cut -c 1-200"))

(setq exec-path (append exec-path '("/opt/homebrew/bin/")))

;; (if (eq system-type 'gnu/linux)
;;     (set-frame-font "Inconsolata 14" nil))
(setq map (make-sparse-keymap))
(defun delay-exit()
  "Delay exit to avoid keybindings getting trapped on WSL2."
  (interactive)
  (save-some-buffers)
  (message "Pausing for 600ms.. (thanks WSL2)")
  (sit-for 0.6)
  (kill-emacs))
(global-set-key (kbd "C-x C-c") 'delay-exit)

;;; init.el ends here
