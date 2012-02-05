; open a *scratch* buffer at startup
(setq inhibit-startup-screen t)

; show line numbers always
(global-linum-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; org-mode                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-install)

; associate file extensions with org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

; mappings for org-mode
;(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)
