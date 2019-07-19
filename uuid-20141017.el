;; -*- coding: utf-8-unix; -*-
;;; uuid.el ---
;; UUID Generation Script

;; Based on http://ergoemacs.org/emacs/elisp_generate_uuid.html
;; Some function names are updated to fit my preference.

;;; Code:

;;;###autoload
(defun insert-uuid ()
  "Insert a random universally unique identifier (UUID).
Uses uuidgen command."
  (interactive)
  (shell-command "uuidgen" t))

(random t)

;; by Christopher Wellons. 2011-11-18. Editted by Xah Lee.
;; Edited by Hideki Saito further to generate all valid variants for "N"
;; in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format. 
;;;###autoload
(defun insert-uuid-internal ()
  "Insert a UUID. This uses a simple hashing of variable data."
  (interactive)
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
			    (user-uid)
			    (emacs-pid)
			    (system-name)
			    (user-full-name)
			    (current-time)
			    (emacs-uptime)
			    (garbage-collect)
			    (buffer-string)
			    (random)
			    (recent-keys)))))
    
    (insert (format "%s-%s-4%s-%s%s-%s"
                    (substring myStr 0 8)
                    (substring myStr 8 12)
                    (substring myStr 13 16)
		    (format "%x" (+ 8 (random 4)))
                    (substring myStr 17 20)
                    (substring myStr 20 32)))))


(provide 'uuid)

;;; uuid.el ends here
