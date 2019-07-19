;; -*- coding: utf-8-unix; -*-
;; UTF-64, UTF-128, UTF-384, UTF-512 support for Emacs (Joke)
;;
;; Based on the code by:
;; http://albinina.sakura.ne.jp/
;;
;; Modifications:
;; - Changed name of each coding system so it is consistent with the rest.
;; 
;; Modified by Hideki Saito <hidekis@gmail.com>

;; UTF-64

;;
;; With BOM
;;

(defun utf-64-be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #x0000#x0000#x0000)
  (insert #xFEFF)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000)
        (forward-char)))))

(defun utf-64-be-unix-post-read-conversion (len)
  (if (zerop (following-char))
      (progn
        (forward-char)
        (if (= (following-char) #xFEFF)
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-64-be-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #x0000#x0000#x0000)
  (insert #xFEFF)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x0000#x0000#x0000)
          (insert #x000D)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000)
        (forward-char)))))

(defun utf-64-be-dos-post-read-conversion (len)
  (if (zerop (following-char))
      (progn
        (forward-char)
        (if (= (following-char) #xFEFF)
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

(defun utf-64-le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #xFEFF)
  (insert #x0000#x0000#x0000)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000)))))

(defun utf-64-le-unix-post-read-conversion (len)
  (if (= (following-char) #xFEFF)
      (progn
        (forward-char)
        (if (zerop (following-char))
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-64-le-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #xFEFF)
  (insert #x0000#x0000#x0000)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x000D)
          (insert #x0000#x0000#x0000)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000)))))

(defun utf-64-le-dos-post-read-conversion (len)
  (if (= (following-char) #xFEFF)
      (progn
        (forward-char)
        (if (zerop (following-char))
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

;; Without BOM

(defun utf-64be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000)
        (forward-char)))))

(defun utf-64be-unix-post-read-conversion (len)
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-64be-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x0000#x0000#x0000)
          (insert #x000D)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000)
        (forward-char)))))

(defun utf-64be-dos-post-read-conversion (len)
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

(defun utf-64le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000)))))

(defun utf-64le-unix-post-read-conversion (len)
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-64le-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x000D)
          (insert #x0000#x0000#x0000)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000)))))

(defun utf-64le-dos-post-read-conversion (len)
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

;; With BOM

(define-coding-system 'utf-64be-with-signature-unix
  "UTF-64 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-64-be-unix-pre-write-conversion
  :post-read-conversion 'utf-64-be-unix-post-read-conversion)

(define-coding-system 'utf-64be-with-signature-dos
  "UTF-64 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-64-be-dos-pre-write-conversion
  :post-read-conversion 'utf-64-be-dos-post-read-conversion)

(define-coding-system 'utf-64be-with-signature-mac
  "UTF-64 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-64-be-unix-pre-write-conversion
  :post-read-conversion 'utf-64-be-unix-post-read-conversion)

(define-coding-system 'utf-64le-with-signature-unix
  "UTF-64 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-64-le-unix-pre-write-conversion
  :post-read-conversion 'utf-64-le-unix-post-read-conversion)

(define-coding-system 'utf-64le-with-signature-dos
  "UTF-64 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-64-le-dos-pre-write-conversion
  :post-read-conversion 'utf-64-le-dos-post-read-conversion)

(define-coding-system 'utf-64le-with-signature-mac
  "UTF-64 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-64-le-unix-pre-write-conversion
  :post-read-conversion 'utf-64-le-unix-post-read-conversion)

;; Without BOM

(define-coding-system 'utf-64be-unix
  "UTF-64 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-64be-unix-pre-write-conversion
  :post-read-conversion 'utf-64be-unix-post-read-conversion)

(define-coding-system 'utf-64be-dos
  "UTF-64 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-64be-dos-pre-write-conversion
  :post-read-conversion 'utf-64be-dos-post-read-conversion)

(define-coding-system 'utf-64be-mac
  "UTF-64 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-64be-unix-pre-write-conversion
  :post-read-conversion 'utf-64be-unix-post-read-conversion)

(define-coding-system 'utf-64le-unix
  "UTF-64 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-64le-unix-pre-write-conversion
  :post-read-conversion 'utf-64le-unix-post-read-conversion)

(define-coding-system 'utf-64le-dos
  "UTF-64 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-64le-dos-pre-write-conversion
  :post-read-conversion 'utf-64le-dos-post-read-conversion)

(define-coding-system 'utf-64le-mac
  "UTF-64 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-64le-unix-pre-write-conversion
  :post-read-conversion 'utf-64le-unix-post-read-conversion)

;; UTF-128

;;
;; With BOM
;;

(defun utf-128-be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (insert #xFEFF)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-128-be-unix-post-read-conversion (len)
  (if (zerop (following-char))
      (progn
        (forward-char)
        (if (= (following-char) #xFEFF)
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-128-be-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (insert #xFEFF)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)
          (insert #x000D)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-128-be-dos-post-read-conversion (len)
  (if (zerop (following-char))
      (progn
        (forward-char)
        (if (= (following-char) #xFEFF)
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

(defun utf-128-le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #xFEFF)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-128-le-unix-post-read-conversion (len)
  (if (= (following-char) #xFEFF)
      (progn
        (forward-char)
        (if (zerop (following-char))
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-128-le-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #xFEFF)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x000D)
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-128-le-dos-post-read-conversion (len)
  (if (= (following-char) #xFEFF)
      (progn
        (forward-char)
        (if (zerop (following-char))
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

;; Without BOM

(defun utf-128be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-128be-unix-post-read-conversion (len)
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-128be-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)
          (insert #x000D)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-128be-dos-post-read-conversion (len)
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

(defun utf-128le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-128le-unix-post-read-conversion (len)
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-128le-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x000D)
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-128le-dos-post-read-conversion (len)
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

;; With BOM

(define-coding-system 'utf-128be-with-signature-unix
  "UTF-128 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-128-be-unix-pre-write-conversion
  :post-read-conversion 'utf-128-be-unix-post-read-conversion)

(define-coding-system 'utf-128be-with-signature-dos
  "UTF-128 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-128-be-dos-pre-write-conversion
  :post-read-conversion 'utf-128-be-dos-post-read-conversion)

(define-coding-system 'utf-128be-with-signature-mac
  "UTF-128 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-128-be-unix-pre-write-conversion
  :post-read-conversion 'utf-128-be-unix-post-read-conversion)

(define-coding-system 'utf-128le-with-signature-unix
  "UTF-128 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-128-le-unix-pre-write-conversion
  :post-read-conversion 'utf-128-le-unix-post-read-conversion)

(define-coding-system 'utf-128le-with-signature-dos
  "UTF-128 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-128-le-dos-pre-write-conversion
  :post-read-conversion 'utf-128-le-dos-post-read-conversion)

(define-coding-system 'utf-128le-with-signature-mac
  "UTF-128 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-128-le-unix-pre-write-conversion
  :post-read-conversion 'utf-128-le-unix-post-read-conversion)

;; Without BOM

(define-coding-system 'utf-128be-unix
  "UTF-128 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-128be-unix-pre-write-conversion
  :post-read-conversion 'utf-128be-unix-post-read-conversion)

(define-coding-system 'utf-128be-dos
  "UTF-128 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-128be-dos-pre-write-conversion
  :post-read-conversion 'utf-128be-dos-post-read-conversion)

(define-coding-system 'utf-128be-mac
  "UTF-128 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-128be-unix-pre-write-conversion
  :post-read-conversion 'utf-128be-unix-post-read-conversion)

(define-coding-system 'utf-128le-unix
  "UTF-128 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-128le-unix-pre-write-conversion
  :post-read-conversion 'utf-128le-unix-post-read-conversion)

(define-coding-system 'utf-128le-dos
  "UTF-128 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-128le-dos-pre-write-conversion
  :post-read-conversion 'utf-128le-dos-post-read-conversion)

(define-coding-system 'utf-128le-mac
  "UTF-128 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-128le-unix-pre-write-conversion
  :post-read-conversion 'utf-128le-unix-post-read-conversion)

;; UTF-256

;;
;; With BOM
;;

(defun utf-256-be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (insert #xFEFF)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-256-be-unix-post-read-conversion (len)
  (if (zerop (following-char))
      (progn
        (forward-char)
        (if (= (following-char) #xFEFF)
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-256-be-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (insert #xFEFF)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
          (insert #x000D)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-256-be-dos-post-read-conversion (len)
  (if (zerop (following-char))
      (progn
        (forward-char)
        (if (= (following-char) #xFEFF)
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

(defun utf-256-le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #xFEFF)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-256-le-unix-post-read-conversion (len)
  (if (= (following-char) #xFEFF)
      (progn
        (forward-char)
        (if (zerop (following-char))
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-256-le-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #xFEFF)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x000D)
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-256-le-dos-post-read-conversion (len)
  (if (= (following-char) #xFEFF)
      (progn
        (forward-char)
        (if (zerop (following-char))
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

;; Without BOM

(defun utf-256be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-256be-unix-post-read-conversion (len)
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-256be-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
          (insert #x000D)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-256be-dos-post-read-conversion (len)
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

(defun utf-256le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-256le-unix-post-read-conversion (len)
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-256le-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x000D)
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-256le-dos-post-read-conversion (len)
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

;; With BOM

(define-coding-system 'utf-256be-with-signature-unix
  "UTF-256 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-256-be-unix-pre-write-conversion
  :post-read-conversion 'utf-256-be-unix-post-read-conversion)

(define-coding-system 'utf-256be-with-signature-dos
  "UTF-256 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-256-be-dos-pre-write-conversion
  :post-read-conversion 'utf-256-be-dos-post-read-conversion)

(define-coding-system 'utf-256be-with-signature-mac
  "UTF-256 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-256-be-unix-pre-write-conversion
  :post-read-conversion 'utf-256-be-unix-post-read-conversion)

(define-coding-system 'utf-256le-with-signature-unix
  "UTF-256 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-256-le-unix-pre-write-conversion
  :post-read-conversion 'utf-256-le-unix-post-read-conversion)

(define-coding-system 'utf-256le-with-signature-dos
  "UTF-256 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-256-le-dos-pre-write-conversion
  :post-read-conversion 'utf-256-le-dos-post-read-conversion)

(define-coding-system 'utf-256le-with-signature-mac
  "UTF-256 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-256-le-unix-pre-write-conversion
  :post-read-conversion 'utf-256-le-unix-post-read-conversion)

;; Without BOM

(define-coding-system 'utf-256be-unix
  "UTF-256 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-256be-unix-pre-write-conversion
  :post-read-conversion 'utf-256be-unix-post-read-conversion)

(define-coding-system 'utf-256be-dos
  "UTF-256 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-256be-dos-pre-write-conversion
  :post-read-conversion 'utf-256be-dos-post-read-conversion)

(define-coding-system 'utf-256be-mac
  "UTF-256 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-256be-unix-pre-write-conversion
  :post-read-conversion 'utf-256be-unix-post-read-conversion)

(define-coding-system 'utf-256le-unix
  "UTF-256 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-256le-unix-pre-write-conversion
  :post-read-conversion 'utf-256le-unix-post-read-conversion)

(define-coding-system 'utf-256le-dos
  "UTF-256 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-256le-dos-pre-write-conversion
  :post-read-conversion 'utf-256le-dos-post-read-conversion)

(define-coding-system 'utf-256le-mac
  "UTF-256 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-256le-unix-pre-write-conversion
  :post-read-conversion 'utf-256le-unix-post-read-conversion)

;; UTF-384

;;
;; With BOM
;;


(defun utf-384-be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (insert #xFEFF)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-384-be-unix-post-read-conversion (len)
  (if (zerop (following-char))
      (progn
        (forward-char)
        (if (= (following-char) #xFEFF)
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-384-be-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (insert #xFEFF)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
          (insert #x000D)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-384-be-dos-post-read-conversion (len)
  (if (zerop (following-char))
      (progn
        (forward-char)
        (if (= (following-char) #xFEFF)
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

(defun utf-384-le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #xFEFF)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-384-le-unix-post-read-conversion (len)
  (if (= (following-char) #xFEFF)
      (progn
        (forward-char)
        (if (zerop (following-char))
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-384-le-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #xFEFF)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x000D)
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-384-le-dos-post-read-conversion (len)
  (if (= (following-char) #xFEFF)
      (progn
        (forward-char)
        (if (zerop (following-char))
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

;; Without BOM

(defun utf-384be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-384be-unix-post-read-conversion (len)
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-384be-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
          (insert #x000D)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-384be-dos-post-read-conversion (len)
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

(defun utf-384le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-384le-unix-post-read-conversion (len)
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-384le-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x000D)
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-384le-dos-post-read-conversion (len)
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

;; With BOM

(define-coding-system 'utf-384be-with-signature-unix
  "UTF-384 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384-be-unix-pre-write-conversion
  :post-read-conversion 'utf-384-be-unix-post-read-conversion)

(define-coding-system 'utf-384be-with-signature-dos
  "UTF-384 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384-be-dos-pre-write-conversion
  :post-read-conversion 'utf-384-be-dos-post-read-conversion)

(define-coding-system 'utf-384be-with-signature-mac
  "UTF-384 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384-be-unix-pre-write-conversion
  :post-read-conversion 'utf-384-be-unix-post-read-conversion)

(define-coding-system 'utf-384le-with-signature-unix
  "UTF-384 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384-le-unix-pre-write-conversion
  :post-read-conversion 'utf-384-le-unix-post-read-conversion)

(define-coding-system 'utf-384le-with-signature-dos
  "UTF-384 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384-le-dos-pre-write-conversion
  :post-read-conversion 'utf-384-le-dos-post-read-conversion)

(define-coding-system 'utf-384le-with-signature-mac
  "UTF-384 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384-le-unix-pre-write-conversion
  :post-read-conversion 'utf-384-le-unix-post-read-conversion)

;; Without BOM

(define-coding-system 'utf-384be-unix
  "UTF-384 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384be-unix-pre-write-conversion
  :post-read-conversion 'utf-384be-unix-post-read-conversion)

(define-coding-system 'utf-384be-dos
  "UTF-384 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384be-dos-pre-write-conversion
  :post-read-conversion 'utf-384be-dos-post-read-conversion)

(define-coding-system 'utf-384be-mac
  "UTF-384 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384be-unix-pre-write-conversion
  :post-read-conversion 'utf-384be-unix-post-read-conversion)

(define-coding-system 'utf-384le-unix
  "UTF-384 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384le-unix-pre-write-conversion
  :post-read-conversion 'utf-384le-unix-post-read-conversion)

(define-coding-system 'utf-384le-dos
  "UTF-384 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384le-dos-pre-write-conversion
  :post-read-conversion 'utf-384le-dos-post-read-conversion)

(define-coding-system 'utf-384le-mac
  "UTF-384 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384le-unix-pre-write-conversion
  :post-read-conversion 'utf-384le-unix-post-read-conversion)

;; UTF-512

;;
;; With BOM
;;

(defun utf-512-be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (insert #xFEFF)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-512-be-unix-post-read-conversion (len)
  (if (zerop (following-char))
      (progn
        (forward-char)
        (if (= (following-char) #xFEFF)
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-512-be-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (insert #xFEFF)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
          (insert #x000D)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-512-be-dos-post-read-conversion (len)
  (if (zerop (following-char))
      (progn
        (forward-char)
        (if (= (following-char) #xFEFF)
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

(defun utf-512-le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #xFEFF)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-512-le-unix-post-read-conversion (len)
  (if (= (following-char) #xFEFF)
      (progn
        (forward-char)
        (if (zerop (following-char))
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-512-le-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #xFEFF)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x000D)
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-512-le-dos-post-read-conversion (len)
  (if (= (following-char) #xFEFF)
      (progn
        (forward-char)
        (if (zerop (following-char))
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

;; Without BOM

(defun utf-512be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-512be-unix-post-read-conversion (len)
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-512be-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
          (insert #x000D)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-512be-dos-post-read-conversion (len)
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

(defun utf-512le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-512le-unix-post-read-conversion (len)
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-512le-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x000D)
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-512le-dos-post-read-conversion (len)
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

;; With BOM

(define-coding-system 'utf-512be-with-signature-unix
  "UTF-512 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-512-be-unix-pre-write-conversion
  :post-read-conversion 'utf-512-be-unix-post-read-conversion)

(define-coding-system 'utf-512be-with-signature-dos
  "UTF-512 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-512-be-dos-pre-write-conversion
  :post-read-conversion 'utf-512-be-dos-post-read-conversion)

(define-coding-system 'utf-512be-with-signature-mac
  "UTF-512 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-512-be-unix-pre-write-conversion
  :post-read-conversion 'utf-512-be-unix-post-read-conversion)

(define-coding-system 'utf-512le-with-signature-unix
  "UTF-512 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-512-le-unix-pre-write-conversion
  :post-read-conversion 'utf-512-le-unix-post-read-conversion)

(define-coding-system 'utf-512le-with-signature-dos
  "UTF-512 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-512-le-dos-pre-write-conversion
  :post-read-conversion 'utf-512-le-dos-post-read-conversion)

(define-coding-system 'utf-512le-with-signature-mac
  "UTF-512 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-512-le-unix-pre-write-conversion
  :post-read-conversion 'utf-512-le-unix-post-read-conversion)

;; Without BOM

(define-coding-system 'utf-512be-unix
  "UTF-512 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-512be-unix-pre-write-conversion
  :post-read-conversion 'utf-512be-unix-post-read-conversion)

(define-coding-system 'utf-512be-dos
  "UTF-512 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-512be-dos-pre-write-conversion
  :post-read-conversion 'utf-512be-dos-post-read-conversion)

(define-coding-system 'utf-512be-mac
  "UTF-512 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-512be-unix-pre-write-conversion
  :post-read-conversion 'utf-512be-unix-post-read-conversion)

(define-coding-system 'utf-512le-unix
  "UTF-512 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-512le-unix-pre-write-conversion
  :post-read-conversion 'utf-512le-unix-post-read-conversion)

(define-coding-system 'utf-512le-dos
  "UTF-512 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-512le-dos-pre-write-conversion
  :post-read-conversion 'utf-512le-dos-post-read-conversion)

(define-coding-system 'utf-512le-mac
  "UTF-512 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-512le-unix-pre-write-conversion
  :post-read-conversion 'utf-512le-unix-post-read-conversion)

(provide 'highutf)
