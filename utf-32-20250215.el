;; -*- coding: utf-8-unix; -*-
;;; utf-32.el --- UTF-32 Support for Emacs

;;; Commentary:
;; UTF-32 support for Emacs
;; 
;; Original code by:
;; http://albinina.sakura.ne.jp/
;;
;; Modifications:
;; - Fixed character encoding and decoding issues.
;; - Corrected buffer positioning and handling of endianness.
;; 
;; Modified by Hideki Saito <hidekis@gmail.com>
;;
;; Installation:
;; (require 'utf-32)
;;

;;; Code:

(defun utf-32-be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char (point-min))
  (insert #x0000 #xFEFF) ;; UTF-32 BE BOM
  (while (not (eobp))
    (let ((c (following-char)))
      (delete-char 1)
      (insert (logand (lsh c -16) #xFFFF) (logand c #xFFFF)))))

(defun utf-32-be-unix-post-read-conversion (len)
  (goto-char (point-min))
  (when (and (not (eobp)) (= (following-char) #x0000))
    (delete-char 1)
    (when (and (not (eobp)) (= (following-char) #xFEFF))
      (delete-char 1)))
  (while (not (eobp))
    (let ((hi (following-char)) lo)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (insert (logior (lsh hi 16) lo))))
  (goto-char (point-min))
  len)

(defun utf-32-le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char (point-min))
  (insert #xFEFF #x0000) ;; UTF-32 LE BOM
  (while (not (eobp))
    (let ((c (following-char)))
      (delete-char 1)
      (insert (logand c #xFFFF) (logand (lsh c -16) #xFFFF)))))

(defun utf-32-le-unix-post-read-conversion (len)
  (goto-char (point-min))
  (when (and (not (eobp)) (= (following-char) #xFEFF))
    (delete-char 1)
    (when (and (not (eobp)) (= (following-char) #x0000))
      (delete-char 1)))
  (while (not (eobp))
    (let ((lo (following-char)) hi)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (insert (logior (lsh hi 16) lo))))
  (goto-char (point-min))
  len)

(defun utf-32be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char (point-min))
  (while (not (eobp))
    (let ((c (following-char)))
      (delete-char 1)
      (insert (logand (lsh c -16) #xFFFF) (logand c #xFFFF)))))

(defun utf-32be-unix-post-read-conversion (len)
  (goto-char (point-min))
  (while (not (eobp))
    (let ((hi (following-char)) lo)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (insert (logior (lsh hi 16) lo))))
  (goto-char (point-min))
  len)

(defun utf-32le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char (point-min))
  (while (not (eobp))
    (let ((c (following-char)))
      (delete-char 1)
      (insert (logand c #xFFFF) (logand (lsh c -16) #xFFFF)))))

(defun utf-32le-unix-post-read-conversion (len)
  (goto-char (point-min))
  (while (not (eobp))
    (let ((lo (following-char)) hi)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (insert (logior (lsh hi 16) lo))))
  (goto-char (point-min))
  len)

;;;###autoload
(define-coding-system 'utf-32be-with-signature-unix
  "UTF-32 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-32-be-unix-pre-write-conversion
  :post-read-conversion 'utf-32-be-unix-post-read-conversion)

;;;###autoload
(define-coding-system 'utf-32le-with-signature-unix
  "UTF-32 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-32-le-unix-pre-write-conversion
  :post-read-conversion 'utf-32-le-unix-post-read-conversion)

;;;###autoload
(define-coding-system 'utf-32be-unix
  "UTF-32 (big endian) without BOM"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-32be-unix-pre-write-conversion
  :post-read-conversion 'utf-32be-unix-post-read-conversion)

;;;###autoload
(define-coding-system 'utf-32le-unix
  "UTF-32 (little endian) without BOM"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-32le-unix-pre-write-conversion
  :post-read-conversion 'utf-32le-unix-post-read-conversion)

;;;###autoload
(define-coding-system 'utf-32be-with-signature-dos
  "UTF-32 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'dos
  :charset-list '(unicode)
  :pre-write-conversion 'utf-32-be-unix-pre-write-conversion
  :post-read-conversion 'utf-32-be-unix-post-read-conversion)

;;;###autoload
(define-coding-system 'utf-32le-with-signature-dos
  "UTF-32 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'dos
  :charset-list '(unicode)
  :pre-write-conversion 'utf-32-le-unix-pre-write-conversion
  :post-read-conversion 'utf-32-le-unix-post-read-conversion)

;;;###autoload
(define-coding-system 'utf-32be-dos
  "UTF-32 (big endian) without BOM"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'dos
  :charset-list '(unicode)
  :pre-write-conversion 'utf-32be-unix-pre-write-conversion
  :post-read-conversion 'utf-32be-unix-post-read-conversion)

;;;###autoload
(define-coding-system 'utf-32le-dos
  "UTF-32 (little endian) without BOM"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'dos
  :charset-list '(unicode)
  :pre-write-conversion 'utf-32le-unix-pre-write-conversion
  :post-read-conversion 'utf-32le-unix-post-read-conversion)

;;;###autoload
(define-coding-system 'utf-32be-with-signature-mac
  "UTF-32 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-32-be-unix-pre-write-conversion
  :post-read-conversion 'utf-32-be-unix-post-read-conversion)

;;;###autoload
(define-coding-system 'utf-32le-with-signature-mac
  "UTF-32 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-32-le-unix-pre-write-conversion
  :post-read-conversion 'utf-32-le-unix-post-read-conversion)

;;;###autoload
(define-coding-system 'utf-32be-mac
  "UTF-32 (big endian) without BOM"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-32be-unix-pre-write-conversion
  :post-read-conversion 'utf-32be-unix-post-read-conversion)

;;;###autoload
(define-coding-system 'utf-32le-mac
  "UTF-32 (little endian) without BOM"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-32le-unix-pre-write-conversion
  :post-read-conversion 'utf-32le-unix-post-read-conversion)

(provide 'utf-32)

;;; utf-32.el ends here
