;;; gtranslate.el --- Use google translate api to perform translations

;; Copyright (C) 2009 Bruno Tavernier

;; Author: Bruno Tavernier <tavernier.bruno@gmail.com>
;; Version: 0.2
;; Keywords: words, translation, language, googleapi

;; This file is NOT part of Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;
;; This utility allows for translation via the google translation api.
;;
;; By using the online translation service, you are bound by its Terms.
;; For more information, see:
;; http://code.google.com/apis/ajaxlanguage/terms.html
;;
;; Tested ONLY with: GNU Emacs 23.x and UTF-8 environment.
;;
;; This module was inspired by text-translator.el
;;    http://www.emacswiki.org/emacs/TextTranslator
;;
;; For a more interactive usage, or to use others translation services,
;; consider using babel.el
;;    git clone git://github.com/juergenhoetzel/babel.git


;; Installation
;; ============
;;
;; To install `gtranslate', make sure this file is saved in a directory in
;; your `load-path', and add the line:
;;
;;   (require 'gtranslate)
;; 
;; to your .emacs file.
;;
;; Or use the autoload mechanism instead
;;
;;   (autoload 'gtranslate-translate "gtranslate" nil t)
;;   (autoload 'gtranslate-translate-auto "gtranslate" nil t)
;; 
;; Then create a function for a one way translation, ex: English -> French
;;
;;   (defun my-en-fr ()
;;     (interactive)
;;     (gtranslate-translate (gtranslate-region-or-input) "en" "fr"))
;;
;; Alternatively you can define a function that work automatically
;; in two directions.
;; Note that it only works for a Roman alphabet / Non-Roman alphabet
;; pair of language, ex: French <-> Japanese.
;;
;;   (defun my-fr-ja ()
;;     (interactive)
;;     (gtranslate-translate-auto (gtranslate-region-or-input) "fr" "ja"))
;;
;; Finally you can assign shortcuts to your functions
;;
;;   (global-set-key "\M-1" 'my-fr-ja)
;;   (global-set-key "\M-2" 'my-en-fr)
;;
;; Note: Feel free to replace `gtranslate-region-or-input' in the example
;; above by any function of your choice that return a string.


;;; Change Log:
;;
;; Version 0.2
;; * cleaner code and bug fixes
;;
;; Version 0.1
;; * initial release

;;; Code:

;;; =====================================================================
;;;              Global variables and customization options

; User Variables
(defcustom gtranslate-buffer "*translated*"
  "Buffer name that displays translation result."
  :group 'gtranslate
  :type 'string)

(defcustom gtranslate-service-port 80
  "Port number of the service used for translation."
  :group 'gtranslate
  :type 'integer)

; Variables
(defvar gtranslate-user-agent "Emacs"
  "User agent displayed.")

; Constants
(defconst gtranslate-version "0.2"
  "version number of this version of gtranslate.")

(defconst gtranslate-service "ajax.googleapis.com"
  "Service to use for translation.")

(defconst gtranslate-special-chars
  '(("\\u0026lt;" . "<")
    ("\\u0026gt;" . ">")
    ("\\u0026#39;" . "'")
    ("\\u003d" . "=")
    ("\\u0026quot;" . "\"")
    ("\\u0026amp;" . "&")
    ("\\\\" . "")) ; should be last to remove remaining "\"
     "Characters returned escaped by the google api.")

;;; =====================================================================
;;;              Core functions

(defun gtranslate-make-url (text from to)
  "Generate the url to send to the translation service."
  (concat "v=1.0"
	  "&q=" (url-hexify-string text)
	  "&langpair=" (format "%s|%s" from to)))

(defun gtranslate-correct-special-chars (text)
  "Convert back the escaped characters returned by the api."
  (interactive)
  (let ((corr-text text))
	(mapcar (lambda (x)
		  (setq corr-text (replace-regexp-in-string (car x) (cdr x) corr-text)))
		gtranslate-special-chars)
	(concat corr-text)))

(defun gtranslate-filter (proc str)
  "Remove the cruft from the service answer."
  (if (string-match "414 Request-URI Too Large" str)
      (progn (kill-buffer (process-buffer proc))
	     (process-exit-status proc)
	     (message "Text too long"))
    (string-match "translatedText\":\"\\(\.*\\)\"\}" str)
    (with-current-buffer (process-buffer proc)
      (erase-buffer)
      (insert (gtranslate-correct-special-chars (match-string 1 str))))))

(defun gtranslate-display (proc event)
  "Display translation information."
  (message "Translating...")
  (when (eq (process-status proc) 'closed)
    (let ((original-split-width-threshold split-width-threshold)
	  (window (get-buffer-window gtranslate-buffer)))
      (save-selected-window
	(setq split-width-threshold nil) ; Split window horizontally
	(pop-to-buffer gtranslate-buffer)
	(setq split-width-threshold original-split-width-threshold) ; Restore setting
	(shrink-window-if-larger-than-buffer window)
	(message "Translation done")))))

(defun gtranslate-translate (text from to)
  "Translate 'text' from language 'from' to language 'to'"
  (get-buffer-create gtranslate-buffer)
  (let ((proc (open-network-stream "translation" gtranslate-buffer gtranslate-service gtranslate-service-port))
	(str (gtranslate-make-url text from to)))
    (set-process-filter proc 'gtranslate-filter)
    (process-send-string proc (concat
			       "GET /ajax/services/language/translate?" str " HTTP/1.1\r\n"
			       "Accept-Encoding: identity\r\n"
			       "Host: " gtranslate-service "\r\n"
			       "Connection: close\r\n"
			       "User-Agent: " gtranslate-user-agent "\r\n" "\r\n"
			       ))
    (set-process-sentinel proc 'gtranslate-display)))

(defun gtranslate-translate-auto (text roman nonroman)
  "Choose automatically which translation to perform between one Roman alphabet and a non-roman alphabet language.
   Alphabet ration is 40%."
  (if (> (/ (* (length (replace-regexp-in-string "[^A-Za-z]+" "" text)) 100) (length text)) 40)
      (gtranslate-translate text roman nonroman)
    (gtranslate-translate text nonroman roman)))

;;; =====================================================================
;;;              Return string function

(defun gtranslate-region-or-input ()
  "Select region if active or ask user input for translation."
  (if (not mark-active)
      (read-string "Text to translate: ")
    (buffer-substring (region-beginning) (region-end))))

(provide 'gtranslate)

;;; gtranslate.el ends here.
