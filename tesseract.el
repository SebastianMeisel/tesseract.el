;;; tesseract.el --- Use Tesseract for OCR from within Emacs.  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2023, by Sebastian Meisel

;; Author: Sebastian Meisel <sebastian.meisel@gmail.com>
;; Version: 0.1
;; Created:  Mai 20, 2023
;; Keywords: ocr
;; Homepage: https://github.com/SebastianMeisel/tesseract.el
;; Package-Requires: ((emacs "27.1"))

;;; License:

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary

;; This is a package that provides support for OCR using Tesseract from within Emacs.
;; Tesseract needs to be installed on your system first to use this package.

;; Put tesseract.el in your load-path and add (require 'tesseract) to your .emacs file.

;;; Code:


;; customization
(defgroup tesseract nil
  "Commands to use Tesseract for OCR from within Emacs."
  :group 'external)

(defcustom tesseract/default-language "eng"
"Default language used for Tesseract OCR. Use tesseract/list-languages to get languages available on your system."
:group 'tesseract)

;; functions

(defun tesseract/list-languages ()
  "List available language packages for tesseract."
  (interactive)
  (split-string (nth 1 (split-string (shell-command-to-string "tesseract --list-langs") ":" nil))))

(setq tesseract/current-language tesseract/default-language)

(defun tesseract/change-language ()
  "Change the language based on the options given by tesseract/list-languages."
  (interactive)
  (let((options (tesseract/list-languages)))
    (setq tesseract/current-language (completing-read "Language:" options nil t "eng" 'tesseract/language-history))))

(defun tesseract/doc-view/ocr-current-page ()
  "Extract text from the current PNG image in DocView mode using Tesseract OCR."
  (interactive)
  (let* ((current-image (plist-get (cdr (image-mode-window-get 'image)) :file))
	(tesseract-language tesseract/current-language)) 
    (with-current-buffer (get-buffer-create "*tesseract*")
      (display-buffer (current-buffer))
      (call-process  "tesseract"
		     nil
		     t
		     t
		     current-image
		     "-"
		     "-l" tesseract-language))))

(defun tesseract/doc-view/ocr-this-pdf ()
  "Extract text from all pages of the PDF open in DocView mode using Tesseract OCR."
  (interactive)
  (let* ((cache-dir (doc-view--current-cache-dir))
	 (current-pdf (directory-files cache-dir nil "png$"))
	(tesseract-language tesseract/current-language)) 
    (with-current-buffer (get-buffer-create "*tesseract*")
      (display-buffer (current-buffer))
      (dolist (current-image current-pdf)
	      (call-process  "tesseract"
			     nil
			     t
			     t
			     (concat cache-dir current-image)
			     "-"
			     "-l" tesseract-language)))))

(defun tesseract/ocr-image (images)
  "Run Tesseract OCR on each image.
  
  IMAGES is a list of paths to the images."
  (let ((tesseract-language tesseract/current-language))
    (dolist (current-image images)
      (call-process  "tesseract"
		     nil
		     nil
		     t
		     current-image
		     (car (split-string current-image "\\.[[:alpha:]]+$" t))
		     "-l" tesseract-language
		     "txt"
		     "quiet"))))

(defun tesseract/ocr-pdf (pdf)
  "Convert all pages of a PDF to images and process them with Tesseract OCR."
  (let* ((tesseract-language tesseract/current-language)
	 (default-directory (make-temp-file "tesseract" t nil)))
    (with-existing-directory
      (call-process "convert"
		    nil
		    "*convert*"
		    t
		    "-density" "300x300"
		    pdf
		    "-density" "300x300"
		    "-colorspace" "RGB"
		    "pdf-pages.png")
      (let ((images (directory-files default-directory nil "png$"))
	    (output-file (concat (car(split-string pdf "pdf$" t)) "txt")))
	(with-temp-buffer
	  (dolist (current-image images)
	    (call-process  "tesseract"
			   nil
			   t
			   nil
			   current-image
			   "-"
			   "-l" tesseract-language
			   "quiet"))
	  (write-file output-file))))))

(defconst tesseract-image-regexp
  "\\.\\(GIF\\|JP\\(?:E?G\\)\\|PN[GM]\\|TIFF?\\|BMP\\|gif\\|jp\\(?:e?g\\)\\|pn[gm]\\|tiff?\\|bmp\\)\\'"
  "Regular expression for image file types supported by Tesseract (Leptonica).")

(defun tesseract/dired/filter-files (file)
  "Filter marked files for supported file types.
  FILE is a file path to match."
  (string-match-p tesseract-image-regexp file))

(defun tesseract/dired/filter-pdfs (file)
  "Filter marked files for pdfs.
  FILE is a file path to match."
  (string-match-p "\\.\\(PDF\\|pdf\\)\\'" file))

(defun tesseract/dired/marked-to-txt ()
  "Run Tesseract OCR on marked files, if they are supported.
 Output to text files with the same base name."
  (interactive)
  (let ((images (dired-get-marked-files
		nil
		nil
		'tesseract/dired/filter-images
		nil
		nil))
	(pdfs (dired-get-marked-files
		nil
		nil
		'tesseract/dired/filter-pdfs
		nil
		nil)))
    (dolist (pdf pdfs)
      (tesseract/ocr-pdf pdf))
    (tesseract/ocr-image images))
  (revert-buffer t t t))

(provide 'tesseract)
;;tesseract.el ends here
