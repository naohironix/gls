#!/usr/bin/env gosh
;; -*- Mode: Scheme; -*-
;;
;; gls.scm generates a html which presents thumbnail of files.
;; It is the underlayer for gls. See also gls.
;;
;; Copyright (c) 2011 Naohiro Nishikawa, All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;  1. Redistributions of source code must retain the above copyright notice,
;;     this list of conditions and the following disclaimer.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
;; EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(use srfi-1)
(use gauche.parseopt)
(use file.util)

(define *img-suffix*
  '(bmp gif jpeg jpg png ppm ps tif tiff xpm
    BMP GIF JPEG JPG PNG PPM PS TIF TIFF XPM))

(define *img-size* '(100 75))

(define (make-regexp ls)
  (string->regexp 
   (string-join
    (map (lambda (x) (string-append "[.]" (symbol->string x) "$")) ls)
    "|" 'infix)))

(define (make-img-file-list files)
  (let ((pattern (make-regexp *img-suffix*)))
    (define (make-img-file-list2 files ls)
      (if (null? files)
	  ls
	  (let ((file (car files)))
	    (if (rxmatch pattern file)
		(make-img-file-list2 (cdr files) (append ls (list file)))
		(make-img-file-list2 (cdr files) ls)))))
    (make-img-file-list2 files '())))

(define (generate-href size files)
  (let ((wsize (car size))
	(hsize (cadr size)))
    (define (generate-href2 files)
      (for-each
       (lambda (file)
	 (format #t "<A HREF=\"~a\"><IMG SRC=\"~a\" WIDTH=~d HEIGHT=~d ALT=\"~a\"></A>\n" file file wsize hsize file))
       files))
    (generate-href2 (make-img-file-list files))))

(define (generate-html size files)
  (format #t "<HTML>\n<BODY>\n")
  (generate-href size files)
  (format #t "</BODY>\n</HTML>\n"))

(define (expand-files files)
  (define (expand-files2 files ls)
    (if (null? files)
	ls
	(if (file-is-directory? (car files))
	    (expand-files2 (cdr files)
			   (append
			    ls
			    (if (string=? (car files) ".")
				(directory-list (car files) :children? #t)
				(map (lambda (x) (string-append
						  (car files) "/" x))
				     (directory-list (car files)
				      :children? #t)))))
	    (expand-files2 (cdr files)
			   (append ls (list (car files)))))))
  (expand-files2 files '()))

(define (make-normal-file-list files)
  (define (make-normal-file-list2 files ls)
    (if (null? files)
	ls
	(let ((file (car files)))
	    (if (eq? (file-type file) 'regular)
		(make-normal-file-list2 (cdr files)
					(append ls (list (car files))))
		(make-normal-file-list2 (cdr files) ls)))))
  (make-normal-file-list2 files '()))

(define (gls size files)
  (generate-html size
   ;; remove none image files from the list
   (make-img-file-list
    ;; remove none regular files from the list
    (make-normal-file-list
     (expand-files files)))))

(define (version)
  (display "gls version 0.1.0\n")
  (exit 1))

(define (usage)
  (display "usage: gls [-smlv] [file ...]\n")
  (exit 1))

(define (main args)
  (let ((size *img-size*))
    (let ((args (parse-options (cdr args)
			       (("l" () (set! size '(200 150)))
				("m" () (set! size '(100 75)))
				("s" () (set! size '(60 45)))
				("v" () (version))
				(else _ (usage))))))
      (if (null? args)
	  (gls size '("."))
	  (gls size args))
      0)))
