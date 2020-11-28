#!/bin/sh
#| run a script on UNIX systems
exec csi -s "$0" "$@"
|#

(require-extension irregex)
(use posix pathname-expand)

(define src-dir "~/jekyll/_posts/")
(define out-dir "~/Chicken/hyde-site/src/blog/")
(define (src #!optional (file "")) (string-append (pathname-expand src-dir) file))
(define (out #!optional (file "")) (string-append (pathname-expand out-dir) file))
(define ($ m i) (irregex-replace/all "\"" (string-trim-both (irregex-match-substring m i))))

(map (lambda (file)
       (if (irregex-match ".+\\.md$" file)
           (begin
             (delete-file* (out file))
             (with-output-to-file (out file)
               (lambda ()
                 (with-input-from-file (src file)
                   (lambda ()
                     (read-line);skip first line
                     (display "(")
                     (let loop ((header #t))
                       (let ((line (read-line)))
                         (unless (eof-object? line)
                             (let ((m (irregex-match "(tag|title|toc):[ ]?(.*)$" line)))
                               (cond ((and m (equal? ($ m 1) "tag"))
                                      (print "(tags " ($ m 2) ")"))
                                     ((and m (equal? ($ m 1) "title"))
                                      (print "(title . \"" ($ m 2) "\")"))
                                     ((and m (equal? ($ m 1) "toc"))
                                      (print "(toc . #t)"))
                                     ((equal? "---" (string-trim-both line))
                                      (set! header #f)
                                      (print ")"))
                                     ((not header)
                                      (print line))))
                             (loop header)))))))))))
     (directory (src)))

