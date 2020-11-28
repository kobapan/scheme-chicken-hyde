(include "backwards-compatible-module")

(backwards-compatible-module hyde

(load-hyde-file
 hyde-environment
 hyde-environments
 define-hyde-environment
 initialize-site
 generate-page
 pathify
 make-external-translator
 serve
 source-dir
 output-dir
 layouts-dir
 default-layouts
 clean-before-build
 excluded-paths
 default-extension
 default-page-vars
 page-eval-env
 translators
 compile-pages
 uri-path-prefix
 markdown-program
 link-shortcuts
 sxml-conversion-rules
 ignore-page?
 around-page-translate
 page-serve-handler

 pages
 page-by-path
 read-page
 serve-page
 current-page
 page?
 page-source-path
 page-path
 page-vars
 page-reader
 page-writer
 page-type

 translate-sxml 
 translate-scss 
 translate-markdown
 translate-svnwiki)

(import scheme)

(cond-expand
  (chicken-4
   (import chicken)
   (require-extension regex)
   (import irregex)

   (use files
        data-structures
        extras
        srfi-1
        ports
        srfi-13
        utils
        posix
        sxml-transforms
        doctype
        matchable
        scss
        spiffy
        srfi-18
        colorize
        intarweb
        uri-common
        svnwiki-sxml
        defstruct
        sxpath
        html-parser
        hyde-page-eval-env)

   (reexport (except hyde-page-eval-env current-page-eval-env))
   (define copy-file file-copy)
   (define read-list read-file))
  (chicken-5
   (import (chicken base)
           (chicken string)
           (chicken irregex)
           (chicken condition)
           (chicken format)
           (chicken port)
           (chicken file)
           (chicken file posix)
           (chicken process)
           (chicken pathname)
           (chicken io)
           (chicken port)
           (chicken module)
           (srfi 1)
           (srfi 13)
           (srfi 18)
           sxml-transforms
           doctype
           matchable
           scss
           spiffy
           colorize
           intarweb
           uri-common
           svnwiki-sxml
           defstruct
           sxpath
           html-parser
           (hyde page-eval-env))
   (reexport (except (hyde page-eval-env) current-page-eval-env))))



(defstruct page source-path path (vars '()) reader writer type)

(define (with-page page proc #!optional (key page))
  (cond ((page? page) (parameterize ((current-page page)) (proc page)))
	((string? page) (with-page (alist-ref page (pages) string=?) proc page))
	(else (die (conc "unknown page: " key) 3))))

(define (write-page page)
  ((with-page page page-writer)))

(define (read-page page #!rest layouts)
  (with-page page
             (lambda (page)
               (parameterize ((current-page page))
                 (wrap-with-layouts ((with-page page page-reader)) layouts)))))

(define hyde-environment (make-parameter 'default))
(define hyde-environments (make-parameter '(default)))
(define source-dir (make-parameter "src"))
(define output-dir (make-parameter "out"))
(define layouts-dir (make-parameter "layouts"))
(define default-layouts (make-parameter '("default.sxml")))
(define clean-before-build (make-parameter #t))
(define excluded-paths (make-parameter (list (irregex '(seq "~" eos)))))
(define default-extension (make-parameter "html"))
(define default-page-vars (make-parameter '()))
(define uri-path-prefix (make-parameter ""))
(define markdown-program (make-parameter "markdown"))
(define link-shortcuts (make-parameter '()))
(define ignore-page? (make-parameter #f))
(define around-page-translate (make-parameter (lambda (p c) (c))))


(define translators (make-parameter '()))
(define current-page (make-parameter #f))
(define pages (make-parameter '()))
(define page-eval-env (make-parameter (make-page-eval-env)))

(define-syntax define-hyde-environment 
  (syntax-rules ()
    ((_ name e1 e2 ...)
     (begin
       (hyde-environments (cons 'name (hyde-environments)))
       (when (eq? 'name (hyde-environment))
         e1 e2 ...)))))

(define (with-current-page-default accessor)
  (lambda (#!optional (page (current-page)))
    (accessor page)))
 
(for-each (lambda (b)
	    (environment-set! (page-eval-env) (car b) (cdr b)))
	  `((read-page . ,read-page)
	    (page-vars . ,(with-current-page-default page-vars))
	    (page-path . ,(with-current-page-default page-path))
	    (page-type . ,(with-current-page-default page-type))
	    (page-source-path . ,(with-current-page-default page-source-path))
	    (current-page . ,current-page)
	    ($ . ,(lambda (name #!optional (page (current-page)))
		    (alist-ref name (page-vars page))))))

(define default-layout-template #<<END
()
`((xhtml-1.0-strict)
  (html
   (head
    (title ,($ 'title)))
   (body
    (h1 ,($ 'title))
    (inject ,contents))))
END
)

(define (output-xml doc rules)
  (SRV:send-reply (fold (lambda (rule doc)
			  (pre-post-order* doc rule))
			doc
			rules)))

(define (colorize-code language code)
  (let* ((class (conc "highlight " language "-language"))
	 (code (map (lambda (s)
                      (handle-exceptions exn
                        (htmlize s)
                        (html-colorize language s)))
                    code)))

    `(pre (@ (class ,class)) (inject . ,code))))

(define sxml-colorize-rules
  `((highlight *macro* . ,(lambda (tag els)
                            (cons 'colorize els)))
    (colorize *preorder* . ,(lambda (tag els)
                              (colorize-code (car els) (cdr els))))
    ,@alist-conv-rules*))

(define sxml-conversion-rules 
  `((inject *preorder* . ,(lambda (tag sxml) sxml))
    (shortcut . ,(lambda (tag attrs)
                   (apply expand-link-shortcut attrs)))
    ,@doctype-rules
    ,@universal-conversion-rules*))

(define (expand-link-shortcut alias . args)
  (let ((uri-template (alist-ref alias (link-shortcuts))))
    (cond ((not uri-template)
           (error 'expand-link-shortcut
                  (format "invalid link shortcut: ~S" alias)))
          ((procedure? uri-template)
           (apply uri-template args))
          (else 
           (apply format uri-template (map uri-encode-string args))))))

(define (print-error error)
  (with-output-to-port (current-error-port)
    (cut print "ERROR: " error)))

(define (die error exit-code)
  (print-error error)
  (exit exit-code))

(define (load-hyde-file #!optional (die-when-missing? #t))
  (if (file-exists? "hyde.scm")
      (begin
        (load "hyde.scm")
        (unless (memq (hyde-environment) (hyde-environments))
          (die (format "environment '~A' is not defined for this site" (hyde-environment)) 1)))
      (begin
        (print-error "no hyde.scm found")
        
        (if die-when-missing? 
            (exit 1)
            (begin (newline) #f)))))

(define (create-directory-verbose name)
  (print "creating " name)
  (create-directory name #t))

(define (initialize-site)
  (unless (null? (directory))
    (die "unable to initialize site, directory is not empty" 2))
  
  (create-directory-verbose (layouts-dir))
  (create-directory-verbose (source-dir))
  (create-directory-verbose (output-dir))

  (print "creating hyde.scm")
  (with-output-to-file "hyde.scm"
    (cut write (cond-expand
                 (chicken-4
                  '(use hyde))
                 (chicken-5
                  '(import hyde)))))
  (let ((default-layout (make-pathname (layouts-dir) (car (default-layouts)))))
    (print "creating " default-layout)
    (with-output-to-file default-layout
      (cut print default-layout-template))))

(define (pathify string)
  (let* ((path  (string-downcase string))
         (path  (irregex-replace/all '(submatch (+ (~ alpha #\- #\space))) path ""))
         (path  (irregex-replace/all '(submatch (+ (" -"))) path "-")))
    (string-trim-both path #\-)))

(define (generate-page ext title)
  (let* ((title (string-intersperse title))
         (path (pathify title))
         (path (make-pathname (source-dir) path ext)))
    (with-output-to-file path (cut write `((title . ,title))))
    (print path)))

(define (page-by-path path)
  (let* ((path (if (string? path)
                   path
                   (let* ((path (if (string=? "" (car path))
                                    path
                                    (cons "" path)))
                          (path (cons (car path)
                                      (remove string-null? (cdr path)))))
                     (string-join path "/"))))
         (path (if (string=? "" path) "/" path))
         (page (find (lambda (page)
                       (string=? (page-path (cdr page)) path))
                     (pages))))
    (and page (cdr page))))

(define (send-page page)
  (print-page-paths page)
  (send-response body: (parameterize ((current-page page))
                         (wrap-with-layouts (read-page page)))
                 headers: `((content-type ,(file-extension->mime-type
                                            (pathname-extension (page-path page)))))))

(define (serve-page page path continue)
  (case (and page (page-type page))
    ((dynamic) (send-page page))
    ((directory) 
     (call/cc (lambda (break)
                (for-each (lambda (index-file)
                            (let* ((index-path (append path (list index-file)))
                                   (index-page (page-by-path index-path)))

                              (when index-page
                                (send-page index-page)
                                (break index-page))))
                          (index-files))

                (continue))))
    (else (continue))))

(define page-serve-handler
  (make-parameter serve-page))

(define (serve)
  (root-path (source-dir))
  (vhost-map `((".*" . 
                ,(lambda (continue)
                   (with-pages
                    (lambda ()
                      (let ((path (cdr (uri-path (request-uri (current-request))))))
                        ((page-serve-handler) (page-by-path path) path continue))))))))
  (print (format "spiffy serving hyde on port ~A" (server-port)))
  (start-server))

(define (cmd name . args)
  (receive (_ exited-normally status)
    (process-wait (process-run name args))
    (unless (and exited-normally (zero? status))
      (error (format "error executing ~A ~A" name (string-intersperse args))))))

(define (pathname-relative-from source dest)
  (let ((source (make-pathname source "/")))
    (if (string=? source (make-pathname dest "/"))
        "."
        (begin
          (assert (string-prefix? source dest) (format "path ~s is not a parent of ~s" source dest))
          (substring dest (string-length source))))))

(define (make-output-path path #!optional page)
  (let* ((output-file (make-pathname (output-dir) (pathname-relative-from (source-dir) path)))
         (output-file (irregex-replace "(.*)/([0-9]{4})-([0-9]{2})-([0-9]{2})-(.*)" output-file 1 "/" 2 "/" 3 "/" 4 "/" 5)))
    (if page
	(pathname-replace-extension output-file (->string (or (alist-ref 'ext (page-vars page)) (default-extension))))
	output-file)))

(define (make-access-path path #!optional page)
  (let ((path (pathname-relative-from 
               (output-dir) 
               (make-output-path path page))))
    (make-absolute-pathname 
     (uri-path-prefix)
     (if (string=? path ".") 
         "/"
         path))))

(define (call-with-returning value proc)
  (proc value)
  value)

(define (wrap-with-layout layout contents)
  (with-input-from-source-file layout
                               (lambda (meta)
                                 (match (translator-for layout)
                                   ((translate . translator-page-vars)
                                    (page-vars-set! (current-page) (append (page-vars (current-page)) meta translator-page-vars))
                                    (environment-set! (page-eval-env) 'contents contents)
                                    (translate))
                                   (else (format "unknown layout format: ~A" layout))))))

(define (wrap-with-layouts contents #!optional layouts)
  (let* ((layouts (or layouts (alist-ref 'layouts (page-vars (current-page))) (default-layouts))))
    (fold (cut wrap-with-layout <> <>)
          contents
          (map (cut make-pathname (layouts-dir) <>) layouts))))

(define (with-input-from-source-file source-file proc)
  (with-input-from-file source-file
    (lambda ()
      (proc (read)))))

(define (compile-page-by-extension file translate page #!optional (env (environment-copy (page-eval-env))))
  (with-input-from-source-file file 
    (lambda (meta)
      (parameterize ((current-page page) (page-eval-env env))
	(translate)))))

(define (translator-for file)
  (and-let* ((ext (pathname-extension file))
             (translator (alist-ref ext (translators) string=?)))
    (cons (lambda () 
            (with-output-to-string (car translator)))
          (cdr translator))))

(define (default-page-vars-for page)
  (append-map cdr (filter (lambda (d)
                            (if (procedure? (car d))
                                ((car d) page)
                                (irregex-search (car d) (page-source-path page))))
                          (default-page-vars))))

(define (default-page-vars-for-date page)
  (let ((m (irregex-search "([0-9]{4}-[0-9]{2}-[0-9]{2})-.+\\.md$" (page-source-path page))))
    (if m `((date . ,(irregex-match-substring m 1))) `())))

(define (classify-path path)
  (let* ((source-path (pathname-relative-from (source-dir) path))
         (source-path (if (string=? "." source-path) "" source-path)))
    (cons source-path
	  (cond ((directory? path)
             (make-page type: 'directory
                        source-path: source-path
                        path: (make-access-path path)
                        reader: (lambda () (directory path))
                        writer: (lambda () (create-directory (make-output-path path) #t))))
            ((translator-for path) => 
             (lambda (translator)
               (let* ((translate (car translator))
                      (translator-page-vars (cdr translator))
                      (local-page-vars (or (with-input-from-file path read) '()))
                      (page (make-page type: 'dynamic
                                       source-path: source-path
                                       vars: (append local-page-vars translator-page-vars)))
                      (page (update-page page path: (make-access-path path page)))
                      (page (update-page page vars: (append (default-page-vars-for-date page)
                                                            local-page-vars
                                                            (default-page-vars-for page)
                                                            translator-page-vars)))
                      (reader (let ((contents #f))
                                (lambda ()
                                  (unless contents
                                    (set! contents (compile-page-by-extension path translate page)))
                                  contents)))
                      (writer (lambda ()
                                (let ((rel-path (make-output-path path page)))
                                  (create-directory (string-append (current-directory) "/" (pathname-directory rel-path))
                                                    #t)
                                  (with-output-to-file rel-path
                                    (lambda ()
                                      (parameterize ((current-page page))
                                        (display ((around-page-translate)
                                                  page
                                                  (lambda () (wrap-with-layouts (reader))))))))))))
                 (update-page page writer: writer reader: reader))))
            (else (make-page type: 'static
                             source-path: source-path
                             path: (make-access-path path)
                             reader: (lambda () (call-with-input-file path read-list))
                             writer: (lambda () (copy-file path (make-output-path path) #t))))))))

(define (print-page-paths page)
  (display (page-source-path page))
  (print " -> " (substring (page-path page) 1)))

(define (compile-page page)
  (unless (and (ignore-page?) ((ignore-page?) page))
    (unless (eq? 'directory (page-type page))
      (print-page-paths page))
    (write-page page)))

(define (exclude-file? file)
  (any (cut irregex-search <> file) (excluded-paths)))

(define (with-pages thunk #!optional include-file?)
  (parameterize ((pages '()))
    (prepare-compilation (or include-file? (constantly #t)))
    (thunk)))

(define (prepare-compilation include-file?)
  (pages (list (classify-path (source-dir))))
  (environment-set! (page-eval-env) 'uri-path-prefix (uri-path-prefix))
  (environment-set! (page-eval-env) 'pages pages)
  
  (find-files (source-dir)
              dotfiles: #t
              test: (conjoin (complement exclude-file?)
                             include-file?)
              action: (lambda (file _)
                        (pages (cons (classify-path file) (pages))))))

(define (compile-pages path-prefixes)
  (when (clean-before-build)
    (print "cleaning output directory")
    (cmd "rm" "-rf" (output-dir))
    (create-directory (output-dir) #t))

  (print "preparing compilation")
  (with-pages
   (lambda ()
     (print "compiling pages")
     (for-each (compose compile-page cdr) (reverse (pages))))
   (and (not (null? path-prefixes))
        (lambda (file)
          (any (lambda (prefix)
                 (string-prefix? prefix file))
               path-prefixes)))))

(define (translate-sxml)
  (output-xml (map (lambda (e) (environment-eval e (page-eval-env))) (read-list))
	      (list sxml-colorize-rules sxml-conversion-rules)))

(translators (cons (list "sxml" translate-sxml) (translators)))

(define (copy-port/buffered buffer-size from to)
  (copy-port from
             to
             (lambda (in)
               (let ((x (read-string buffer-size in)))
                 (if (or (eof-object? x) (string=? "" x))
                     #!eof
                     x)))
             (lambda (x out)
               (write-string x #f out))))

(define (make-external-translator program-name #!optional (args (constantly '())))
  (lambda ()
    (receive (in out pid err)
      ;; TODO: Check for errors
      (process* (if (procedure? program-name)
                    (program-name)
                    program-name) (args))
      (copy-port/buffered 1024 (current-input-port) out)
      (close-output-port out)
      (copy-port/buffered 1024 in (current-output-port))
      (close-input-port in)
      (close-input-port err))))

(define translate-markdown (make-external-translator markdown-program))

(translators (cons (list "md" translate-markdown) (translators)))

(define (translate-scss)
  (let loop ((sexp (read)))
    (unless (eof-object? sexp)
      (let ((scss (environment-eval sexp (page-eval-env))))
        (write-css (if (memq (car scss) '(css css+))
                       scss
                       (cons 'css+ scss))))
      (loop (read)))))

(translators (cons (list "scss" translate-scss '(ext . css) '(layouts))
		   (translators)))

(define +svnwiki-shortcut-link+ 
  (irregex `(seq (submatch (+ (~ #\:))) #\: (submatch (+ any)))))

(define (expand-link-shortcut/svnwiki tag attrs)
  (let* ((m (irregex-match +svnwiki-shortcut-link+ (car attrs)))
         (uri (cond ((and m (irregex-match-substring m 1)) =>
                     (lambda (alias)
                       (expand-link-shortcut (string->symbol alias)
                                             (irregex-match-substring m 2))))
                    (else (car attrs)))))
                                                      
    (list (if (absolute-uri? (uri-reference uri))
              'link
              'int-link)
          uri
          (cdr attrs))))

;; Copied from qwiki-sxml
(define (internal-link r)
  (pre-post-order* 
   r
   `((*default* . ,(lambda (tag . elems) elems))
     (*text* . ,(lambda (trigger str) 
                  (let ((str (string-downcase str)))
                    (fold (lambda (regex/subst str)
                            (irregex-replace/all (car regex/subst) str (cdr regex/subst)))
                          str
                          '(("^[^a-z]+" . "")
                            ("[^a-z0-9_ \t-]" . "")
                            ("[ \t]+" . "-")))))))))

;; Copied from qwiki-sxml; some unnecessary rules were removed
(define (svnwiki-html-transformation-rules content)
  `(((diff
      *macro*
      . ,(lambda (tag elems)
           ;; The diff-language class is a bit weird here, but
           ;; consistent with what we would emit in a highlight block.
           (let* ((classname "highlight diff-language diff-page")
                  (diff (handle-exceptions exn elems
                          (map (lambda (s)
                                 (cdr (html->sxml (html-colorize 'diff s))))
                               elems))))
             `(pre (@ (class ,classname)) . ,diff))))

     (wiki-content
      *macro* .
      ,(lambda (tag contents)
         `(div (@ (id "content")) . ,contents)))

     (tags
      *preorder* .
      ,(lambda (tag page-tags)
         `(ul (@ (class "tags"))
              . ,(map (lambda (tag) `(li ,tag))
                      (string-split (car page-tags))))))

     (highlight
      *macro*
      . ,(lambda (tag elems)
           (let* ((lang (car elems))
                  (classname (conc "highlight " lang "-language"))
                  (code (handle-exceptions exn
                            (cdr elems)
                          (map (lambda (s)
                                 (cdr (html->sxml (html-colorize lang s))))
                               (cdr elems)))))
             `(pre (@ (class ,classname)) . ,code))))

     (examples
      ((example
        ((init
          *macro*
          . ,(lambda (tag elems)
               `(div (@ (class "init")) (highlight scheme . ,elems))))
         (expr
          *macro*
          . ,(lambda (tag elems)
               `(div (@ (class "expression")) (highlight scheme . ,elems))))
         (input
          *macro*
          . ,(lambda (tag elems)
               `(div (@ (class "io input")) (em "input: ")
                     (highlight scheme . ,elems))))
         (output
          *macro*
          . ,(lambda (tag elems)
               `(div (@ (class "io output")) (em "output: ")
                     (highlight scheme . ,elems))))
         (result
          *macro*
          . ,(lambda (tag elems)
               `(div (@ (class "result"))
                     (span (@ (class "result-symbol")) " => ")
                     (highlight scheme . ,elems))))) ;; Or use "basic lisp" here?
        . ,(lambda (tag elems)
             `(div (@ (class "example")) . ,elems))))
      . ,(lambda (tag elems)
           `(div (@ (class "examples"))
                 (span (@ (class "examples-heading")) "Examples:") . ,elems)))

     (page-specific-links
      *macro* 
      . ,(lambda (tag elems)
           `(ul (@ (id "page-specific-links"))
                (li ,(if ((if-sxpath '(// new-file)) (cons tag elems))
                         `(span (@ (class "disabled")
                                   (title "This page doesn't exist yet"))
                                "show")
                         `(a (@ (href "?action=show")) "show")))
                (li ,(if ((if-sxpath '(// read-only)) (cons tag elems))
                         `(span (@ (class "disabled")
                                   (title "This page has been frozen. "
                                          "Only someone with direct access "
                                          "to the repository can edit it."))
                                "edit")
                         `(a (@ (href "?action=edit") (rel "nofollow")) "edit")))
                (li ,(if ((if-sxpath '(// new-file)) (cons tag elems))
                         `(span (@ (class "disabled")
                                   (title "This page doesn't exist yet"))
                                "history")
                         `(a (@ (href "?action=history")) "history"))))))

     (@ *preorder* . ,(lambda (tag elements) (cons tag elements)))

     (toc ;; Re-scan the content for "section" tags and generate
      *macro*
      . ,(lambda (tag rest) ;; the table of contents
           `(div (@ (id "toc"))
                 ,rest
                 (ol ,(let find-sections ((content content))
                        (cond
                         ((not (pair? content)) '())
                         ((pair? (car content))
                          (append (find-sections (car content))
                                  (find-sections (cdr content))))
                         ((eq? (car content) 'section)
                          (let* ((level (cadr content))
                                 (head-word (caddr content))
                                 (href (list "#" (internal-link head-word)))
                                 (subsections (find-sections (cdddr content))))
                            (cond ((and (integer? level) head-word)
                                   `((li (a (@ (href (,href))) ,head-word)
                                         ,@(if (null? subsections)
                                               '()
                                               `((ol ,subsections))))))
                                  (else
                                   (error 'html-transformation-rules
                                          "section elements must be of the form (section level head-word . contents)")))))
                         (else (find-sections (cdr content)))))))))

     (section
      *macro*
      . ,(lambda (tag elems)
           (let* ((level (car elems))
                  (head-word (cadr elems))
                  (link (internal-link head-word))
                  (contents (cddr elems)))
             (cond ((and (integer? level) head-word)
                    `((a (@ (href ,@(list "#" link)))
                         (,(string->symbol (string-append "h" (number->string level)))
                          (@ (id ,link))
                          ,head-word)) . ,contents))
                   (else
                    (error 'html-transformation-rules
                           (conc "section elements must be of the form (section level head-word . contents), got " elems)))))))

     (section*
      *macro*
      . ,(lambda (tag elems)
           (let ((level (car elems))
                 (head-word (cadr elems))
                 (contents (cddr elems)))
             (cond ((and (integer? level) head-word)
                    `((,(string->symbol (string-append "h" (number->string level)))
                       ,head-word ) . ,contents))
                   (else
                    (error 'html-transformation-rules
                           (conc "section elements must be of the form (section level head-word . contents), got " elems)))))))

     (def
      ((sig . ,(lambda (tag types)
                 (map (lambda (spec)
                        `(span (@ (class ,(conc "definition " (car spec))))
                               (em "[" ,(symbol->string (car spec)) "]")
                               " " (tt ,@(cdr spec)) (br)))
                      types))))
      . ,(lambda (tag elems) elems))

     (pre
      . ,(lambda (tag elems)
           `(pre (tt . ,elems))))

     (image-link
      *macro*
      . ,(lambda (tag elems)
           `(img (@ (src ,(car elems)) . ,(if (null? (cdr elems))
                                              '()
                                              `((alt ,(cadr elems))
                                                (title ,(cadr elems))))))))

     (int-link
      *macro*
      . ,(lambda (tag elems)
           ;; Normalize links so people can refer to sections by their proper name
           (let* ((parts (string-split (car elems) "#" #t))
                  (nparts (intersperse
                           (cons (car parts) (internal-link (cdr parts)))
                           "#")))
             `(a (@ (href ,@nparts) (class "internal"))
                 ,(if (null? (cdr elems)) (car elems) (cadr elems))))))

     (link
      *macro*
      . ,(lambda (tag elems)
           `(a (@ (href ,(car elems)) (class "external"))
               ,(if (null? (cdr elems)) (car elems) (cadr elems)))))

     ,@alist-conv-rules*)

    ((html:begin
      . ,(lambda (tag elems)
           (list xhtml-1.0-strict
                 "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
                 elems
                 "</html>")))

     (verbatim
      *preorder*
      . ,(lambda (tag elems)
           elems))

     ,@universal-conversion-rules*)))

(define (translate-svnwiki)
  (let* ((doc (svnwiki->sxml (current-input-port)))
         (doc (pre-post-order* doc `((int-link . ,expand-link-shortcut/svnwiki)
                                     ,@alist-conv-rules*)))
         (rules (svnwiki-html-transformation-rules doc))
         (rules (append (butlast rules)
                        (list (cons (assq 'inject sxml-conversion-rules)
                                    (last rules))))))

    (output-xml doc (cons sxml-colorize-rules rules))))

(translators (cons* (list "wiki" translate-svnwiki)
		    (list "sw" translate-svnwiki)
		    (translators)))

)

