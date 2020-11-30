(use hyde lowdown srfi-19)
(require-extension irregex)
(translators (cons (list "md" markdown->html) (translators)))

;; ---
;; custom values
;; ---

(define (base-uri) "http://localhost:8080")
(define (main-title) "My site")

;; ---
;; fundamental
;; ---

(define $ (environment-ref (page-eval-env) '$))

(define (page-url #!optional (page (current-page)))
  (page-path page))

;; add / on the right end
(define (r/ s)
  (if (equal? #\/ (string-ref s (- (string-length s) 1))) s (string-append s "/")))

;; ---
;; navigation-links
;; ---

(define (navigation-links)
 (let ((navs `(("ABOUT"
                ("My profile" . "profile"))
               ("BLOG"
                ("My blog" . "blog")
                ("#tags" . "tags"))
               ("OTHERS"
                ("Links" . "links"))
               )))
  `(ul (@ (class "gmenu"))
        ,(map (lambda (nav)
               (if (list? nav)
                   `(li (a (@ (class "dropdown-toggle")) ,(car nav))
                       (ul ,(map (lambda (sub-nav)
                                   `(li (a (@ (href ,(base-uri)"/",(r/ (cdr sub-nav)))
                                              (id ,(cdr sub-nav)))
                                           ,(car sub-nav))))
                                 (cdr nav))))
                    `(li (a (@ (href ,(base-uri)"/",(r/ (cdr nav)))
                               (id ,(cdr nav)))
                            ,(car nav)))))
              navs))))

;; ---
;; listing pages
;; ---

;; YYYY-mm-dd date string to utc seconds 
(define (page-date->second page)
  (let ((date-string ($ 'date page)))
    (time->milliseconds (date->time-utc (string->date date-string "~Y-~m-~d")))))

(define (sort-by pages accessor)
    (sort pages (lambda (p1 p2) (> (accessor p1) (accessor p2)))))

(define (pages-matching regex #!optional tag)
  (map cdr (filter (lambda (p) (and (irregex-match regex (car p))
                                    (if tag (member tag (or ($ 'tags (cdr p)) '())) #t)))
                   ((environment-ref (page-eval-env) 'pages)))))

(define (all-posts #!optional (tag #f))
  (sort-by (pages-matching "blog/.+\\.md$" tag) page-date->second))

(define (all-pages-and-posts)
  (let ((p '("profile"
             "links")))
    (append
     (all-posts)
     (pages-matching
      (string-trim-right
       (fold (lambda (s res) (string-append s "/.+$|" res)) "" p)
       (string->char-set "|"))))))

;; ---
;; tags
;; ---

;; posts link filtered by tag
;; exclude current post
;; @num show num posts
(define (tag-posts-links tag #!optional num)
  (let* ((tag-posts-all (all-posts tag)))
    `(dl ,(let loop ((tag-posts-all tag-posts-all) (num num) (res '()))
            (if (or (= 0 num) (null? tag-posts-all))
                res
                (let ((p (car tag-posts-all)))
                  (if (equal? ($ 'title) ($ 'title (car tag-posts-all)))
                      (loop
                       (cdr tag-posts-all)
                       num
                       res)
                      (loop
                       (cdr tag-posts-all)
                       (- num 1)
                       (cons res `((dt (a (@ (href ,(base-uri),(page-path p))) ,($ 'title p))
                                       " "
                                       (span (@ (class "date")) ,($ 'date p)))))))))))))
;; list of all posts grouped by tag
(define (tag-groups posts)
  (let ((tags '()))
    (map (lambda (post)
           (map (lambda(t)
                  (let ((old (or (alist-ref t tags equal?) '())))
                    (set! tags
                      (alist-update! t
                                     (cons `((title . ,($ 'title post)) (url . ,(page-url post)) (date . ,($ 'date post))) old)
                                     tags
                                     equal?))))
                (or ($ 'tags post) '())))
         posts)
    (reverse (map (lambda (t)
                    (cons (car t) (reverse (cdr t))))
                  tags))))

;; tag's css class
(define (tag-class t)
  (let* ((ranges '((1 "low")
                   (13 "medium")
                   (30 "high")))
         (class (filter (lambda (a) (>= (length (cdr t)) (car a))) ranges)))
    (string-append "tag-" (cadar (take-right class 1)))))

;; ---
;; table of contents
;; ---

;; toc
;; for use in layout
;; with h2 , h3
(define (toc string)
  (if (not ($ 'toc))
      string
      (with-input-from-string string
        (lambda ()
          (let loop ((num 1) (prev "") (html "") (toc ""))
            (let ((line (read-line)))
              (if (eof-object? line)
                  (string-append
                   (if (or (equal? "" toc) (not ($ 'toc))) "" (string-append toc "</ul></div>"))
                   html)
                  (let ((m (irregex-search "<h([2-3])( ?[^>]*)>(<a href=[^>]+>)?([^<]+)(</a>)?</h[2-3]>" line)))
                    (if m
                        (let ((deps (irregex-match-substring m 1))
                              (attr (irregex-match-substring m 2))
                              (atag (irregex-match-substring m 3))
                              (text (irregex-match-substring m 4)))
                          (loop
                           (+ num 1)
                           deps
                           (string-append
                            html "<h" deps " id=\"toc-"
                            (number->string num) "\"" attr ">" (if atag atag "") text (if atag "</a>" "") "</h" deps ">\n")
                           (string-append
                            (if (equal? "" toc) "<div id=\"toc\"><p>目次</p> <ul>" toc)
                            (if (and (equal? "3" prev) (equal? "2" deps)) "</ul>" "")
                            (if (and (equal? "2" prev) (not (equal? "2" deps))) "<ul>" "")
                            "<li><a href=\"#toc-" (number->string num) "\">"
                            text "</a></li> ")))
                        (loop
                         num prev (string-append html line) toc))))))))))

;; ---
;; page vars
;; ---

(default-page-vars `(((: bos "blog/" (+ any) ".md" eos)
                      (layouts "related-article.sxml" "blogpost.sxml" "default.sxml"))))

;; ---
;; misc
;; ---

(define max-articles 20)

;; show first img in a page
(define (page->img page)
  (let ((m (irregex-search "(<img src=[^> ]*)" (read-page page))))
    (if m (string-append (irregex-match-substring m) "height=\"100px\"")
        "")))

;;
;; show page content before <!-- more --> line
;; .md only!
;; @file-path file path relative to (source-dir)
;; @more a href text
;; @url a href url
(define (digest file-path #!optional (more "...more") (url file-path))
  (with-input-from-file (string-append (source-dir) "/" file-path)
    (lambda ()
      (read)
      (let loop ((lines ""))
        (let* ((buff (read-line))
               (m (irregex-search "<!-+ *more *-+>" buff)))
          (cond (m
                 (markdown->sxml (string-append lines "[" more "](" (base-uri) "/" url ")")))
                ((eof-object? buff)
                 (markdown->sxml lines))
                (else
                 (loop (string-append lines buff)))))))))
                      
;; page content raw string
(define (page-content source-path)
  (with-input-from-file (string-append (source-dir) "/" source-path)
    (lambda ()
      (read)
      (read-string))))

