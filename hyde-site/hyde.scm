(use hyde lowdown srfi-19)
(require-extension irregex)
(translators (cons (list "md" markdown->html) (translators)))

;; ---
;; custom values
;; ---

(define (base-uri) "http://localhost:8080")
(define (main-title) "My site")
(define max-articles 20)

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
    (define pair->a-link
      (lambda (src)
        (cond
         ((pair? src)
          `(a (@ (href ,(conc (base-uri) "/" (r/ (cdr src))))
                 (id ,(cdr src)))
              ,(car src)))
         (else src))))
`(div
    (ul (@ (class "gmenu"))
         ,(map (lambda (nav)
                 (if (list? nav)
                     `(li ,(pair->a-link (car nav))
                          (ul ,(map (lambda (sub-nav)
                                      `(li ,(pair->a-link sub-nav)))
                                    (cdr nav))))
                     `(li ,(pair->a-link nav))))
               navs))

    (div (@ (id "tagcloud"))
          ,(map (lambda (t)
                  `((a (@ (href ,(base-uri)"/tags/#",(car t))
                          (class ,(tag-class t)" onetag"))
                       "#",(car t))))
                (tag-groups (all-posts))))
    
    )))

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

;; (all-posts)
;; (all-posts 'ペチカ)
;; (all-posts 'ペチカ 10)
;; (all-posts #f 10)
(define (all-posts #!optional (tag #f) (num 0))
  (let ((posts (sort-by (pages-matching "blog/.+\\.md$" tag) page-date->second)))
    (cond ((= num 0) posts)
          ((< (length posts) num) posts)
          (else (take posts num)))))

;; (list-posts '火)
;; (list-posts '火 10)
(define (list-posts tag #!optional (num 0))
  (let ((posts (all-posts tag num)))
    `(ul ,(map (lambda (post)
                 `((li (a (@ (href ,(base-uri),(page-path post))) ,($ 'title post))
                       " "
                       (span (@ (class "date")) ,($ 'date post))
                       )))
               posts))))

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

;; post-links filtered by tag
;; exclude current post
;; @tag string: tag name
;; @num int: show num posts
(define (posts-with-tag-links tag #!optional num)
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
                                       (span (@ (class "date")) ,($ 'date p)
                     
                                             ,(fold (lambda (tag seed)
                                                      (append seed
                                                              `(" "(span (a (@ (href ,(base-uri)"/tags/#",tag)
                                                                               (class "tag"))
                                                                            "#",tag)))))
                                                    '()
                                                    (or ($ 'tags p) '()))
                     
                                             ))))))))))))

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
                         num prev (string-append html line "\n") toc))))))))))

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
