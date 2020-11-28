(include "backwards-compatible-module")

(backwards-compatible-module (hyde atom)

(translate-atom)

(import scheme)

(cond-expand
  (chicken-4
   (import chicken)
   (use hyde atom rfc3339 posix extras srfi-1))
  (chicken-5
   (import (chicken base)
           (chicken format)
           (chicken time posix)
           (srfi 1)
           hyde
           atom
           rfc3339)))

(define $ (environment-ref (page-eval-env) '$))

(define (maybe-authors->atom-authors authors)
  (if authors
      (map (lambda (author)
             (make-author name: author))
           authors)
      '()))

(define (pages->atom-doc pages #!key
                         (page-title (lambda (page) ($ 'title page))) 
                         (page-date (lambda (page)
                                      (or ($ 'updated page) ($ 'date page))))
                         (page-type (lambda (page)
                                      ($ 'type page)))
                         (page-authors (lambda (page)
                                         ($ 'authors page)))
                         (page-date->rfc3339-string (lambda (x) x))
                         (page->atom-content (lambda (page)
                                               (make-content (read-page page) type: 'html))))

  (unless (and ($ 'tag) ($ 'base-uri) ($ 'date))
    (error "An atom page requires at least these page-vars to be defined"
           '(tag base-uri date)))

  (let* ((rfc3339-string->seconds 
          (lambda (date)
            (rfc3339->seconds (string->rfc3339 date))))
         (page-date->seconds
          (lambda (date)
            (rfc3339-string->seconds (page-date->rfc3339-string date))))
         (rfc3339-string->YYYY-MM-DD 
          (lambda (date)
            (time->string (seconds->utc-time (rfc3339-string->seconds date)) 
                          "%Y-%m-%d")))
         (page-date->YYYY-MM-DD
          (lambda (date)
            (rfc3339-string->YYYY-MM-DD (page-date->rfc3339-string date))))
         (feed-authors (maybe-authors->atom-authors ($ 'authors))))
    (make-atom-doc
     (make-feed
      title: (make-title ($ 'title))
      subtitle: (make-subtitle ($ 'subtitle))
      icon: (and ($ 'icon) (make-icon (string-append ($ 'base-uri) ($ 'icon))))
      logo: (and ($ 'logo) (make-logo (string-append ($ 'base-uri) ($ 'logo))))
      authors: feed-authors
      updated: (rfc3339->string
                (seconds->rfc3339
                 (fold (lambda (p c)
                         (let ((p (page-date->seconds (page-date p))))
                           (if (and c (> c p)) c p)))
                       #f
                       pages)))
      id: (format ($ 'tag) ($ 'date) "/")
      links: (list (make-link uri: (string-append ($ 'base-uri) (or ($ 'root-path) "/"))
                              relation: "alternate"
                              type: 'html)
                   (make-link uri: (string-append ($ 'base-uri) (page-path (current-page)))
                              relation: "self"
                              type: 'atom))
      entries: (map (lambda (p)
                      (make-entry title: (make-title (page-title p))
                                  published: (page-date->rfc3339-string (page-date p))
                                  updated: (page-date->rfc3339-string (page-date p))
                                  id: (format ($ 'tag) 
                                              (page-date->YYYY-MM-DD (page-date p))
                                              (page-path p))
                                  links: (list (make-link uri: (string-append ($ 'base-uri) (page-path p)) 
                                                          type: (or (page-type p) ($ 'entries-type))))
                                  authors: (let ((authors (maybe-authors->atom-authors (page-authors p))))
                                             ;; we include the feed authors for every entry in case there are 
                                             ;; none for this entry specifically since feed readers tend to ignore
                                             ;; feed-wide authors
                                             (if (null? authors) feed-authors authors))
                                  content: (page->atom-content p)))
                    pages)))))

(define (translate-atom)
  (let ((env (page-eval-env)))
    (for-each (lambda (binding)
		(environment-set! env (car binding) (cadr binding)))
	      `((make-atom-doc ,make-atom-doc) (make-author ,make-author) (make-category ,make-category)
		(make-content ,make-content) (make-contributor ,make-contributor) (make-entry ,make-entry)
		(make-feed ,make-feed) (make-generator ,make-generator) (make-icon ,make-icon)
		(make-link ,make-link) (make-logo ,make-logo) (make-rights ,make-rights)
		(make-source ,make-source) (make-subtitle ,make-subtitle) (make-summary ,make-summary)
		(make-title ,make-title) (make-rfc3339 ,make-rfc3339) (rfc3339->string ,rfc3339->string)
		(seconds->rfc3339 ,seconds->rfc3339) (utc-time->rfc3339 ,utc-time->rfc3339)
		(time->rfc3339 ,time->rfc3339) (pages->atom-doc ,pages->atom-doc)))

    (write-atom-doc (environment-eval (read) env))))

(translators (cons (list "atom" translate-atom '(ext . atom) '(layouts))
		   (translators)))

)
