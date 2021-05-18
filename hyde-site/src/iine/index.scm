#!/bin/bash
#|
exec csi -s "$0" "$@"
|#

(use sql-de-lite
     srfi-19;date time
     simple-md5;string->md5sum
     )

(define ($.query->list string #!optional (delimiter "&") (separator "= "))
  (let loop ((params (string-split (if string string "") delimiter))
             (result '()))
    (if (null? params)
        (reverse result)
        (loop (cdr params)
              (cons (string-split (car params) separator) result)))))
(define ($.get src key)
  (let ((v (alist-ref key src equal?)))
    (if v (car v) #f)))

(define uri-params ($.query->list (get-environment-variable "QUERY_STRING")))
(define cookies ($.query->list (get-environment-variable "HTTP_COOKIE") ";"))

(let* ((db (open-database "iine.db"))
       (mode ($.get uri-params "mode"))
       (pid (get-environment-variable "HTTP_REFERER"))
       (uid ($.get cookies "iine-uid"))
       (display-count
        (lambda () (display (query
                             fetch-value
                             (sql db "SELECT COUNT(*) FROM iine_table WHERE pid = ?")
                             pid))))
       )
  ;; create table
  (exec (sql db
             "CREATE TABLE IF NOT EXISTS iine_table
              (
               id integer primary key not null,
               pid integer not null,
               uid text not null,
               created timestamp not null default (datetime('now', 'localtime'))
              );"))
  (exec (sql db "PRAGMA synchronous = 0"))
  
  ;; Cookieがなければ発行
  (unless uid
    (set! uid (string->md5sum (date->string (current-date) "~Y-~m-~d ~H:~M:~S~N")))
    (print (string-append
            "Set-Cookie:iine-uid=" uid ";"
            "Max-Age=" (number->string (* 60 60 24)) ";"
            "Path=/;secure")))
  
  (let ((cgi-header (lambda ()
                      (print "Content-Type: text/html; charset=UTF-8")
                      (newline))))
    (cgi-header))
  
  (cond
   ;; ページ読み込み時の処理
   ((and (equal? mode "get") pid) (display-count))
   ;; いいネリクエスト時の処理
   ((and (equal? mode "put") pid uid)
    ;;いいネしてなかったら
    (if (= 0 (query fetch-value
                    (sql db "SELECT COUNT(*) FROM iine_table WHERE pid = ? AND uid = ?")
                    pid uid))
        ;; いいネする
        (exec (sql db "INSERT INTO iine_table (pid, uid) VALUES (?, ?)")
              pid uid))
    (display-count))
   (else ""))
  
  (close-database db)
  
  )
