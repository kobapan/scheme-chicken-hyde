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
       (uid ($.get cookies "userid"))
       (display-count
        (lambda () (display (query
                             fetch-value
                             (sql db "SELECT COUNT(*) FROM iine_table WHERE entry_id = ?")
                             pid))))
       )
  ;; create table
  (exec (sql db
             "CREATE TABLE IF NOT EXISTS iine_table
              (
               id integer primary key not null,
               entry_id integer not null,
               account_id text not null,
               created timestamp not null default (datetime('now', 'localtime'))
              );"))
  (exec (sql db "PRAGMA synchronous = 0"))
  
  ;; Cookieがなければ発行
  (unless uid
    (set! uid (string->md5sum (date->string (current-date) "~Y-~m-~d ~H:~M:~S~N")))
    (print (string-append
            "Set-Cookie:userid=" uid ";"
            "Max-Age=" (number->string (* 60 60 24)) ";"
            "Path=/;secure")))
 
  ;; cgi header ここまで
  (let ((cgi-header (lambda ()
                      (print "Content-Type: text/html; charset=UTF-8")
                      (newline))))
    (cgi-header))

  (cond
   ;; ページ読み込み時の表示処理
   ((and (equal? mode "get") pid) (display-count))
   ;; いいネ！した時の処理
   ((and (equal? mode "put") pid uid)
    ;;いいネ！の記録がなかたら
    (if (= 0 (query fetch-value
                    (sql db "SELECT COUNT(*) FROM iine_table WHERE entry_id = ? AND account_id = ?")
                    pid uid))
        ;; いいネ！レコードを挿入
        (exec (sql db "INSERT INTO iine_table (entry_id, account_id) VALUES (?, ?)")
              pid uid))
    (display-count))
   (else ""))
  
  (close-database db)
  
  )
