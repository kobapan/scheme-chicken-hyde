(include "backwards-compatible-module")

(backwards-compatible-module (hyde page-eval-env)

(make-page-eval-env
 environment-set!
 environment-extend!
 environment-ref
 environment-copy
 environment-eval
 current-page-eval-env)

(import scheme)

(cond-expand
  (chicken-4
   (import chicken)
   (use srfi-69))
  (chicken-5
   (import (chicken base)
           (chicken irregex)
           (srfi 69))))

(define-record-type page-eval-env
  (%make-page-eval-env bindings)
  page-eval-env?
  (bindings page-eval-env-bindings page-eval-env-bindings-set!))

(define (make-page-eval-env)
  (%make-page-eval-env (make-hash-table)))

(define (environment-set! env name val)
  (hash-table-set! (page-eval-env-bindings env) name val))

(define environment-extend! environment-set!)

(define not-found (list 'not-found))

(define (environment-ref env name)
  (let ((val (hash-table-ref/default (page-eval-env-bindings env) name not-found)))
    (if (eq? not-found val)
        (error "Undefined page-eval-env binding" name)
        val)))

(define (environment-copy env)
  (%make-page-eval-env (hash-table-copy (page-eval-env-bindings env))))

(define current-page-eval-env
  (make-parameter #f))

(define (environment-eval exp env)
  (parameterize ((current-page-eval-env env))
    (eval
     (list '##core#let
           (hash-table-map
            (page-eval-env-bindings env)
            (lambda (name val)
              (list name
                    (cond-expand
                      (chicken-4
                       `(hyde-page-eval-env#environment-ref
                         (hyde-page-eval-env#current-page-eval-env)
                         ',name))
                      (chicken-5
                       `(hyde.page-eval-env#environment-ref
                         (hyde.page-eval-env#current-page-eval-env)
                         ',name))))))
           exp))))

)
