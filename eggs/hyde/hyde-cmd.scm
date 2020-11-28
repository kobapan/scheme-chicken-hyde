(cond-expand
  (chicken-4
   (use chicken-syntax hyde matchable))
  (chicken-5
   (import (chicken process-context)
           (chicken port)
           (chicken format)
           hyde
           matchable)))

(define usage #<<END
Hyde - A static website compiler

Usage: hyde <options> <command>

Options:

-e ENV
    The environment to execute the command in (default is "default")

Commands are:

hyde init
    Initializes a site in the current directory.

hyde new <page-type> [<title> ...]
    Creates a new page with the given page type and title. The page's
    filename will be inferred from the given title by downcasing it and
    replacing spaces with dashes.

hyde serve
    Serves the current site with spiffy, (re-)compiling the site on
    each request (useful for development).

hyde build [<prefix> ...]
    Builds the current site. If prefixes are given then only paths having
    those prefixes will be built

hyde
    Compiles the current site.
END
)

(define args (command-line-arguments))

(when (and (> (length args) 1)
           (string=? "-e" (car args)))

  (hyde-environment (string->symbol (cadr args)))
  (set! args (cddr (command-line-arguments))))

(define (maybe-compile-pages #!optional (prefixes '()))
  (if (load-hyde-file #f)
      (compile-pages prefixes)
      (print usage)))

(match args
  (("init")
   (initialize-site))
  (("new" ext . title)
   (load-hyde-file)
   (generate-page ext title))
  (("serve")
   (load-hyde-file)
   (serve))
  (("build" prefixes ...)
   (maybe-compile-pages prefixes))
  (() (maybe-compile-pages))
  (((or "help" "-help" "--help" "usage" "-usage" "--usage"))
   (print usage))
  ((command _ ...)
   (with-output-to-port (current-error-port)
     (lambda ()
       (print (format "Unknown command '~a'~%" command))))
   (print usage)
   (exit 1)))
