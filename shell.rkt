#lang curly-fn sweet-exp racket

require
  for-syntax racket/string
  for-syntax sugar
  for-syntax racket/contract
  for-syntax racket/system
  for-syntax racket/match
  sugar

struct shell (process in out err)

begin-for-syntax
  struct shell (process in out err)
  define/contract (make-quoted-executable-fn exe-name)
    {symbol? -> (or/c procedure? any/c)}
    ;; TODO: Lazily move this to runtime and fail on first access of the
    ;;       program? Does that defeat the point?
    let ([exe-path (find-executable-path (->string exe-name))])
      if exe-path
        quasiquote
          lambda args
            lambda
              (#:input  [in  #f]
               #:output [out #f]
               #:err    [err #f])
              call-with-values
                #{apply #{subprocess out in err ,exe-path} (map ->string args)}
                #{shell %1 %3 %2 %4}
        `(error ,(format "Can't find executable ~s" (symbol->string exe-name)))
  define posix-programs
    '(|[| ar at awk basename batch bc cat chfn chgrp chmod chown chsh cksum cmp
      col comm cp crontab csplit cut dd df diff dirname dmesg du echo ed egrep
      env expr fgrep find fold fuser gencat getconf gettext grep groupadd
      groupdel groupmod groups gunzip gzip head hostname iconv id infocmp
      install ipcrm ipcs join kill killall ln locale localedef logger logname lp
      lpr ls lsb_release m4 mailx make man md5sum mkdir mkfifo mknod mktemp more
      mount msgfmt mv newgrp nice nl nohup od passwd paste patch pathchk pax
      pidof pr ps pwd renice rm rmdir sed sendmail seq sh shutdown
      split strings strip stty su tail tar tee test tic tput tr tsort tty umount
      uname unexpand uniq useradd userdel usermod wc xargs zcat
      (no-override date)  (no-override expand) (no-override false)
      (no-override file)  (no-override printf) (no-override sleep)
      (no-override sort)  (no-override sync)   (no-override time)
      (no-override touch) (no-override true))
  define arch-programs
    remove*
      '(ed lp lpr pax sendmail)
      posix-programs
  define/contract make-posix-name
    {symbol? -> symbol?}
    compose string->symbol #{string-join `("posix-" ,%) ""} symbol->string
  define (map-exe-require args)
    match args
      (list 'with-name name program)
        `(define ,name ,(make-quoted-executable-fn program))
      (list 'no-override program)
        let ([new-name (make-posix-name program)])
          map-exe-require `(with-name ,new-name ,program)
      (? symbol? program)
        map-exe-require `(with-name ,program ,program)
  define (require/executable-impl lst)
    ->
      listof (or/c (listof symbol?) symbol?)
      cons/c symbol? list?
    cons
      'begin
      map
        map-exe-require
        cdr lst
  define-namespace-anchor ns-anchor

;; TODO: Sort into `with-name` first, then `no-override`, then plain. Uniq'ify
;;       keeping the first of each (so that you can do, for example,
;;       `(require-executable*/arch-linux (with-name posix-vi vi))`). This can
;;       probably be done by adding to a hash keyed on executable name,
;;       overwriting if the current value is "less important"
define-syntax (require-executable stx)
  datum->syntax stx (require/executable-impl (syntax->datum stx))

define-syntax (require-executable*/posix stx)
  datum->syntax stx
    require/executable-impl posix-programs

define-syntax (require-executable*/arch-linux stx)
  datum->syntax stx
    require/executable-impl arch-programs

define (flags arg)
  string-join
    cons
      "-"
      map symbol->string
        arg
    ""

define filename "test.rkt"

define (with-io thnk)
  lambda (input)
    parameterize
      \\
        current-input-port input
        current-output-port void
      (thnk)

define (output . procs)
  void
    map
      compose
        subprocess-wait
        shell-process
        #{% #:output (current-output-port)}
      procs

define (pipe . procs)
  match procs
    (list single) single
    (list-rest head tail)
      lambda
        (#:input  [in  #f]
         #:output [out #f]
         #:err    [err #f])
        let-values ([(head-shell) (head #:input in #:err err)])
          (apply pipe tail) #:input (shell-out head-shell) #:output out #:err err

define (shell->string proc)
  letrec
    \\
      shl      (proc)
      in-prt   (shell-out shl)
    begin
      subprocess-wait (shell-process shl)
      port->string    in-prt

require-executable ls cat

define ls-clt #{ls "-clt"}

shell->string
  pipe
    ls-clt "."
    cat "-"
