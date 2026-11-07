#lang racket/base

;; Adapted from Racket 9.3 racket/private/stream-cons.rkt. See NOTICE.
(require (prefix-in for: racket/private/for)
         (only-in racket/private/stream-cons [stream-cons native-stream-cons])
         (only-in rackunit require/expose))
(provide stream-cons stream-lazy stream-force stream-inspect stream-force-once)

;; Unchanged native definitions: reuse rather than copy them.
;; native-stream-cons preserves head evaluation and multiple-value handling.
(require/expose racket/private/stream-cons
  (eagerly-created-stream?
   eagerly-created-stream-first-forced? eagerly-created-stream-first
   eagerly-created-stream-rest stream-force-first reentrant-error))

;; Redefine the delayed structure because its stream methods must call our
;; stream-force. The native method closures retain their original binding.
;; The native cons structure can be reused: its head forcing is unchanged.
(define-struct lazily-created-stream (creator-if-unforced thunk-or-stream)
  #:mutable
  #:reflection-name 'stream
  #:property for:prop:stream (vector
                              (lambda (p) (stream-null? p))
                              (lambda (p) (stream-car p))
                              (lambda (p) (stream-cdr p))))


;; Redefine constructors to build the local delayed structure.
(define-syntax stream-lazy
 (syntax-rules ()
   [(stream-lazy expr)
    (make-lazily-created-stream 'stream-lazy (lambda () expr))]
   [(stream-lazy #:who who-expr expr)
    (make-lazily-created-stream (or who-expr 'stream-lazy) (lambda () expr))]))


;; Redefine full forcing to follow a cached immediate delayed result.
;; Forces a lazily constructed stream to a stream of any other kind
(define (stream-force s)
  (cond
    [(lazily-created-stream? s)
     (let loop ([s s] [dep-ses '()])
       (cond
         [(not (lazily-created-stream-creator-if-unforced s))
          (define v (lazily-created-stream-thunk-or-stream s))
          ;; A one-step operation may have cached another delayed stream.
          ;; Keep the same two-state representation and follow that link.
          (if (lazily-created-stream? v)
              (loop v (cons s dep-ses))
              (begin
                (for ([dep-s (in-list dep-ses)])
                  (set-lazily-created-stream-creator-if-unforced! dep-s #f)
                  (set-lazily-created-stream-thunk-or-stream! dep-s v))
                v))]
         [else
          (define thunk (lazily-created-stream-thunk-or-stream s))
          (set-lazily-created-stream-thunk-or-stream! s reentrant-error)
          (define v (thunk))
          (cond
            [(lazily-created-stream? v)
             ;; try again
             (loop v (cons s dep-ses))]
            [(for:stream? v)
             ;; any other kind of stream is success
             (set-lazily-created-stream-creator-if-unforced! s #f)
             (set-lazily-created-stream-thunk-or-stream! s v)
             (if (null? dep-ses)
                 v
                 (loop s dep-ses))]
            [else
             (define who (lazily-created-stream-creator-if-unforced s))
             (if (symbol? who)
                 (raise-arguments-error
                  who
                  "delayed expression produced a non-stream"
                  "result" v)
                 (raise-arguments-error*
                  'stream-cons
                  'racket/primitive
                  "rest expression produced a non-stream"
                  "rest result" v))])]))]
    [(for:stream? s) s]
    [else (raise-argument-error 'stream-force "stream?" s)]))


;; Support definitions for the local delayed-node methods and constructors.
(define (stream-null? obj)
  (for:stream-empty? (stream-force obj)))

(define-syntax stream-cons
  (syntax-rules ()
    [(_ head rest)
     (native-stream-cons head #:eager (stream-lazy #:who #t rest))]
    [(_ #:eager head rest)
     (native-stream-cons #:eager head #:eager (stream-lazy #:who #t rest))]
    [(_ head #:eager rest)
     (native-stream-cons head #:eager rest)]
    [(_ #:eager head #:eager rest)
     (native-stream-cons #:eager head #:eager rest)]))

(define (stream-car strm)
  (let ([v (stream-force strm)])
    (if (eagerly-created-stream? v) ; shortcut
        (stream-force-first v)
        (for:stream-first v))))

(define (stream-cdr strm)
  (let ([v (stream-force strm)])
    (if (eagerly-created-stream? v) ; shortcut
        (eagerly-created-stream-rest v)
        (for:stream-rest v))))


;; Experimental extensions. A completed node can cache an immediate delayed
;; result; ordinary stream-force still normalizes and compresses the chain.
;; No new structure fields, state tags, promises, or side tables are used.
(define (cached-endpoint source)
  (let loop ([source source])
    (if (and (lazily-created-stream? source)
             (not (lazily-created-stream-creator-if-unforced source)))
        (loop (lazily-created-stream-thunk-or-stream source))
        source)))

(define (stream-inspect source)
  (define s (cached-endpoint source))
  (cond
    [(eagerly-created-stream? s)
     (cond [(eagerly-created-stream-first-forced? s) 'ready]
           [(eq? (eagerly-created-stream-first s) reentrant-error) 'blocked]
           [else 'head-lazy])]
    [(lazily-created-stream? s)
     (if (eq? (lazily-created-stream-thunk-or-stream s) reentrant-error)
         'blocked 'lazy)]
    [(or (null? s) (eq? s for:empty-stream)) 'empty]
    [(pair? s) (if (list? s) 'ready
                   (raise-argument-error 'stream-inspect "stream?" source))]
    [(for:stream? s) 'unknown]
    [else (raise-argument-error 'stream-inspect "stream?" source)]))

;; Compress only links already evaluated before this call. stop is the
;; endpoint found before the step; target may be its newly cached result.
;; A second traversal avoids allocating a list of visited nodes.
(define (compress-cached-prefix! source stop target)
  (let loop ([current source])
    (when (and (not (eq? current stop))
               (lazily-created-stream? current)
               (not (lazily-created-stream-creator-if-unforced current)))
      (define next (lazily-created-stream-thunk-or-stream current))
      (set-lazily-created-stream-thunk-or-stream! current target)
      (loop next))))

(define (stream-force-once source)
  (define s (cached-endpoint source))
  (cond
    [(lazily-created-stream? s)
     (define who (lazily-created-stream-creator-if-unforced s))
     (define thunk (lazily-created-stream-thunk-or-stream s))
     (set-lazily-created-stream-thunk-or-stream! s reentrant-error)
     (define result (thunk))
     (unless (for:stream? result)
       (raise-arguments-error (if (symbol? who) who 'stream-cons)
                              "delayed expression produced a non-stream"
                              "result" result))
     ;; Existing cached links are acyclic. A new edge can create a cycle
     ;; only if its target already reaches s. Do not execute target thunks.
     (let check ([target result])
       (cond
         [(eq? target s) (reentrant-error)]
         [(and (lazily-created-stream? target)
               (not (lazily-created-stream-creator-if-unforced target)))
          (check (lazily-created-stream-thunk-or-stream target))]))
     (set-lazily-created-stream-thunk-or-stream! s result)
     (set-lazily-created-stream-creator-if-unforced! s #f)
     (compress-cached-prefix! source s result)
     result]
    [(for:stream? s)
     (compress-cached-prefix! source s s)
     s]
    [else (raise-argument-error 'stream-force-once "stream?" source)]))
