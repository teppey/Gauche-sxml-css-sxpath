;;;
;;;  css-sxpath - CSS3 Selector to SXPath Converter
;;;

(define-module sxml.css-sxpath
  (use sxml.sxpath)
  (use sxml.tools)
  (use srfi-1)
  (use srfi-13)
  (export css-sxpath
          if-css-sxpath
          if-car-css-sxpath
          car-css-sxpath))
(select-module sxml.css-sxpath)

;;----------------------------------------------------------
;; Utility functions
;;

(define (flatten lst)
  (cond ((null? lst) '())
        ((not (pair? (car lst)))
         (cons (car lst) (flatten (cdr lst))))
        (else
          (append (flatten (car lst))
                  (flatten (cdr lst))))))

(define (string-value s)
  (let1 s (string-trim-both s)
    (cond ((#/^\"([^\"]*)\"/ s) => (cut <> 1))
          ((#/^\'([^\']*)\'/ s) => (cut <> 1))
          (else s))))


;;----------------------------------------------------------
;; Parse result
;;
(define (make-result value rest) (list value rest))
(define (result-value x) (car x))
(define (result-rest x) (cadr x))
(define (success? x) (not (null? x)))
(define (fail? x) (null? x))
(define fail '())


;;----------------------------------------------------------
;; Primitives
;;
(define (return val)
  (lambda (input)
    (make-result val input)))

(define (>>= p f)
  (lambda (input)
    (let1 r (p input)
      (if (success? r)
        ((f (result-value r))
         (result-rest r))
        fail))))

;;----------------------------------------------------------
;; Combinators
;;
(define-syntax &do
  (syntax-rules ()
    [(_ () expr) expr]
    [(_ ((v <- p) . more) expr)
     (>>= p (lambda (v)
                (&do more expr)))]
    [(_ ((action) . more) expr)
     (>>= action (lambda (_)
                    (&do more expr)))]))

(define (&seq p q)
  (lambda (input)
    (let1 r (p input)
      (if (success? r)
        (let1 s (q (result-rest r))
          (if (success? s)
            (make-result (list (result-value r) (result-value s))
                         (result-rest s))
            fail))
        fail))))

(define (&or . ps)
  (cond ((null? ps)
         (lambda (input) fail))
        (else
          (lambda (input)
            (let1 r ((car ps) input)
              (if (success? r)
                r
                ((apply &or (cdr ps)) input)))))))

(define (&many p)
  (lambda (input)
    (let loop ((input input)
               (acc '()))
      (let1 r (p input)
        (if (fail? r)
          (make-result (reverse acc) input)
          (loop (result-rest r)
                (cons (result-value r) acc)))))))

(define (&many1 p)
  (&do ((a <- p)
        (b <- (&many p)))
    (return (cons a b))))

(define (&optional p)
  (lambda (input)
    (let1 r (p input)
      (if (success? r)
        r
        (make-result #f input)))))

(define (&peek p)
  (lambda (input)
    (let1 r (p input)
      (if (success? r)
        (make-result (result-value r) input)
        fail))))

;;----------------------------------------------------------
;; Parsers
;;
(define (p:empty input)
  (if (null? input)
    (make-result #t input)
    fail))

(define (p:anychar input)
  (if (not (null? input))
    (make-result (car input) (cdr input))
    fail))

(define (p:char c)
  (define test
    (cond ((char-set? c) (pa$ char-set-contains? c))
          ((char? c) (pa$ eqv? c))
          (else (error "argument must be char or char-set"))))
  (lambda (input)
    (or (and (not (null? input))
             (test (car input))
             (make-result (car input) (cdr input)))
        fail)))

(define p:star (p:char #\*))
(define p:lparen (p:char #\())
(define p:rparen (p:char #\)))
(define p:lbracket (p:char #\[))
(define p:rbracket (p:char #\]))
(define p:backslash (p:char #\\))
(define white (char-set #\space #\tab #\return #\newline #\page))
(define p:white (p:char white))
(define p:escape
  (&do ([p:backslash]
        [ch <- (p:char #[^\x0a\x0d\x0c0-9a-fA-F])])
    (return ch)))
(define p:nmchar
  (&or (p:char #[_a-zA-Z0-9-])
       p:escape))
(define p:nmstart
  (&or (p:char #[_a-zA-Z])
       p:escape))

(define p:space
  (&do ([a <- (&many p:white)])
    (return a)))

(define p:string2
  (&do ([(p:char #\")]
        [value <- (&many (p:char #[^\"])) ]
        [(p:char #\")])
    (return (list->string value))))

(define p:string1
  (&do ([(p:char #\')]
        [value <- (&many (p:char #[^\'])) ]
        [(p:char #\')])
    (return (list->string value))))

(define p:string
  (&or p:string1 p:string2))

(define (twice c1 c2 sval)
  (&do ([(p:char c1)]
        [(p:char c2)])
    (return sval)))
(define p:prefixmatch (twice #\^ #\= 'prefixmatch))
(define p:suffixmatch (twice #\$ #\= 'suffixmatch))
(define p:substringmatch (twice #\* #\= 'substringmatch))
(define p:includes (twice #\~ #\= 'includes))
(define p:dashmatch (twice #\| #\= 'dashmatch))

(define p:ident
  (&do ([head <- p:nmstart]
        [tail <- (&many p:nmchar)])
    (return (list->string (cons head tail)))))

(define p:name
  (&do ([name <-(&many1 p:nmchar)])
    (return (list->string name))))

(define p:element-name p:ident)

(define p:type-selector
  (&do ([name <- p:element-name])
    (return (lambda (node root vars)
              ((sxml:filter (ntype?? (string->symbol name)))
               node)))))

(define p:universal
  (&do ([star <- p:star])
    (return (lambda (node root vars)
              node))))

(define p:hash
  (&do ([(p:char #\#)]
        [name <- p:name])
    (return (lambda (node root vars)
              ((sxml:filter
                 (lambda (node)
                   (and-let* ((id-val (sxml:attr-u node 'id)))
                     (equal? id-val name))))
               node)))))

(define p:class
  (&do ([(p:char #\.)]
        [ident <- p:ident])
    (return (lambda (node root vars)
              ((sxml:filter
                 (lambda (node)
                   (and-let* ((class-val (sxml:attr-u node 'class)))
                     (member ident (string-split class-val #/\s+/)))))
               node)))))

(define p:attrib
  (&do ([p:lbracket]
        [p:space]
        [ident <- p:ident]
        [p:space]
        [args <- (&optional
                   (&do ([a <- (&or
                                 p:prefixmatch
                                 p:suffixmatch
                                 p:substringmatch
                                 (p:char #\=)
                                 p:includes
                                 p:dashmatch
                                 )]
                         [p:space]
                         [b <- (&or p:ident p:string)]
                         [p:space])
                     (return (cons a b))))]
        [p:rbracket])
    (if (not args)
      (return (lambda (node root vars)
                ((sxml:filter
                   (lambda (node)
                     (sxml:attr-u node (string->symbol ident))))
                 node)))
      (let ([type (car args)]
            [arg (cdr args)]
            [ident (string->symbol ident)])
        (define (test-attr attr-name pred)
          (lambda (node root vars)
            ((sxml:filter
               (lambda (node)
                 (and-let* ((value (sxml:attr-u node attr-name)))
                   (pred value))))
             node)))
        (case type
          [(prefixmatch)
           (return (test-attr ident (pa$ string-prefix? arg)))]
          [(suffixmatch)
           (return (test-attr ident (pa$ string-suffix? arg)))]
          [(substringmatch)
           (return (test-attr ident (cut string-scan <> arg)))]
          [(#\=)
           (return (test-attr ident (pa$ equal? arg)))]
          [(includes)
           (return
             (test-attr ident (lambda (value)
                                (member arg (string-split value #/\s+/)))))]
          [(dashmatch)
           (return (test-attr ident (any-pred
                                      (pa$ equal? arg)
                                      (pa$ string-prefix? #`",|arg|-"))))]
          [else
            (return fail)])))))

(define p:pseudo-class
  (&do ([(p:char #\:)]
        [name <- (&or
                   p:functional-pseudo
                   p:ident)])
    (cond [(equal? name "first-child")
           (return (lambda (node root vars)
                     ((node-pos 1) node)))]
          [(equal? name "last-child")
           (return (lambda (node root vars)
                     ((node-pos -1) node)))]
          [(equal? name "enabled")
           (return (lambda (node root vars)
                     (define (pred node)
                       (or (and-let* ((value (sxml:attr-u node 'enabled)))
                               (member value '("true" "enabled")))
                           (and-let* ((value (sxml:attr-u node 'disabled)))
                             (equal? value "false"))))
                     ((sxml:filter pred) node)))]
          [(equal? name "disabled")
           (return (lambda (node root vars)
                     (define (pred node)
                       (or (and-let* ((value (sxml:attr-u node 'disabled)))
                               (member value '("true" "disabled")))
                           (and-let* ((value (sxml:attr-u node 'enabled)))
                             (equal? value "false"))))
                     ((sxml:filter pred) node)))]
          [(equal? name "checked")
           (return (lambda (node root vars)
                     (define (pred node)
                       (and-let* ((value (sxml:attr-u node 'checked)))
                         (member value '("true" "checked"))))
                     ((sxml:filter pred) node)))]
          [else
            ;functional-pseudo
            (return name)])))

(define p:function
  (&do ([ident <- p:ident]
        [p:lparen])
    (return ident)))

(define p:functional-pseudo
  (&do ([fname <- p:function]
        [farg <- (&many1 (p:char #[^\x29]))] ; except rparen
        [p:rparen])
    (let1 farg (string-trim-both (list->string farg))
      (cond [(equal? fname "nth-child")
             (return (lambda (node root vars)
                       ((node-pos (x->integer farg)) node)))]
            [(equal? fname "not")
             (cond
               [(#/^\[\s*([^~\|=\s]+)\s*([~\|]?=)(.+)\]$/ farg)
                => (lambda (m)
                     (cond
                       ((equal? (m 2) "~=")
                        [return
                          (lambda (node root vars)
                            ((sxml:filter
                               (lambda (node)
                                 (not
                                   (and-let* ((name (string->symbol (m 1)))
                                              (value (sxml:attr-u node name)))
                                     (member (string-value (m 3))
                                             (string-split value #/\s+/))))))
                             node))])
                       [(equal? (m 2) "|=")
                        (return
                          (lambda (node root vars)
                            ((sxml:filter
                               (lambda (node)
                                 (not
                                   (and-let* ((name (string->symbol (m 1)))
                                              (value (sxml:attr-u node name)))
                                     (or (equal? value (string-value (m 3)))
                                         (string-prefix? #`",(string-value (m 3))-" value))))))
                             node)))]
                       [else
                         (return
                           (lambda (node root vars)
                             ((sxml:filter
                                (lambda (node)
                                  (not
                                    (and-let* ((name (string->symbol (m 1)))
                                               (value (sxml:attr-u node name)))
                                      (equal? value (string-value (m 3)))))))
                              node)))]))]
               [(#/^\[(.+?)\]$/ farg)
                => (lambda (m)
                     (return
                       (lambda (node root vars)
                         ((sxml:filter
                            (lambda (node)
                              (not
                                (sxml:attr-u node (string->symbol (m 1))))))
                          node))))]
               [else
                 ; unsupported negation arg
                 (return fail)])]
            [else
              ; unsupported function
              (return fail)]))))

(define p:simple-selector-seqence
  (&or
    (&do ([a <- (&optional
                  (&or
                    p:type-selector
                    p:universal))]
          [b <- (&many1
                  (&or
                    p:hash
                    p:class
                    p:attrib
                    p:pseudo-class))])
      (if a
        (return (cons a b))
        (return (cons (result-value (parse p:universal "*"))
                      b))))
    p:type-selector
    p:universal))

(define p:combinator
  (&do ([a <- (&or
                (&do ([p:space]
                      [b <- (&or
                              (p:char #\+)
                              (p:char #\>)
                              (p:char #\~))]
                      [p:space])
                  (return b))
                (&do ([c <- (&many1 p:white)])
                  (return 'white)))])
    (case a
      [(#\>)
       (return (lambda (node root vars)
                 (sxml:child-elements node)))]
      [(#\+)
       (return (lambda (node root vars)
                 (let1 following-sibling
                   (((sxml:following-sibling (ntype?? '*))
                     root) node)
                   ((node-pos 1) following-sibling))))]
      [(#\~)
       (return (lambda (node root vars)
                 (((sxml:following-sibling (ntype?? '*))
                   root) node)))]
      [(white)
       (return (lambda (node root vars)
                 ((sxml:descendant (ntype?? '*)) node)))]
      [else  (return fail)])))

(define p:selector
  (&or
    ; "* E ..."
    (&do ([p:space]
          [p:universal]
          [(&many1 p:white)]
          [a <- (&many1 p:simple-selector-seqence)])
      (return (cons (lambda (node root vars)
                      ((sxml:descendant (ntype?? '*)) node))
                    a)))
    ; "E ..."
    (&do ([p:space]
          [a <- p:simple-selector-seqence]
          [b <- (&many
                  (&do ([c <- p:combinator]
                        [p:space]
                        [d <- p:simple-selector-seqence])
                    (return (list c d))))]
          [p:space])
      (return (list (lambda (node root vars)
                      ((sxml:descendant (ntype?? '*)) node))
                    (cons a b))))))

(define p:selectors-group
  (&do ([first <- p:selector]
        [rest  <- (&many
                    (&do ([p:space]
                          [(p:char #\,)]
                          [p:space]
                          [a <- p:selector])
                      (return a)))])
    (return (cons first rest))))


;;----------------------------------------------------------
;; Execute parser
;;
(define (parse parser selector)
  (parser (string->list selector)))

;; CSS Selector -> Converter
(define (compile-selector selector)
  (let1 r (parse p:selectors-group selector)
    (if (and (success? r) (null? (result-rest r)))
      (apply node-or (map (lambda (path)
                            (sxpath (flatten path)))
                          (result-value r)))
      (error "parse error" selector))))

(define (css-sxpath selector)
  (cond ((string? selector)
         (compile-selector selector))
        ((pair? selector)
         (apply node-join
                (map (lambda (elt)
                       (cond ((or (string? elt) (pair? elt))
                              (css-sxpath elt))
                             ((symbol? elt)
                              (sxpath (list elt)))
                             ((procedure? elt)
                              (sxpath (list elt)))
                             (else
                               (error "bad path step" elt))))
                     selector)))
        (else
          (error "argument must be string or list"))))

(define (if-css-sxpath selector)
  (lambda (sxml)
    (let1 x ((css-sxpath selector) sxml)
      (if (null? x) #f x))))

(define (if-car-css-sxpath selector)
  (lambda (sxml)
    (let1 x ((css-sxpath selector) sxml)
      (if (null? x) #f (car x)))))

(define (car-css-sxpath selector)
  (lambda (sxml)
    (let1 x ((css-sxpath selector) sxml)
      (if (null? x) '() (car x)))))



(provide "sxml/css-sxpath")
;;; vim:set lispwords+=&do:
