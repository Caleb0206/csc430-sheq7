#lang typed/racket
(require typed/rackunit)

;; SHEQ7
;; Status message

;; Data definitions

;; Value - Numbers, Booleans, String, CloV, PrimV, ArrayV, NullV
(define-type Value (U Real Boolean String CloV PrimV ArrayV NullV))

;; NullV - contains nothing
(struct NullV () #:transparent)

;; ArrayV - Array contains and an address and a size, both of Natural types
(struct ArrayV ([address : Natural] [size : Natural]) #:transparent)

;; CloV - Closures contain list of symbol params, body of ExprC, Env
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)

;; PrimV - Represents a primitive operator by its symbol
(struct PrimV ([op : Symbol]) #:transparent)
 
;; LamC - Lambdas contain a list of symbol args, and a body of ExprC
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)

;; Binding : pair of a Symbol and a Natural address
(struct Binding ([name : Symbol] [address : Natural]) #:transparent)

;; Env : a list of Bindings
(define-type Env (Listof Binding))

;; ExprC type : NumC, IfC, IdC, AppC, LamC, StringC, MutateC, NullC
(define-type ExprC (U NumC IfC IdC AppC LamC StringC MutateC NullC))

;; NumC : a Real
(struct NumC ([n : Real]) #:transparent)

;; StringC : a String
(struct StringC ([s : String]) #:transparent)

;; IdC : a symbol representing an ID
(struct IdC ([name : Symbol]) #:transparent)

;; IfC : an if statement of ExprC, and ExprC's to act on if true or false
(struct IfC ([v : ExprC] [iftrue : ExprC] [iffalse : ExprC]) #:transparent)

;; AppC : Represents a function application.function ExprC with a list of arg ExprC's
(struct AppC ([expr : ExprC] [args : (Listof ExprC)]) #:transparent)

;; MutateC : A mutation of symbol id to an ExprC expr
(struct MutateC ([id : Symbol] [expr : ExprC]) #:transparent)

;; NullC : Null
(struct NullC () #:transparent)

;; Types - Number, Boolean, String, Array of Integers, Functions
(define-type Type (U NumT BoolT StrT IntArrayT FunT))

;; NumT - Number type
(struct NumT () #:transparent)

;; BoolT - Boolean type
(struct BoolT () #:transparent)

;; StrT - String type
(struct StrT () #:transparent)

;; IntArrayT - Integer Array type
(struct IntArrayT () #:transparent)

;; FunT - Function type with arguments and return types
(struct FunT ([args : (Listof Type)] [return : Type]) #:transparent)

;; parse-type
(define (parse-type [s : Sexp]) : Type
  (match s
    ['num (NumT)]
    ['bool (BoolT)]
    ['str (StrT)]
    ['intarray (IntArrayT)]

    ; (list (list (? symbol? args) '= vals) ...)
    [(list args ... '-> return)
     (FunT (map parse-type (cast args (Listof Sexp))) (parse-type return))]
    [_ (error 'parse-type "SHEQ: invalid type syntax ~a" s)]))

(check-equal? (parse-type 'num) (NumT))
(check-equal? (parse-type 'bool) (BoolT))
(check-equal? (parse-type 'str) (StrT))
(check-equal? (parse-type 'intarray) (IntArrayT))
(check-equal? (parse-type '{num str -> bool}) (FunT (list (NumT) (StrT)) (BoolT)))
(check-exn #rx"SHEQ: invalid type syntax" (lambda () (parse-type 22)))

;; TypeBinding - a binding of a Symbol and a Type
(struct TypeBinding ([name : Symbol] [ty : Type]) #:transparent)

;; TypeEnv - a list of TypeBindings
(define-type TypeEnv (Listof TypeBinding))

;; type-check - takes an ExprC and TypeEnv, returns the ExprC's Type
(define (type-check [e : ExprC] [tenv : TypeEnv]) : Type
  (match e
    [(NumC _) (NumT)]
    [(StringC _) (StrT)]
    [(IdC name) (lookup-type name tenv)]))


;; lookup-type - takes and symbol and a TypeEnv, returns the symbol's type through search in TypeEnv
(define (lookup-type [id : Symbol] [tenv : TypeEnv]) : Type
  (match tenv
    ['() (error 'lookup-type "SHEQ: An unbound identifier ~a in env ~e" id tenv)]
    [(cons (TypeBinding name type) r)
     (if (equal? id name)
         type
         (lookup-type id r))]))

;; lookup-type tests
(check-equal? (lookup-type 'x (list (TypeBinding 'x (BoolT)))) (BoolT))
(check-exn #rx"SHEQ: An unbound identifier " (lambda () (lookup-type 'x (list (TypeBinding 'str (StrT))))))

;; type-check tests
(check-equal? (type-check (NumC 5) (list (TypeBinding 'num (NumT)))) (NumT))
(check-equal? (type-check (StringC "ha") '()) (StrT))
(check-equal? (type-check (IdC 'z) (list (TypeBinding 'yes? (BoolT)) (TypeBinding 'z (NumT)))) (NumT))


;; Top level environment definitions
(define top-env-defs (list
                      (list 'true #t)
                      (list 'false #f)
                      (list '+ (PrimV '+))
                      (list '- (PrimV '-))
                      (list '* (PrimV '*))
                      (list '/ (PrimV '/))
                      (list '<= (PrimV '<=))
                      (list 'equal? (PrimV 'equal?))
                      (list 'substring (PrimV 'substring))
                      (list 'strlen (PrimV 'strlen))
                      ; (list 'error (PrimV 'error))
                      (list 'println (PrimV 'println))
                      (list 'read-num (PrimV 'read-num))
                      (list 'read-str (PrimV 'read-str))
                      (list '++ (PrimV '++))
                      (list 'make-array (PrimV 'make-array))
                      (list 'array (PrimV 'array))
                      (list 'aref (PrimV 'aref))
                      (list 'aset! (PrimV 'aset!)))) 

;; reserved-keywords - a list of key-words
(define reserved-keywords '(if lambda let = in end : := rlet ->))

;; ---- Interpreters ----

;; top-interp - Parse and evaluate the S-exp, return a serialized String result
(define (top-interp [s : Sexp]) : String
  (define store (make-initial-store 2000))
  (define env (make-default-env store))
  (serialize (interp (parse s) env store)))

;; interp - takes the complete AST (ExprC) with an Env, returning a Value
(define (interp [e : ExprC] [env : Env] [store : (Vectorof Value)]) : Value
  ; template
  #;(match e
      [numc -> number]
      [stringc -> s]
      [nullC -> nullV]
      [mutatec -> nullv]
      [ifc -> eval if expr]
      [LamC -> CloV params body env]
      [AppC -> interp CloV or PrimV]
      [Idc -> get binding])

  ; body
  (match e
    [(NumC n) n]
    [(StringC s) s]
    [(NullC) (NullV)]
    [(MutateC id expr) 
     (store-set! id (interp expr env store) env store)]
    [(IfC v if-t if-f)
     (define test-val (interp v env store))
     (cond
       [(boolean? test-val)
        (if test-val
            (interp if-t env store)
            (interp if-f env store))]
       [else (error 'interp "SHEQ: If expected boolean test, got ~a" test-val)])]
    [(LamC params body) (CloV params body env)]
    [(AppC (IdC 'seq) exprs)
     (interp-seq exprs env store)]
    [(AppC lam args)
     (define f-val (interp lam env store))
     (define arg-vals
       (for/list : (Listof Value) ([a args])
         (interp a env store)))
     (cond
       [(CloV? f-val)
        (if (equal? (length arg-vals) (length (CloV-params f-val)))
            (interp (CloV-body f-val)
                    ; Append new Closure env
                    (append (map (lambda ([id : Symbol] [v : Value]) 
                                   (Binding id (allocate store v))) 
                                 (CloV-params f-val) 
                                 arg-vals)
                            (CloV-env f-val))
                    store)
            (error 'interp "SHEQ: Incorrect number of arguments for CloV, got ~a expected ~a"
                   (length (CloV-params f-val))
                   (length arg-vals)))]
        
       [(PrimV? f-val)
        (interp-prim f-val arg-vals store)]
       [else
        (error 'interp "SHEQ: Attempted to apply non function value ~a" f-val)])]         
    [(IdC id) (store-get id env store)]))

;; interp-seq - takes a list of ExprC's to interpret them sequentially, returns the last expression's Value
(define (interp-seq [exprs : (Listof ExprC)] [env : Env] [store : (Vectorof Value)]) : Value
  (match exprs
    ['() (error 'interp "SHEQ: seq needs at least 1 expression.")]
    [(list f) (interp f env store)]
    [(cons f rest) (begin (interp f env store) (interp-seq rest env store))]))

;; interp-prim - takes a PrimV and a list of Values, returns a Value
(define (interp-prim [p : PrimV] [args : (Listof Value)] [store : (Vectorof Value)]) : Value
  (match (PrimV-op p)
    ['+
     (match args
       [(list a b)
        (cond
          [(and (real? a) (real? b)) (+ a b)]
          [else (error 'interp-prim "SHEQ: PrimV + expected 2 numbers, got ~a" args)])]
       [_ (error 'interp-prim "SHEQ: + received incorrect number of arguments, expected 2, got ~a" (length args))])]
    ['*
     (match args
       [(list a b)
        (cond
          [(and (real? a) (real? b)) (* a b)]
          [else (error 'interp-prim "SHEQ: PrimV * expected 2 numbers, got ~a" args)])]
       [_ (error 'interp-prim "SHEQ: * received incorrect number of arguments, expected 2, got ~a" (length args))])]
    ['-
     (match args
       [(list a b)
        (cond
          [(and (real? a) (real? b)) (- a b)]
          [else (error 'interp-prim "SHEQ: PrimV - expected 2 numbers, got ~a" args)])]
       [_ (error 'interp-prim "SHEQ: - received incorrect number of arguments, expected 2, got ~a" (length args))])]
    ['/
     (match args
       [(list a b)
        (cond
          [(and (real? a) (real? b) (not (equal? b 0))) (/ a b)]
          [(and (real? a) (real? b) (equal? b 0))
           (error 'interp-prim "SHEQ: Divide by zero error")]
          [else (error 'interp-prim "SHEQ: PrimV / expected 2 numbers, got ~a" args)])]
       [_ (error 'interp-prim "SHEQ: / received incorrect number of arguments, expected 2, got ~a"
                 (length args))])]
    ['<=
     (match args
       [(list a b)
        (cond
          [(and (real? a) (real? b)) (<= a b)]
          [else (error 'interp-prim "SHEQ: PrimV <= expected 2 numbers, got ~a" args)])]
       [_ (error 'interp-prim "SHEQ: <= received incorrect number of arguments, expected 2, got ~a"
                 (length args))])]
    ['equal?
     (match args
       [(list a b)
        (cond [(or (CloV? a) (CloV? b) (PrimV? a) (PrimV? b)) #f]
              [else (equal? a b)])]
       [_ (error 'interp-prim "SHEQ: equal? received incorrect number of arguments, expected 2, got ~a"
                 (length args))])]
    ['substring
     (match args
       [(list s start stop)
        (cond
          [(and (string? s)
                (natural? start)
                (natural? stop)
                (>= start 0)
                (< start (string-length s))
                (>= stop 0)
                (<= stop (string-length s)))
           (substring s (inexact->exact start) (inexact->exact stop))]
          [else
           (error 'interp-prim "SHEQ: substring needs string and 2 valid natural indices, got ~a" args)])]
       [_ (error 'interp-prim "SHEQ: substring received incorrect number of arguments, expected 3, got ~a"
                 (length args))])]
    ['strlen
     (match args
       [(list s)
        (if (string? s)
            (string-length s)
            (error 'interp-prim "SHEQ: Syntax error, ~a is not a string" s))]
       [_ (error 'interp-prim "SHEQ: strlen received incorrect number of arguments, expected 1, got ~a"
                 (length args))])]
    #; ['error
     (match args
       [(list v)
        (error 'interp-prim "SHEQ: user-error ~a" (serialize v))]
       [_ (error 'interp-prim "SHEQ: error received incorrect number of arguments, expected 1, got ~a"
                 (length args))])]
    ['println
     (match args
       [(list s)
        (if (string? s)
            (begin
              (displayln s)
              #t)
            (error 'interp-prim "SHEQ: Attempted to print a non-string value, got ~a" s))]
       [_ (error 'interp-prim "SHEQ: println received incorrect number of arguments, expected 1, got ~a"
                 (length args))])]
    ['read-num
     (match args
       ['()
        (begin
          (display "> ")
          (define input (read-line))
          (cond
            [(eof-object? input)
             (error 'interp-prim "SHEQ: read-num read EOF")]
            [else
             (define num (string->number input))
             (if (real? num)
                 num
                 (error 'interp-prim "SHEQ: read-num expected a Number, got ~a" input))]))]
       [_ (error 'interp-prim "SHEQ: read-num received incorrect number of arguments, expected 0, got ~a"
                 (length args))])]
    ['read-str
     (match args
       ['()
        (begin
          (display "> ")
          (define input (read-line))
          (if (eof-object? input)
              (error 'interp-prim "SHEQ: read-str read EOF")
              input))]
       [_ (error 'interp-prim "SHEQ: read-str received incorrect number of arguments, expected 0, got ~a"
                 (length args))])]
    ['++
     (match args
       ['() ""]
       [_ (apply string-append (map val->string args))])]
    ['make-array
     (match args
       [(list size val)
        (cond
          [(and (natural? size) (>= size 1))
           (define len (inexact->exact size))
           ; Shouldn't make vectors. Should just create ArrayV
           ; Sets the address to the address after where this array will be stored
           (define arr (ArrayV (+ 1 (next-address store)) len))
           (allocate store arr)
           (allocate-lst store (for/list ([i (in-range len)]) val))
           arr]
          [(and (natural? size) (< size 1))
           (error 'interp-prim "SHEQ: make-array expected size 1 or greater, got ~a" size)]
          [(not (natural? size))
           (error 'interp-prim "SHEQ: make-array expected a natural number for size, got ~a" size)]
          )]
       [_ (error 'interp-prim "SHEQ: make-array received incorrect number of arguments, expected 2, got ~a"
                 (length args))])]
    ['array
     (match args
       ['() (error 'interp-prim "SHEQ: array expected at least one element, got ~a" args)]
       [_
        (define len (length args))
        (define arr (ArrayV (+ 1 (next-address store)) len))
        (allocate store arr)
        (allocate-lst store args)
        arr])]
    ['aref
     (match args
       [(list arr index)
        (cond
          [(not (ArrayV? arr))
           (error 'interp-prim "SHEQ: aref expected an array, got ~a" arr)]
          [(not (integer? index))
           (error 'interp-prim "SHEQ: aref expected an integer for index, got ~a" index)]
          [(or (< index 0) (>= index (ArrayV-size arr)))
           (error 'interp-prim "SHEQ: aref index out of bounds. ~a index for array of size ~a"
                  index
                  (ArrayV-size arr))]
          [else
           ; Add the index to the array's address and return that value in the store
           (vector-ref store (assert (+ index (ArrayV-address arr)) natural?))])]
       [_ (error 'interp-prim "SHEQ: aref received incorrect number of arguments, expected 2, got ~a" (length args))])]
    ['aset!
     (match args
       [(list arr index val)
        (cond
          [(not (ArrayV? arr))
           (error 'interp-prim "SHEQ: aset! expected an array, got ~a" arr)]
          [(not (integer? index))
           (error 'interp-prim "SHEQ: aset! expected an integer for index, got ~a" index)]
          
          [(or (< index 0) (>= index (ArrayV-size arr)))
           (error 'interp-prim "SHEQ: aset! index out of bounds. ~a index for array of size ~a"
                  index
                  (ArrayV-size arr))]
          [else
           (vector-set! store (assert (+ index (ArrayV-address arr)) natural?) val)
           (NullV)])]
       [_ (error 'interp-prim "SHEQ: aset! received incorrect number of arguments, expected 3, got ~a"
                 (length args))])]
    [_
     (error 'interp-prim "SHEQ: Invalid PrimV op, got ~a" args)]))


;; ---- Parser ---- 
;; parse - takes a S-exp and returns concrete syntax in ExprC AST
(define (parse [e : Sexp]) : ExprC
  ; template
  #;(match e
      [number -> NumC]
      [string -> StringC]
      ['null -> NullC]
      [not reserved symbol -> idc]
      [list 'id ':= expr -> mutatec]
      [list 'let ... -> AppC(LamC)]
      [list 'if ... -> IfC]
      [list 'lambda ... -> LamC]
      [list f args -> AppC]
      [else -> throw unknown error])
  ; body
  (match e
    ;; Match Real
    [(? real? n) (NumC n)]
    ;; Match String
    [(? string? s) (StringC s)]
    ;; Match Null
    ['null (NullC)]
    ;; Match Id
    [(? symbol? name)
     (if (reserved-symbol? name)
         (error 'parse "SHEQ: Syntax error, unexpected reserved keyword, got ~e" name)
         (IdC name))]
    ;; Match mutation
    [(list (? symbol? id) ':= expr) 
     (if (reserved-symbol? id)
         (error 'parse "SHEQ: Syntax error, cannot mutate reserved keyword, ~e" id)
         (MutateC id (parse expr)))]
    ;; Match Let
    [(list 'let 
           (list (list (? symbol? args) '= vals) ...) 
           'in
           in-body
           'end)
     (define args-list (cast args (Listof Symbol)))
     (cond
       [(not (distinct-args? args-list))
        (error 'parse "SHEQ: Let binding list is invalid, duplicate variables ~a" args-list)]
       [(ormap reserved-symbol? args-list)
        (error 'parse "SHEQ: let binding list is invalid, reserved symbol was used ~a" args-list)]
       [else (AppC 
              (LamC args-list (parse in-body)) 
              (for/list : (Listof ExprC) ([v vals]) 
                (parse (cast v Sexp))))])]
    ;; Match If
    [(list 'if v iftrue iffalse)
     (IfC (parse v) (parse iftrue) (parse  iffalse))]
    ;; Match Lambda
    [(list 'lambda (list (? symbol? args) ...) ': body)
     (define args-list (cast args (Listof Symbol)))
     (if (distinct-args? args-list)
         (LamC args-list (parse body))
         (error 'parse "SHEQ: Lambda args list is invalid, duplicate parameters found, ~a" args-list))]
    ;; Match Application
    [(list f args ...)
     (AppC (parse f) (for/list : (Listof ExprC) ([a args]) (parse a)))]
    [other (error 'parse "SHEQ: Syntax error, got ~e" other)]))


;; serialize - takes a Value and returns a serialized String
(define (serialize [v : Value]) : String
  (match v
    [(? real? r) (~v r)]
    [(? boolean? b) (if b
                        "true"
                        "false")]
    [(? string? s) (~v s)]
    [(NullV) "null"]
    [(CloV _ _ _) "#<procedure>"]
    [(PrimV _) "#<primop>"]
    [(ArrayV addr size) (string-append "#<array>")]))

;; val->string - converts a Value to a string
(define (val->string [v : Value]) : String
  (match v
    [(? real? r) (~a r)]
    [(? boolean? b) (if b
                        "true"
                        "false")]
    [(? string? s) (~a s)]
    [(CloV _ _ _) "#<procedure>"]
    [(PrimV _) "#<primop>"]
    [(ArrayV _ _) "#<array>"]))

;; ---- Helper functions ----

;; get-binding takes a symbol and enviornment, performs a lookup and returns a Natural index if found
(define (get-binding [s : Symbol] [env : Env]) : Natural
  (match env
    ['() (error 'get-binding "SHEQ: An unbound identifier ~a in env ~e" s env)]
    [(cons (Binding name val) r)
     (if (equal? s name)
         val
         (get-binding s r))]))

;; distinct-args? - returns true if every symbol in args is distinct 
(define (distinct-args? [args : (Listof Symbol)]) : Boolean
  (not (check-duplicates args)))

;; reserved-symbol? - Determines if a given symbol is in the reserved keywords
(define (reserved-symbol? [s : Symbol]) : Boolean
  (if (memq s reserved-keywords)
      #t
      #f))

;; make-initial-store - takes a Natural number size, returns a Vector of Values where index 0 is equal to 1
(define (make-initial-store [size : Natural]) : (Vectorof Value)
  (define env-size (length top-env-defs))
  (define stre : (Mutable-Vectorof Value) (make-vector (+ size env-size) 0))
  (vector-set! stre (ann 0 Natural) 1)
  stre)

;; make-default-env - takes a Vector of Values and creates a list of Bindings for the top-env-defs list
(define (make-default-env [stre : (Vectorof Value)]) : Env
  (for/list ([bind top-env-defs])
    (match bind [(list id val) (Binding id (allocate stre val))])))

;; store-get - Given an id, env, and Vector of Values, returns the Value in the 
;; Vector at the index defined by the env for the id.
(define (store-get [id : Symbol] [env : Env] [stre : (Vectorof Value)]) : Value
  (define addr (get-binding id env))
  (vector-ref stre addr))

;; store-set! - Given an id, new Value, env, and Vector of Values, sets the Value
;; at the index defined by the env for the id.
(define (store-set! [id : Symbol] [new-v : Value] [env : Env] [stre : (Vectorof Value)]) : NullV
  (define addr (get-binding id env))
  (vector-set! stre addr new-v)
  (NullV))

;; allocate - takes a Vector of Values and a Value to store, returns the Natural pointer to the stored Value's address
(define (allocate [stre : (Vectorof Value)] [val : Value]) : Natural
  (define next-free (next-address stre))

  (if (>= next-free (vector-length stre))
      (error 'allocate "SHEQ: Out of memory. Tried to allocate space for ~a." val)
      (vector-set! stre (ann next-free Natural) val))
  ; Update next free location in store
  (vector-set! stre (ann 0 Natural) (+ 1 next-free))
  next-free)

;; allocate-lst - takes a Vector of Values and a list of Values to store,
;; returns a Natural pointer to the last stored Value's address
(define (allocate-lst [stre : (Vectorof Value)] [vals : (Listof Value)]) : Natural
  (define next-free (next-address stre))
  (define new-free (+ next-free (length vals)))
  (if (>= new-free (vector-length stre))
      (error 'allocate-lst "SHEQ: Out of memory. Tried to allocate ~a cells for list ~a in store."
             (length vals)
             vals)
      (for ([v vals]) (allocate stre v)))
  new-free)

;; next-address - takes a Store and returns the next store address (index 0 of the store)
(define (next-address [stre : (Vectorof Value)]) : Natural
  (assert (vector-ref stre (ann 0 Natural)) natural?))



;; ---- Tests ----
;; while : SHEQ implementation of the while loop
(define while '{let {[while = "undefined"]}
                 in
                 {seq
                  {while :=
                         {lambda {guard body} :
                           {if {guard}
                               {seq
                                {body}
                                {while guard body}}
                               null}}}
                  ;; Test for while loop (commented out for Handin)
                  #; {let {[x = 0]}
                       in
                       {seq
                        {while
                         {lambda (): {<= x 2}}
                         {lambda () : 
                           {seq
                            {println {++ "" x}}
                            {x := {+ x 1}}}}}
                        x}
                       end}
                  while}
                 end})

;; Test for while (commented out for Handin)
; (check-equal? (top-interp while) "3")

(check-equal? (top-interp while) "#<procedure>")



;; in-order : accepts an array of numbers and its size, returns true if array is increasing order
(define in-order
  '{let {
         ;; commented out while function for own tests
         #; [while = "undefined"]
         [in-order = "undefined"]
         [not = {lambda (b) : {if b false true}}]
         [and = {lambda (a b) : {if a b false}}]}
     in
     {seq
      ;; commented out while function for own tests
      #; {while :=
                {lambda {guard body} :
                  {if {guard}
                      {seq
                       {body}
                       {while guard body}}
                      null}}}
      {in-order :=
                {lambda (arr size) :
                  {let {[i = 0]
                        [result = true]}
                    in
                    {seq
                     {while
                      {lambda () :
                        {and {<= i {- size 2}} result}}
                      {lambda () :
                        {if {<= {aref arr {+ i 1}} {aref arr i}}
                            {result := false}
                            {i := {+ i 1}}}}}
                     result}
                    end}}}
      ;; Commented out test for Handin
      #; {in-order {array 1 2 3} 3}
      in-order}
     end})

;; Test for in-order (commented out for Handin)
; (check-equal? (top-interp in-order) "true")
(check-equal? (top-interp in-order) "#<procedure>")


;; Large test
; The program calculates two areas using two different functions, and then compares them.
;; The result is the result of the comparison
(define prog '{
               let
                  {[square = {lambda (x) : {* x x}}]
                   [area = {lambda (w h) : {* w h}}]
                   [gt = {lambda (v1 v2 t f) : {if {<= v1 v2} 1 0}}]}
                in
                {gt {square 4} {area 4 3} 0 1}
                end})
(check-equal? (top-interp prog) "0")

;; ---- top-interp Tests ----
(check-equal? (top-interp '{+ 3 2}) "5")
(check-equal? (top-interp '{if {<= 5 100} "less than" "not less than"}) "\"less than\"")

;; - substring test - from handin server tests
(check-equal? (top-interp '{equal? (substring (substring "abcd" 1 4) 1 3) "cd"}) "true")

(check-equal? (top-interp 
               '{let {[x = 5]
                      [y = {+ 8 9}]}
                  in
                  {+ x {* y {let {[x = 3]}
                              in
                              {+ x x}
                              end}}}
                  end}) "107")

;; - top-interp seq test
(check-equal? (top-interp '{seq
                            {let ([n = 5])
                              in
                              {+ 1 n}
                              end}
                            {let ([x = 2])
                              in
                              {* 2 x}
                              end}}) "4")

;; - top-interp with arrays
(check-equal? (top-interp '{let ([arr1 = {make-array 5 0}]
                                 [arr2 = {array 0 0 0 0 0}])
                             in
                             {equal? arr1 arr2}
                             end}) "false")

(check-equal? (top-interp '{let ([arr3 = {make-array 5 0}]
                                 [f = {lambda (x) : {* x 2}}])
                             in
                             {seq
                              {aset! arr3 0 {f 10}}
                              {aref arr3 0}}
                             end}) "20")

;; - top-interp with mutations

(check-equal? (top-interp '{let {[x = 33]}
                             in
                             {seq
                              {x := "changed"}
                              x}
                             end})  "\"changed\"")

(check-equal? (top-interp '{let {[fact = "bogus"]}
                             in
                             {seq
                              {fact := {lambda {x} : {if {equal? x 0} 1 {* x {fact {- x 1}}}}}}
                              {fact 2}}
                             end}) "2")



;; - incorect num of arguments (from handin)
(check-exn #rx"SHEQ: Incorrect number of arguments for CloV"
           (lambda () (top-interp '{{lambda () : 19} 17})))

;; - divide by zero error test case (from handin)
(check-exn #rx"SHEQ: Divide by zero error"
           (lambda () (top-interp
                       '{{lambda (ignoreit) : {ignoreit {/ 52 {+ 0 0}}}} {lambda (x) : {+ 7 x}}})))

;; ---- interp tests ----
(define make-test-store (lambda () (make-initial-store 100))) ; (make-test-store) is a store for the tests below

;; helper lambda for encapsulating store and env creation on interp
(define test-interp (lambda ([expr : ExprC]) 
                      (define store (make-test-store))
                      (interp expr (make-default-env store) store)))

(check-equal? (test-interp (NullC)) (NullV))

(check-equal? (test-interp (IdC 'true)) #t)

(check-equal? (test-interp (NumC 89)) 89)

(check-equal? (test-interp (AppC (IdC '+) (list (NumC 8)
                                                (AppC (IdC '*) (list (NumC 2) (NumC 3))))))  14)

(check-equal? (test-interp (AppC (IdC '<=) (list (NumC 9) (NumC 10)))) #t)

(check-equal? (test-interp (AppC (IdC 'equal?) (list (NumC 9) (NumC 10)))) #f)

(check-equal? (test-interp (AppC (IdC 'equal?) (list (NumC 9) (NumC 10)))) #f)

(check-equal? (test-interp (IfC (AppC (IdC '<=) (list (NumC 5) (NumC 2))) (NumC 1) (NumC -1))) -1)

(check-equal? (test-interp (AppC (LamC '(x) (AppC (IdC '+) (list (IdC 'x) (NumC 1))))
                                 (list (NumC 5)))) 6)

(check-equal? (test-interp (IfC (AppC (IdC 'equal?) (list (NumC 81) (NumC 81)))
                                (IdC 'true) (IdC 'false))) #t)


(check-equal? (test-interp (AppC
                            (LamC '(x)
                                  (AppC
                                   (IdC 'seq)
                                   (list
                                    (MutateC 'x (StringC "changed")) (IdC 'x))))
                            (list (NumC 33))))
              "changed")


;; interp with seq
(check-exn #rx"SHEQ: seq needs at least 1 expression."
           (lambda () (test-interp (AppC (IdC 'seq) '()))))


;; ---- interp error check ---- 
(check-exn #rx"SHEQ: An unbound identifier" (lambda () (test-interp (IdC 'x))))

(check-exn #rx"SHEQ: PrimV \\+ expected 2 numbers"
           (lambda () (test-interp (AppC (IdC '+) (list (IdC '-) (NumC 4))))))

(check-exn #rx"SHEQ: Divide by zero error"
           (lambda () (test-interp (AppC (IdC '/) (list (NumC 5) (NumC 0))))))

(check-exn #rx"SHEQ: If expected boolean test"
           (lambda () (test-interp (parse '{if 32 23 32}))))

(check-exn #rx"SHEQ: \\+ received incorrect number of arguments, expected 2, got"
           (lambda ()
             (test-interp (AppC (LamC '(x)
                                      (AppC (IdC '+) (list (IdC 'x) (NumC 1) (NumC 2))))
                                (list (NumC 5))))))

(check-exn #rx"SHEQ: Attempted to apply non function value"
           (lambda ()
             (test-interp (AppC (NumC 9) (list (NumC 12))))))

(check-exn #rx"SHEQ: An unbound identifier"
           (lambda () (test-interp (MutateC 'x (StringC "notlet")))))



;; ---- serialize tests ----
(check-equal? (serialize '32) "32")
(check-equal? (serialize #f) "false")
(check-equal? (serialize #t) "true")
(check-equal? (serialize (CloV '(x) (NumC 34) (list (Binding 'fake 100)))) "#<procedure>")
(check-equal? (serialize (PrimV '<=)) "#<primop>")
(check-equal? (serialize (ArrayV 2 12)) "#<array>")
(check-equal? (serialize (NullV)) "null")

; (check-exn #rx"SHEQ: user-error true" (lambda () (interp-prim (PrimV 'error) (list #t) (make-test-store))))


;; ---- parse Tests ----

(check-equal? (parse '{(lambda (x) : {+ x 1}) 5})
              (AppC (LamC '(x) (AppC (IdC '+) (list (IdC 'x) (NumC 1)))) (list (NumC 5))))

(check-equal? (parse '{+ 5 12}) (AppC (IdC '+) (list (NumC 5) (NumC 12))))

(check-equal? (parse '{applyThis 5 12}) (AppC (IdC 'applyThis) (list (NumC 5) (NumC 12))))

(check-equal? (parse 'double) (IdC 'double))

(check-equal? (parse '{double x 2}) (AppC (IdC 'double) (list (IdC 'x) (NumC 2))))

(check-equal? (parse '{ifleq0? 5 x y}) (AppC (IdC 'ifleq0?) (list (NumC 5) (IdC 'x) (IdC 'y))))

(check-equal? (parse '{let {[x = 5] [y = {* 7 8}]} in {+ x y} end}) 
              (AppC 
               (LamC (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) 
               (list (NumC 5) 
                     (AppC (IdC '*) (list (NumC 7) (NumC 8))))))

(check-equal? (parse '{if {<= 3 90} 3 90})
              (IfC (AppC (IdC '<=) (list (NumC 3) (NumC 90))) (NumC 3) (NumC 90)))

(check-equal? (parse '{x := 2}) (MutateC 'x (NumC 2)))

(check-equal? (parse 'null) (NullC))


;; parse errors
(check-exn #rx"SHEQ: Lambda args list is invalid, duplicate parameters found"
           (lambda () (parse '(lambda (x y x) : 33))))
(check-exn #rx"SHEQ: Let binding list is invalid, duplicate variables"
           (lambda () (parse '(let {[bo = {lambda () : 33}] [bo = "Twenty"]} in {bo} end))))
(check-exn #rx"SHEQ: let binding list is invalid, reserved symbol was used"
           (lambda () (parse '(let {[if = {lambda () : 0}]} in {if 3} end))))

(check-exn #rx"SHEQ: Syntax error, got"
           (lambda () (parse '{})))

(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '{let 2})))

(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '{end 3 4 3 2})))

(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '=)))

(check-exn #rx"SHEQ: Syntax error, cannot mutate reserved keyword"
           (lambda () (parse '{let := "notlet"})))




;; ---- interp-prim Tests ----
;; PrimV '+ tests
(check-equal? (interp-prim (PrimV '+) (list 8 9) (make-test-store)) 17)
(check-exn #rx"SHEQ: PrimV \\+ expected 2 numbers, got"
           (lambda () (interp-prim (PrimV '+) (list 8 #t) (make-test-store))))
(check-exn #rx"SHEQ: \\+ received incorrect number of arguments, expected 2, got"
           (lambda () (interp-prim (PrimV '+) (list 8 23 3 2) (make-test-store))))

;; PrimV '* tests
(check-equal? (interp-prim (PrimV '*) (list 8 4) (make-test-store)) 32)
(check-exn #rx"SHEQ: PrimV \\* expected 2 numbers, got"
           (lambda () (interp-prim (PrimV '*) (list #f #t) (make-test-store))))
(check-exn #rx"SHEQ: \\* received incorrect number of arguments, expected 2, got"
           (lambda () (interp-prim (PrimV '*) (list 2) (make-test-store))))

;; PrimV '/ tests
(check-equal? (interp-prim (PrimV '/) (list 33 11) (make-test-store)) 3)
(check-exn #rx"SHEQ: PrimV \\/ expected 2 numbers, got"
           (lambda () (interp-prim (PrimV '/) (list #f #t) (make-test-store))))
(check-exn #rx"SHEQ: Divide by zero error"
           (lambda () (interp-prim (PrimV '/) (list 3 0) (make-test-store)))) 
(check-exn #rx"SHEQ: \\/ received incorrect number of arguments, expected 2, got"
           (lambda () (interp-prim (PrimV '/) (list 21 2 3) (make-test-store))))

;; PrimV '- tests
(check-equal? (interp-prim (PrimV '-) (list 33 11) (make-test-store)) 22)
(check-exn #rx"SHEQ: PrimV \\- expected 2 numbers, got"
           (lambda () (interp-prim (PrimV '-) (list #f #t) (make-test-store))))
(check-exn #rx"SHEQ: \\- received incorrect number of arguments, expected 2, got"
           (lambda () (interp-prim (PrimV '-) (list 9 3 2 1 3) (make-test-store))))

;; PrimV '<= tests
(check-equal? (interp-prim (PrimV '<=) (list 3 11) (make-test-store)) #t)
(check-equal? (interp-prim (PrimV '<=) (list 3 -11) (make-test-store)) #f)
(check-exn #rx"SHEQ: PrimV \\<= expected 2 numbers, got"
           (lambda () (interp-prim (PrimV '<=) (list #f #t) (make-test-store))))
(check-exn #rx"SHEQ: \\<= received incorrect number of arguments, expected 2, got"
           (lambda () (interp-prim (PrimV '<=) (list 3) (make-test-store))))

;; PrimV 'equal? tests
(check-equal? (interp-prim (PrimV 'equal?) (list 9 9) (make-test-store)) #t)
(check-equal? (interp-prim (PrimV 'equal?) (list (NullV) (NullV)) (make-test-store)) #t)
(check-equal? (interp-prim (PrimV 'equal?) (list #f #f) (make-test-store)) #t)
(check-equal? (interp-prim (PrimV 'equal?) (list "hi" "hi") (make-test-store)) #t)
(check-equal? (interp-prim (PrimV 'equal?) (list 3 #f) (make-test-store)) #f)
(check-equal? (interp-prim (PrimV 'equal?)
                           (list (CloV '(x) (NumC 1) '()) (CloV '(x) (NumC 1) '()))
                           (make-test-store)) #f)
(check-equal? (interp-prim (PrimV 'equal?)
                           (list (PrimV '-) (PrimV '-))
                           (make-test-store)) #f)

;; - equal? array test
(define temp-store (make-initial-store 20))
(define left (interp-prim (PrimV 'make-array) (list 3 0) temp-store))
(define right (interp-prim (PrimV 'make-array) (list 5 11) temp-store))
(check-equal? (interp-prim (PrimV 'equal?) (list left right) temp-store) #f)
(check-equal? (interp-prim (PrimV 'equal?) (list left left) temp-store) #t)

;; - equal? error
(check-exn #rx"SHEQ: equal\\? received incorrect number of arguments, expected 2, got"
           (lambda () (interp-prim (PrimV 'equal?) (list 3) (make-test-store))))

;; PrimV 'substring tests
(check-equal? (interp-prim (PrimV 'substring) (list "hello world!" 0 5) (make-test-store)) "hello")

(check-equal? (interp-prim (PrimV 'substring) (list "abcd" 1 4) (make-test-store)) "bcd")


(check-exn #rx"SHEQ: substring needs string and 2 valid natural indices"
           (lambda () (interp-prim (PrimV 'substring) (list "hello" 99 1) (make-test-store))))
(check-exn #rx"SHEQ: substring received incorrect number of arguments, expected 3"
           (lambda () (interp-prim (PrimV 'substring) (list "bib" 0 1 23 3) (make-test-store))))

;; PrimV 'strlen tests
(check-equal? (interp-prim (PrimV 'strlen) (list "hello world!") (make-test-store)) 12)
(check-exn #rx"SHEQ: Syntax error" (lambda () (interp-prim (PrimV 'strlen) (list 3) (make-test-store))))
(check-exn #rx"SHEQ: strlen received incorrect number of arguments, expected 1"
           (lambda () (interp-prim (PrimV 'strlen) (list "bib" "five" 3) (make-test-store))))

;; PrimV 'error test
#; (check-exn #rx"SHEQ: error received incorrect number of arguments, expected 1"
           (lambda () (interp-prim (PrimV 'error) (list "This" "too many") (make-test-store))))

;; PrimV invalid PrimV test
(check-exn #rx"SHEQ: Invalid PrimV op"
           (lambda () (interp-prim (PrimV 'dothis) (list 9) (make-test-store))))

;; PrimV 'println tests
(check-equal? (interp-prim (PrimV 'println) (list "test: Hello World from interp") (make-test-store)) #t)

(check-exn #rx"SHEQ: Attempted to print a non-string value"
           (lambda ()
             (interp-prim (PrimV 'println) (list 5) (make-test-store))))
(check-exn #rx"SHEQ: println received incorrect number of arguments"
           (lambda ()
             (interp-prim (PrimV 'println) (list "a" "c") (make-test-store))))


;; PrimV 'read-num test
(check-equal? (with-input-from-string "52\n"
                (lambda () (interp-prim (PrimV 'read-num) '() (make-test-store)))) 52)

(check-exn #rx"SHEQ: read-num expected a Number"
           (lambda () 
             (with-input-from-string "five"
               (lambda () (interp-prim (PrimV 'read-num) '() (make-test-store))))))

(check-exn #rx"SHEQ: read-num read EOF"
           (lambda () 
             (with-input-from-string ""
               (lambda () (interp-prim (PrimV 'read-num) '() (make-test-store))))))

(check-exn #rx"SHEQ: read-num received incorrect number of arguments"
           (lambda () (interp-prim (PrimV 'read-num) (list 4 2 1) (make-test-store))))

;; PrimV 'read-str tests
(check-equal? (with-input-from-string "hello\n"
                (lambda () (interp-prim (PrimV 'read-str) '() (make-test-store)))) "hello")

(check-exn #rx"SHEQ: read-str received incorrect number of arguments"
           (lambda () (interp-prim (PrimV 'read-str) (list "s" "b" "c") (make-test-store))))

(check-exn #rx"SHEQ: read-str read EOF"
           (lambda () 
             (with-input-from-string ""
               (lambda () (interp-prim (PrimV 'read-str) '() (make-test-store))))))

;; PrimV '++ tests
(check-equal? (interp-prim (PrimV '++) (list 4 "hello" #f) (make-test-store)) "4hellofalse")
(check-equal? (interp-prim (PrimV '++) '() (make-test-store)) "")
(check-equal? (interp-prim (PrimV '++) (list (ArrayV 3 2)) (make-test-store)) "#<array>")

;; PrimV 'make-array tests

;; (stre1)[1] = ArrayV of size 3 of all 10's
(define stre1 (make-test-store))
(check-equal? (interp-prim (PrimV 'make-array) (list 3 10) stre1) (ArrayV 2 3))

(check-exn #rx"SHEQ: Out of memory"
           (lambda () (interp-prim (PrimV 'make-array) (list 1000000 1) (make-test-store))))

(check-exn #rx"SHEQ: make-array expected size 1 or greater"
           (lambda () (interp-prim (PrimV 'make-array) (list 0 1) (make-test-store))))

(check-exn #rx"SHEQ: make-array expected a natural number for size"
           (lambda () (interp-prim (PrimV 'make-array) (list 2.4 1) (make-test-store))))

(check-exn #rx"SHEQ: make-array received incorrect number of arguments, expected 2, got"
           (lambda () (interp-prim (PrimV 'make-array) (list 2.3) (make-test-store))))

;; PrimV 'array tests

;; stre1[5] = ArrayV of size 3 of {"a", "b", 3}
(check-equal? (interp-prim (PrimV 'array) (list "a" "b" 3) stre1) (ArrayV 6 3))

(check-exn #rx"SHEQ: array expected at least one element"
           (lambda () (interp-prim (PrimV 'array) '() (make-test-store))))

;; PrimV 'aref tests
(define aref-store (make-initial-store 20))
(interp-prim (PrimV 'array) (list "a" "b" "c") aref-store)
(interp-prim (PrimV 'array) (list 10 20 30) aref-store)
(check-equal? (interp-prim (PrimV 'aref) (list (ArrayV 2 3) 0) aref-store) "a")

(check-equal? (interp-prim (PrimV 'aref) (list (ArrayV 5 3) 2) aref-store) 20)

(check-exn #rx"SHEQ: aref expected an array"
           (lambda () (interp-prim (PrimV 'aref) (list 23 23) aref-store)))

(check-exn #rx"SHEQ: aref expected an integer for index"
           (lambda () (interp-prim (PrimV 'aref) (list (ArrayV 4 3) 1.2) aref-store)))

(check-exn #rx"SHEQ: aref index out of bounds"
           (lambda () (interp-prim (PrimV 'aref) (list (ArrayV 4 3) 290192) aref-store)))

(check-exn #rx"SHEQ: aref received incorrect number of arguments, expected 2, got"
           (lambda () (interp-prim (PrimV 'aref) (list 3 2 3 23 32 32) aref-store)))

;; PrimV 'aset! tests
(define aset!-store (make-initial-store 20))
(interp-prim (PrimV 'array) (list "a" "b" "c") aset!-store)
(interp-prim (PrimV 'array) (list 10 20 30) aset!-store)

;; (make-test-store) : [9, (ArrayV 2 3), "a", "b", "c", (ArrayV 6 3), 10, 20, 30, ...]
(check-equal? (interp-prim (PrimV 'aset!) (list (ArrayV 2 3) 0 "changed") aset!-store) (NullV))
;; after      : [9, (ArrayV 2 3), "changed", "b", "c", (ArrayV 6 3), 10, 20, 30,...]
(check-equal? (interp-prim (PrimV 'aref) (list (ArrayV 2 3) 0) aset!-store) "changed")

(check-exn #rx"SHEQ: aset! expected an array"
           (lambda () (interp-prim (PrimV 'aset!) (list "notarray" 23 1) aset!-store)))

(check-exn #rx"SHEQ: aset! expected an integer for index"
           (lambda () (interp-prim (PrimV 'aset!) (list (ArrayV 6 3) "parry" 1) aset!-store)))

(check-exn #rx"SHEQ: aset! index out of bounds"
           (lambda () (interp-prim (PrimV 'aset!) (list (ArrayV 6 3) -12 "bear") aset!-store)))

(check-exn #rx"SHEQ: aset! received incorrect number of arguments, expected 3, got"
           (lambda () (interp-prim (PrimV 'aset!) (list 0 0) aset!-store)))

;; ---- Helper Tests ----

;; value->string tests
(check-equal? (val->string 3) "3")
(check-equal? (val->string #t) "true")
(check-equal? (val->string #f) "false")
(check-equal? (val->string "s") "s")
(check-equal? (val->string (CloV '(x) (NumC 4) (list (Binding 'fake 100)))) "#<procedure>")
(check-equal? (val->string (PrimV '+)) "#<primop>")
(check-equal? (val->string (ArrayV 5 4)) "#<array>")

;; distinct-args? tests
(check-equal? (distinct-args? '(x y z)) #t)
(check-equal? (distinct-args? '(x y x)) #f)

;; reserved-symbol tests
(check-equal? (reserved-symbol? 'lambda) #t)
(check-equal? (reserved-symbol? '+++) #f)

;; make-default-env test
(check-equal? (make-default-env (make-initial-store 20))
              (list
               (Binding 'true 1)
               (Binding 'false 2)
               (Binding '+ 3)
               (Binding '- 4)
               (Binding '* 5)
               (Binding '/ 6)
               (Binding '<= 7)
               (Binding 'equal? 8)
               (Binding 'substring 9)
               (Binding 'strlen 10)
               #;(Binding 'error 11)
               (Binding 'println 11)
               (Binding 'read-num 12)
               (Binding 'read-str 13)
               (Binding '++ 14)
               (Binding 'make-array 15)
               (Binding 'array 16)
               (Binding 'aref 17)
               (Binding 'aset! 18)))


;; get-binding tests
(check-equal? (get-binding 'sym (list (Binding 'sym 5))) 5)
(check-exn #rx"SHEQ: An unbound identifier" (lambda () (get-binding 'sym '())))


;; make-initial-store tests
(check-equal? (vector-length (make-initial-store 10)) (+ 10 (length top-env-defs)))
(check-equal? (vector-ref (make-initial-store 102) (ann 0 Natural)) 1)

;; store-get tests
(define mde-store (make-initial-store 100))
(define mde-env (make-default-env mde-store))

(check-equal? (store-get 'true mde-env mde-store) #t)
(store-set! 'true 1 mde-env mde-store) (NullV)
(check-equal? (store-get 'true mde-env mde-store) 1) 

;; store-set! test
(check-equal? (store-set! 'true 2 mde-env mde-store) (NullV))
(check-equal? (store-get 'true mde-env mde-store) 2) 


;; allocate tests
(define allo-store-test : (Mutable-Vectorof Value) (make-vector 2 0))
(check-equal? (allocate allo-store-test "one cell") 0)
(check-equal? (allocate allo-store-test 12) 1)

(check-exn #rx"SHEQ: Out of memory. Tried to allocate"
           (lambda () (allocate allo-store-test "another cell"))) 

;; allocate-lst tests
(define st (make-initial-store 20))

(check-equal? (allocate-lst st (list 1 2 3)) 4)
(check-equal? (allocate-lst st (list 1 2 3 4 5 6 7)) 11)

(define allo-err-store (make-initial-store 2))
; Populate the store with default env so that it doesn't have blank extra space
(make-default-env allo-err-store)
(check-exn #rx"SHEQ: Out of memory. Tried to allocate"
           (lambda () (allocate-lst allo-err-store (list 1 2 3))))


;; next-address test
(check-equal? (next-address st) 11)
(check-equal? (next-address allo-store-test) 2)