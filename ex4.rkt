#lang pl 03
 
#|
Elad Keyshawn - 315631887
Shlomi Alon - 311610547
|#

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;==========BNF for ROL==========
#| 
 <ROL> ::= {reg-len = <num> <RegE>}
 <RegE> ::=
            <Bits>                      |
            {and <RegE> <RegE>}         |
            {or <RegE> <RegE>}
            {xor <RegE> <RegE>}         |
            {shl <RegE>}                |
            {with {<ID> <RegE>} <RegE>} |
            {<ID>}                      |
            {call <RegE> <RegE>}        | 
            {fun <ID> <RegE>}           |
            {if <Bool> <RegE> <RegE>}
 <Bool> ::=
            false                       |
            true                        |
            {geq? <RegE> <RegE>}        |
            {maj? <RegE>}
 <Bits> ::=
            1                           |
            0                           |
            <Bits>...
|#


;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))


;; RegE abstract syntax trees
(define-type RegE
 [Reg Bit-List]
 [And RegE RegE]
 [Or RegE RegE]
 [Xor RegE RegE]
 [Shl RegE]
 [Id Symbol]
 [With Symbol RegE RegE]
 [Bool Boolean]
 [Geq RegE RegE]
 [Maj RegE]
 [If RegE RegE RegE]
 [Fun Symbol RegE]
 [Call RegE RegE]
  ) 



;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List);; to cast a list of bits as a bit-list
(define (list->bit-list lst)
(cond [(null? lst) null]
      [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
      [else (cons 0 (list->bit-list (rest lst)))]))


;; parses an s-expressions into RegEs
;;converts it to RegE tree syntax
(: parse-sexpr-RegL : Sexpr Number -> RegE)
(define (parse-sexpr-RegL sexpr reg-len)
  (match sexpr
    [(list (and a (or 1 0)) ... ) 
     (if (= reg-len (length a))
           (Reg (list->bit-list a))
           (error 'parse-sexpr "wrong number of bits in ~s" a))]
    [(list 'and list1 list2) (And (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'or list1 list2) (Or (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'xor list1 list2) (Xor (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'shl list1) (Shl (parse-sexpr-RegL list1 reg-len))]
    [(list 'if list1 list2 list3)
     (If (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len) (parse-sexpr-RegL list3 reg-len))]
    [(symbol: id-name)
           (if (eq? id-name 'false)(Bool #f)
            (if (eq? id-name 'true)(Bool #t)(Id id-name)))]
    [(cons 'with args)
     (match sexpr
    [(list 'with (list (symbol: oldName) newName) body)
     (With oldName (parse-sexpr-RegL newName reg-len) (parse-sexpr-RegL body reg-len))]
    [else (error 'parse-sexpr-RegE "bad with syntax in ~s" sexpr)])]
    [(list 'geq? list1 list2) (Geq (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'maj? list1) (Maj(parse-sexpr-RegL list1 reg-len))]
    [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name)) body)
          (Fun name (parse-sexpr-RegL body reg-len))]
         [else (error 'parse-sexpr-RegL "bad `fun' syntax in ~s" sexpr)])]
    [(list 'call fun arg) (Call (parse-sexpr-RegL fun reg-len) (parse-sexpr-RegL arg reg-len))] ; Call
    [else (error 'parse-sexpr-RegL "bad syntax in ~s" sexpr)]

    )) 


;; to convert the main s-expression into ROL
(: parse-sexpr : Sexpr -> RegE)
(define (parse-sexpr sexpr)
   (match sexpr
      [(list 'reg-len' = (number: n) args) 
       (if(> n 0)(parse-sexpr-RegL args n)(error 'parse-sexpr "Register length must be at least 1 ~s" sexpr))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

;; parses a string containing a RegE expression to a RegE AST
(: parse : String -> RegE)
(define (parse str)
 (parse-sexpr (string->sexpr str)))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;==========define-type: RES Outputs: Boolean | Register==========
;;include two variants Register and Boolean 
;;RegV - as a variance
(define-type RES 
  [RegV Bit-List]
  [boolVal Boolean]
  [FunV Symbol RegE ENV]
  )


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;==========Func: subst | Input:RegE Symbol RegE | Output: RegE==========
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
;; for this answer i Used in the practical session.
(: subst : RegE Symbol RegE -> RegE)
(define (subst expr from to)
  (cases expr
    [(Reg n) expr]
    [(And l r) (And (subst l from to) (subst r from to))]
    [(Or l r) (Or (subst l from to) (subst r from to))]
    [(Xor l r) (Xor (subst l from to) (subst r from to))]
    [(Geq l r) (Geq (subst l from to) (subst r from to))]
    [(Shl l) (Shl (subst l from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(Maj l) (Maj (subst l from to))]
    [(Bool b) expr]
    [(If l e r) (If (subst l from to) (subst e from to) (subst r from to))]
    [(With bound-id named-expr bound-body)
     (With bound-id (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
    [(Call l r)
     (Call (subst l from to) (subst r from to))]
    [(Fun bound-id bound-body)
     (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]
    ))

;==========Test: subst==========
(test (subst (Bool #t) 'x (Bool #f)))
(test (subst (Bool #f) 'x (Bool #t)))
(test (subst (Geq (Reg '(1 0)) (Reg '(1 1))) 'x (Id 'x)))
(test (subst (Geq (Reg '(0 1)) (Reg '(1 1))) 'x (Id 'x)))
(test (subst (Maj (Reg '(1 1))) 'x (Id 'x)))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; Defining functions for dealing with arithmetic operations
(: bit-and : BIT BIT -> BIT)
;; Arithmetic and on two bits
 (define (bit-and a b)
   (cond
     [(or (eq? a 0)(eq? b 0))0]
     [else 1]))
;==========Test: bit-and==========
(test (bit-and 0 1) => 0)
(test (bit-and 1 1) => 1)

(: bit-xor : BIT BIT -> BIT)
;; Arithmetic and on two bits
 (define (bit-xor a b)
   (cond
     [(eq? a b) 0]
     [else 1]
     ))
;==========Test: bit-and==========
(test (bit-xor 0 1) => 1)
(test (bit-xor 1 1) => 0)

 (: bit-or : BIT BIT -> BIT)
;; Arithmetic and on two bits
 (define(bit-or a b)
    (cond
      [(or (eq? a 1)(eq? b 1))1]
      [else 0]))
;==========Test: bit-or==========
(test (bit-or 0 0) => 0)
(test (bit-or 1 0) => 1)


(: reg-arith-op : (BIT BIT -> BIT) RES RES -> RES)
;; Consumes two registers and some binary bit operation 'op',
;; and returns the register obtained by applying op on the
;; i'th bit of both registers for all i.

(define(reg-arith-op op reg1 reg2)
(: bit-arith-op : Bit-List Bit-List -> Bit-List)
;; Consumes two bit-lists and uses the binary bit operation 'op'.
;; It returns the bit-list obtained by applying op on the
;; i'th bit of both registers for all i.
;; send the first bit of bl1 and first bit of bl2 to fuction op and append to rest bl1 and bll2
 (define(bit-arith-op bl1 bl2)
     (cond
       [(null? bl1)'()]
       [else(append (cons (op (first bl1) (first bl2)) (bit-arith-op (rest bl1) (rest bl2))))]))
 (RegV (bit-arith-op (RegV->bit-list reg1) (RegV->bit-list reg2))))


(: majority? : Bit-List -> Boolean)
 ;; Consumes a list of bits and checks whether the
 ;; number of 1's are at least as the number of 0's.
(define(majority? bl)
  (majority-counting bl 0 (/ (length bl) 2)))
;; consumes bit-list, number of '1' (0) and (length of bit-list)/2
;; returns the number of 1's in a Bit-List
(: majority-counting : Bit-List Number Number -> Boolean)
  (define (majority-counting bl count1 len)
    (cond
      [(null? bl) (<= len count1)]
      [(<= len count1) true]
      [(eq? (first bl) 1)  (majority-counting (rest bl) (+ 1 count1) len)]
      [else  (majority-counting (rest bl) count1 len)]))
;==========Test: majority?==========
(test (majority? `(1 1 0 0)) => #t)
(test (majority? `(0 0 1 0)) => #f)
(test (majority? `(0 0 1 1)) => #t)
(test (majority? `(0 1 0 0)) => #f)


(: geq-bitlists? : Bit-List Bit-List -> Boolean)
 ;; Consumes two bit-lists and compares them. It returns true if the
 ;; first bit-list is larger or equal to the second.
(define (geq-bitlists? bl1 bl2)
  (cond 
  [(or (null? bl1) (null? bl2)) true]
  [(> (first bl1) (first bl2)) true]
  [(< (first bl1) (first bl2)) false]
  [else (geq-bitlists? (rest bl1) (rest bl2))]))
;==========Test: geq-bitlists?==========
(test (geq-bitlists? '(1 0 0 0 0) '(1 0 0 0 0))=> #t)
(test (geq-bitlists? '(1 1 1) '(1 1 1)) => #t)
(test (geq-bitlists? '(1 0 0 0 1) '(1 0 0 0 1)) => #t)
(test (geq-bitlists? '(1 1 0 1 1) '(1 1 0 1 1)) => #t)
(test (geq-bitlists? '(0 0 0 0 1) '(1 0 0 0 1)) => #f)


(: shift-left : Bit-List -> Bit-List)
 ;; Shifts left a list of bits (once)
 (define(shift-left bl)
   (append (rest bl) (list (first bl))))
;==========Test: shift-left==========
(test (shift-left '(1 0 0 1)) => '(0 0 1 1))
(test (shift-left '(0 1 0 1)) => '(1 0 1 0))
(test (shift-left '(0 0 0 0)) => '(0 0 0 0))
(test (shift-left '(1 1 1 1)) => '(1 1 1 1))


(: RegV->bit-list : RES -> Bit-List)
 ;; extract a bit-list from RES type
 (define (RegV->bit-list reg)
   (cases reg
     [(RegV bt) bt]
     [else (error 'RegV->bit-list "error in RegV->bit-list")]))


 ;; Types for environments, values, and a lookup function

  (define-type ENV
    [EmptyEnv]
    [Extend Symbol RES ENV])

  (: lookup : Symbol ENV -> RES)
  (define (lookup name env)
    (cases env
      [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
      [(Extend id val rest-env)
       (if (eq? id name) val (lookup name rest-env))]))

(: lookup2 : Symbol ENV -> Boolean)
  (define (lookup2 name env)
    (cases env
      [(EmptyEnv) #t]
      [(Extend id val rest-env)
       (if (eq? id name) #f (lookup2 name rest-env))]))


;; Runs through the AST tree recursively if at anypoint the lookup table won't find variable in env, it'll return true,
;; and since all recursive calls are connected with "or" the true value will bubble up the stack so the func will return true.
(: containsFreeInstance? : RegE ENV -> Boolean)
(define (containsFreeInstance? expr env)
  (cases expr
    [(Reg b)  #f]
    [(And l r) ( or (containsFreeInstance? l env) (containsFreeInstance? r env))]
    [(Or l r) (or (containsFreeInstance? l env) (containsFreeInstance? r env))]
    [(Xor l r) (or (containsFreeInstance? l env) (containsFreeInstance? r env))]
    [(Shl l) (containsFreeInstance? l env)]
    [(Geq reg1 reg2) (or (containsFreeInstance? reg1 env) (containsFreeInstance? reg2 env))]
    [(Maj l) (containsFreeInstance? l env)]
    [(If condition if_T if_F)(or (containsFreeInstance? condition env) (containsFreeInstance? if_T env) (containsFreeInstance? if_F env))]
    [(Id sym) (if (or (eq? sym 'false) (eq? sym 'true)) #f (lookup2 sym env))]
    [(Bool l) #f]
    [(With bound-id named-expr bound-body)
       (containsFreeInstance? bound-body
             (Extend bound-id (boolVal (containsFreeInstance? named-expr env)) env))]
    [(Fun bound-id bound-body)
       (containsFreeInstance? bound-body 
             (Extend bound-id (boolVal(containsFreeInstance? bound-body env)) env))]
      [(Call fun-expr arg-expr) (if (or (containsFreeInstance? fun-expr env) (containsFreeInstance? fun-expr env)) #t #f)]
     )
  )

(: check-code : String -> Boolean) (define (check-code str)
(containsFreeInstance? (parse str) (EmptyEnv)))

(test (check-code "{ reg-len = 3 {call {fun {x} {or x {1 0 1}}} {0 0 1}}}") => #f)
(test (check-code "{ reg-len = 3 {call {fun {y} {xor x {0 0 1}}} {0 0 1}}}") => #t)
(test (check-code "{ reg-len = 3 {call foo {0 0 1}}}") => #t)
(test (check-code "{ reg-len = 3 {fun {x} {and x {or {1 0 1} {shl {0 0 0}}}}}}") => #f)
(test (check-code "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}") => #t)

(: eval : RegE ENV -> RES)
;; evaluates RegE expressions by reducing them to bit-lists
;; return bit-list or boolean. 
(define (eval expr env)
   (cases expr
    [(Reg b)  (RegV b)]
    [(And l r) (reg-arith-op bit-and (eval l env) (eval r env))]
    [(Or l r) (reg-arith-op bit-or (eval l env) (eval r env))]
    [(Xor l r) (reg-arith-op bit-xor (eval l env) (eval r env))]
    [(Shl l) (RegV (shift-left (RegV->bit-list (eval l env))))]
    [(Geq reg1 reg2) (boolVal (geq-bitlists? (RegV->bit-list (eval reg1 env)) (RegV->bit-list (eval reg2 env))))]
    [(Maj l) (boolVal(majority? (RegV->bit-list(eval l env))))]
    [(If condition if_T if_F)(cases (eval condition env)[(boolVal boo) (if (eq? boo #t) (eval if_T env) (eval if_F env))][else (error 'eval "error in ~s" condition)])]
    [(Id sym) (if (eq? sym 'false)(boolVal #f)(if (eq? sym 'true)(boolVal #t)(lookup sym env)))]

     [(Bool l) (boolVal l)]
    [(With bound-id named-expr bound-body)
       (eval bound-body
             (Extend bound-id (eval named-expr env) env))]
    [(Fun bound-id bound-body)
       (FunV bound-id bound-body env)]
    [(Call fun-expr arg-expr)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id bound-body f-env)
            (eval bound-body
                  (Extend bound-id (eval arg-expr env) f-env))]
           [else (error 'eval "`call' expects a function, got: ~s"
                              fval)]))]
     ))
 
 
;==========Test: eval(operator)==========
(test (eval (Reg '(1 0 1 1)) (EmptyEnv)) => (RegV '(1 0 1 1)))
(test (eval (And (Reg '(1 1 0 0)) (Reg '(1 0 1 0))) (EmptyEnv)) => (RegV '(1 0 0 0)) )
(test (eval (Or (Reg '(1 1 0 0)) (Reg '(1 0 1 0))) (EmptyEnv)) => (RegV '(1 1 1 0)) )
(test (eval (Xor (Reg '(1 1 0 0)) (Reg '(1 0 1 0))) (EmptyEnv)) => (RegV '(0 1 1 0)) )
(test (eval (Shl (Reg '(0 0 0 1))) (EmptyEnv)) => (RegV '(0 0 1 0)))
;(test (eval (Id 'a) (EmptyEnv)) =error> "free identifier: a")
(test (eval (Id 'false) (EmptyEnv)))
(test (eval (Id 'true) (EmptyEnv)))
(test (eval (With 'x (Or (Reg '(1 0 1 0)) (Reg '(0 1 0 0))) (Shl (Shl (Shl (Id 'x))))) (EmptyEnv)) => (RegV '(0 1 1 1)))
(test (eval (Bool #f) (EmptyEnv)) => (boolVal #f))
(test (eval (Geq (Reg '(0 0 1 1)) (Reg '(0 1 1 1))) (EmptyEnv)) => (boolVal #f))
(test (eval (Maj (Reg '(1 0 0 0))) (EmptyEnv)) => (boolVal #f))
(test (eval (If (Bool #t)
                (And (Reg '(1 1 0 0)) (Reg '(1 0 1 0)))
                (Or (Reg '(1 1 0 0)) (Reg '(1 0 1 0)))) (EmptyEnv)) => (RegV '(1 0 0 0)))

;; New Fun and Call tests
;(test (eval (Call (Fun 'x (Maj x)) (Reg '(1 0 0 0))) (EmptyEnv)) => (boolVal #f) )
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(: run : String -> Bit-List)
;; evaluate a ROL program contained in a string
;; we will not allow to return a boolean type
(define (run str)
  (cases (eval (parse str) (EmptyEnv))
    [(RegV bl) bl]
    [else (error 'run "error- can't return boolean!")]))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



;;More general tests
(test (run "{ reg-len =  3  {call {fun {x} {and x x}} {1 1 1} }}") => '(1 1 1))
(test (run "{ reg-len =  4  {1 0 0 0}}") => '(1 0 0 0))  
(test (run "{ reg-len = 4  {shl {1 0 0 0}}}") => '(0 0 0 1)) 
(test (run "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1)) 
(test (run "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => '(1 0 1 1)) 
(test (run "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => '(1 0))
(test (run "{ reg-len = 4 {with {x {0 1 1 0 }} {with {x {1 1 1 1}} {shl x}}}}") => '(1 1 1 1))
(test (run "{ reg-len = 3 {with {x {0 0 0}} {if {geq? {1 0 1} x} {0 0 1} {1 1 0}}}}") => '(0 0 1))
(test (run "{ reg-len = 3  {if {geq? {1 0 1} {1 1 1}} true false}}") =error> "error- can't return boolean!" )
(test (RegV->bit-list (RegV '(1 0 0 0))) => '(1 0 0 0))
;(test (run "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}") =error> "free identifier: y") 
(test (run "{ reg-len = 2 { with {x { or {and {shl {1 0}} {1 0}} {1 0}}} {shl x}}}") => '(0 1)) 
(test (run "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in (0 1 1)") 
(test (run "{ reg-len =  0  {}}") =error> "Register length must be at least 1")
(test (run "{ reg-len = 4  {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}") => '(0 1 1 1))
(test (run "{ reg-len = 4 {if true {shl {1 0 1 1}} {1 1 0 1}}}") => '(0 1 1 1))
(test (run "{ reg-len = 4 {if true {1 0 1 1} {1 1 0 1}}}") => '(1 0 1 1))
(test (run "{ reg-len = 4 {if false {shl {1 0 1 1}} {1 1 0 1}}}") => '(1 1 0 1))
(test (run "{ reg-len = 4 {if {geq? {1 0 1 0} {1 1 1 1}} {0 0 1 1} {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}}") => '(0 1 0 1))
(test (run "{ reg-len = 4 {if {geq? {1 0 1 0} {0 0 0 0}} {with {x {1 0 1 1}} {shl x}} {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}}") => '(0 1 1 1))
(test (run "{ reg-bla bla = 3 {1 1 1}}") =error> "bad syntax in (reg-bla bla = 3 (1 1 1))")
(test (geq-bitlists? '(1 1 1) '(1 1 1)) => true)
(test (geq-bitlists? '(1 1 0) '(1 1 1)) => false)
(test (geq-bitlists? '(1 1 1) '(1 1 0)) => true)
(test (run "{ reg-len = 4 {if {shl {1 0 0 0}} {shl {1 0 1 1}} {1 1 0 1}}}") =error> "error in (Shl (Reg (1 0 0 0)))")
(test (majority? '(1 1 1 1 0)) => true)
(test (run "{ reg-len = 4 {with {5 {1 1 1 1}} {shl y}}}") =error> "bad with syntax in (with (5 (1 1 1 1)) (shl y))")
(test (run "{ reg-len = 4 {shli {1 0 0 0}}}") =error> "bad syntax in (shli (1 0 0 0)")
(test (run "{ reg-len = 4 {with {x {0 1 1 0 }} {and x x}}}") => '(0 1 1 0))
(test (run "{ reg-len = 4 {with {x {0 1 1 0 }} {or x x}}}") => '(0 1 1 0))
(test (run "{ reg-len = 4 {with {x {0 1 1 0 }} {if {maj? x} {shl {1 0 1 1}} {1 1 0 1}}}}") => '(0 1 1 1))
(test (RegV->bit-list (boolVal #t)) =error> "error in RegV->bit-list")
(test (subst (Bool #t) 'x (Bool #t)) => (Bool #t))
(test (run "{ reg-len = 4 {with {y {0 1 1 0 }} {with {x {1 1 1 1}} {shl x}}}}") => '(1 1 1 1))
(test (run "{ reg-len = 4 {with {x {1 1 1 1}} {with {y {1 0 1 0}} {and x y}}}}") => '(1 0 1 0))
(test (run "{ reg-len = 3 {if {geq? {1 0 1} {1 1 1}} {0 0 1} {1 1 0}}}") => '(1 1 0))
(test (run "{ reg-len = 3 {if {geq? {1 0 1} {1 1 1}} {0 0 1} {1 1 0}}}") => '(1 1 0))

(test (run "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
 (test (run "{ reg-len = 4 {xor {1 0 0 0} {1 1 1 1}}}") =>
'(0 1 1 1))
 (test (run "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
 (test (run "{ reg-len = 4
 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") =>
'(0 1 0 1))
 (test (run "{ reg-len = 4
 { or {and {shl {1 0 1 0}}
 {shl {1 0 0 1}}} {1 0 1 0}}}") =>
'(1 0 1 1))
 (test (run "{ reg-len = 2
 { or {and {shl {1 0}} {1 0}} {1 0}}}") => '(1
0))
 (test (run "{ reg-len = 4
 {with {x {1 1 1 1}} {shl y}}}") =error>
"lookup: no binding for y")

#| Dont forget to add these error handlings
(test (run "{ reg-len = 4
 {or {1 1 1 1} {0 1 1}}}") =error>
 "wrong number of bits
in (0 1 1)")
 (test (run "{ reg-len = 0 {}}") =error>
 "Register length must be
at least 1")
|#
