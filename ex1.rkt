#lang pl
;==========Function:getMax==========
(: getMax : (Listof Number) -> Number );#name: getMax | #input: List | #output:max number of list
(define (getMax list)
     (if (null? list)0 ;if the list is empty
     (if (null? (cdr list)) (car list);If the second number does not exist return the first one - in the list we have 1 number
         (if (< (car list) (getMax (cdr list)))(getMax (cdr list))(car list);check recursively if the one number bigger from the second
      )
    )
  )
)
;==========Test:getMax==========
(test (getMax (list 1 2 5 3 )) => '5)
(test (getMax (list 5 3 )) => '5)
(test (getMax (list 3 )) => '3)
(test (getMax (list  )) => '0)

;==========Function:getMin==========
(: getMin : (Listof Number) -> Number );#name: getMin | #input: List | #output:min number of list
(define (getMin list)
     (if (null? list)0 ;if the list is empty
     (if (null? (cdr list)) (car list);If the second number does not exist return the first one - in the list we have 1 number
         (if (> (car list) (getMin (cdr list)))(getMin (cdr list))(car list);Recursive check if one number is smaller than the other
      )
    )
  )
)
;==========Test:getMin==========
(test (getMin (list 1 2 5 3 )) => '1)
(test (getMin (list 5 3 )) => '3)
(test (getMin (list 3 )) => '3)
(test (getMin (list  )) => '0)

;==========Function:min&max==========
(: min&max : Number Number Number Number Number -> (Listof Number));#name: min&max | #input: five numbers | #output: list of numbers
(define (min&max num1 num2 num3 num4 num5)
  (list (getMin (list num1 num2 num3 num4 num5)) (getMax (list num1 num2 num3 num4 num5)));return list with two numbers, the first one - min, the second - max (by helper func)
  )
;==========Test:min&max==========
(test (min&max 2 3 2 7 5) => '(2 7))
(test (min&max 2 2 2 2 2) => '(2 2))
(test (min&max -1 -2 -4 -7 -100) => '(-100 -1))
(test (min&max -1 2 -4 -7 0) => '(-7 2))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;==========Function:sublist-numbers==========
(: sublist-numbers : (Listof Any) -> (Listof Number));#name: sublist-numbers | #input: List of any types | #output:List of number
(define (sublist-numbers list)
  (sublist-numbers-ans list '());return function that give us the answer => list with only numbers
  )
(: sublist-numbers-ans : (Listof Any) (Listof Number) -> (Listof Number));#name: sublist-numbers-ans | #input: List of any types ans list of numbers | #output:List of number
(define (sublist-numbers-ans list listAns)
  (cond
    [(null? list) listAns];if the list null return '()
    [(number? (first list))(sublist-numbers-ans (rest list) (cons(first list) listAns))];if the first is number add him to listAns and cut the list without the first
    (else (sublist-numbers-ans (rest list) listAns));else, cut the list without the first
    )
  )
;==========Test:sublist-numbers==========
(test (sublist-numbers (list 'any "Benny" 10 'OP 8)) => '(8 10))
(test (sublist-numbers '(any "Benny" OP (2 3))) => null)
(test (sublist-numbers '()) => null)
(test (sublist-numbers (list -2 'any 11 10 'OP 8)) => '(8 10 11 -2))
(test (sublist-numbers (list '() 1 1 1 )) => '(1 1 1))

;==========Function:sublist-numbers==========
(: min&max-lists ((Listof(Listof Any)) -> (Listof (Listof Number))));#name: min&max-lists | #input: List of list of Any ('Matrix with any types') | #output:List of list of numbers ('Matrux with Numbers')
(define (min&max-lists matrix)
  (min&max-lists-ans matrix '());call function that get the metrix of Any and metrix that save the answer
  )
;==========Function:min&max-lists-ans==========
(: min&max-lists-ans : (Listof(Listof Any))(Listof (Listof Number))  -> (Listof (Listof Number)));#name: sublist-numbers-ans | #input: List of list of Any ('Matrix with any types') | #output:List of list of numbers ('Matrux with Numbers')
(define (min&max-lists-ans matrix ans)
  (cond
    [(null? matrix) ans];if the matrix null return '()
    [(null? (sublist-numbers (first matrix)));if the sub list without numbers 
     (min&max-lists-ans (rest matrix) (append ans (list null)))];then cut this sub list and append to ans null
    (else (min&max-lists-ans (rest matrix) (append ans (list (min-max (sublist-numbers (first matrix))))))); else cut this sub list and append to ans min max list with only numbers
    )
  )
;==========Function:min&max-lists-ans==========
(: min-max : (Listof Number) -> (Listof Number));like Q-1 but get list of numbers
(define (min-max someList)
  (list (getMin someList) (getMax someList)))

;==========Test:min&max==========
(test (min-max '( 2 3 2 7 5)) => '(2 7))
(test (min-max '( 2 2 2 2 2)) => '(2 2))
(test (min-max '(-1 -2 -4 -7 -100)) => '(-100 -1))
(test (min-max '(-1 2 -4 -7 0)) => '(-7 2))
;==========Test:min&max-lists==========
(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))
(test (min&max-lists '()) => null)
(test (min&max-lists '((-4 5 1 5 L) (4 5 105 7 3 2 1) ())) => '((-4 5) (1 105) ()))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;==========Type:KeyStack==========
(define-type KeyStack
  [Push Symbol String KeyStack]
  [EmptyKS]
  )

(: pop-stack : KeyStack -> (U KeyStack #f)) 
(define (pop-stack stack)
  ( cases stack
     [( EmptyKS ) #f ]
     [(Push a b c) c])
  )

  
;==========Test:EmptyKS + Push==========
(test (EmptyKS) => (EmptyKS))
(test (KeyStack? (Push 'b "B" (Push 'a "A" (EmptyKS)))) => #t)
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>
      (Push 'b "B" (Push 'a "A" (EmptyKS))))

;(test (search-stack 'a (EmptyKS)) => #f)
;(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
;(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A"
(EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;==========Function:is-odd?==========
(: is-odd? : Natural -> Boolean);#name: is-odd? | #input: Natural number | #output: boolean (#t or #f)
(define (is-odd? x)
 (if (zero? x)
 false
 (is-even? (- x 1))))
;==========Function:is-even?==========
(: is-even? : Natural -> Boolean);#name: is-even? | #input: Natural number | #output: boolean (#t or #f)
(define (is-even? x)
 (if (zero? x)
  true
 (is-odd? (- x 1))))

;The functions will get one number and transfer it to each other with a reduced value in one until the value will be equal to zero
;Start from is-even? func
;Example of running -> (test (not (is-even? 3)))
;Step 1:
;(if (zero? 3) true (is-odd? (- 3 1))))
;Step 2:
;(if (zero? 2)false(is-even? (- 2 1))))
;Step 3:
;(if (zero? 1) true (is-odd? (- 1 1))))
;Step 4:
;(if (zero? 0)false
;return false

;Start from is-odd? func
;Example of running -> (test (not (is-odd? 3)))
;Step 1:
;(if (zero? 3) false (is-even? (- 3 1))))
;Step 2:
;(if (zero? 2) true (is-odd? (- 2 1))))
;Step 3:
;(if (zero? 1) false (is-even? (- 1 1))))
;Step 4:
;(if (zero? 0) true
;return true

;==========Test:is-odd?==========
(test (not (is-odd? 12)))
(test (is-odd? 1))
(test (not (is-odd? 0)))
;==========Test:is-even?==========
(test (is-even? 12))
(test (is-even? 0))
(test (not (is-even? 1)))


;==========Function:every?==========
(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;input: two arguments both ALL dependentent on the same type A. The first argument is a function get argument of type A and returns a Boolean
;the second argument is a list of type A. The function working recursively by checking that the list is not empty.
;he take the first item from the list and check if its keeps the pred and the result with the rest of the list recursively.
;the functions return true if all the item keep the predicate, if the AND operator will break (at least item not keep on pred) then the function return false 
(define (every? pred lst)
 (or (null? lst)
 (and (pred (first lst))
 (every? pred (rest lst))))
  )
;; An example for the usefulness of this polymorphic function

;==========Function:all-even==========
(: all-even? : (Listof Natural) -> Boolean);#name: all-even? | #input: List of Natural | #output: boolean (#t or #f)
;the function get a list of natural numbers and return true of all elements are even.
;every function wiil check each item if is even, and return false if at leat one is not.
(define (all-even? lst)
 (every? is-even? lst))

;==========Test:all-even?==========
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))

;==========Function:every2?==========
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
Boolean))
;Similarly to every function this functions get two variable A and B and two pred one for A and one for B both return boolean, and two lists of A and B.
;the function check that each item of both list A and B in the same index, and both keep on the pred1 -> A and pred2 -> B.
(define (every2? pred1 pred2 lst1 lst2)
 (or (null? lst1) ;; both lists assumed to be of same length
 (and (pred1 (first lst1))
 (pred2 (first lst2))
 (every2? pred1 pred2 (rest lst1) (rest lst2)))))
