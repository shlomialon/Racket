#lang pl 02

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;==========BNF for LE==========
#| 
<LE>  ::= <VAR>    (1)
        | <LIST>   (2)

<VAR> ::= <num>    (3)
       | '<sym>    (4)

<LIST>  ::= null                     (5)
         | { append <LIST> ... }     (6)
         | { cons <LE> <LIST> }      (7)
         | {list <LE> ... }          (8)
|# 

#|
Expretion - (cons 311 (cons (append (cons 'shl null) (list 'al 'on)) null))

<LE> -> <LIST> -> (cons <VAR> <LIST>) -> (cons <num> <LIST>) -> (cons 311 (cons <LIST> <LIST>)) -> (cons 311 (cons (append <LIST> ...) <LIST>)) -> (cons 311 (cons (append <LIST> <LIST>) <LIST>))
   -> (cons 311 (cons (append (cons <VAR> <LIST>) (list <LE>)) <LIST>)) -> (cons 311 (cons (append (cons 'shl null) (list <LE> ...)) <LIST>)) -> (cons 311 (cons (append (cons 'shl null) (list <LE><LE>)) <LIST>))
   -> (cons 311 (cons (append (cons 'shl null) (list 'al 'on)) <LIST>)) -> (cons 311 (cons (append (cons 'shl null) (list 'al 'on)) null))
|#
#|==========Test:Tree expretion building:==========
 Output should be - '(311 (shl al on))
 (cons 311 (1)) =>
            | 
           (cons (2) null)
                  |
                 (append (3) (4)
                          |   |
                          (cons 'shl null) = 'shl
                              (list 'al 'on) = 'alon

                         (append 'shl 'alon) = 'shl alon
                         (cons '(shl  alon) null) = 'shl alon
                         (cons 311 '((shl alon))) = '(311 (shl alon))
|#


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;its help me for this question - https://stackoverflow.com/questions/22560573/how-to-do-square-in-racket

;==========Function:square==========
#|name: square | #input: Number | #output: square of number|#
(: square : Number -> Number)
(define (square num) (* num num))

;==========Test:square==========
(test (square 2) => 4)
(test (square 1) => 1)
(test (square 0)  => 0)
(test (square -5)  => 25)

;==========Function:sum-of-squares==========
#|this question take me 2 houres because I had to understand the syntax of use fold1 and map
name: sum-of-squares | #input: List of numbers | #output: sum of square of list numbers
(foldl - 1 '(1 2 3 4 5)) is actually equivalent to (- 5 (- 4 (- 3 (- 2 (- 1 1))))) - i using foldl to sum the numbers after they been squared|#
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares numbers)
  (foldl + 0 (map square numbers)
 ))

;==========Test:sum-of-squares==========
(test (sum-of-squares (list 1 2 3)) => 14)
(test (sum-of-squares (list 1)) => 1)
(test (sum-of-squares (list -2 -2 -2)) => 12)
(test (sum-of-squares (list 1.2 9.4)) => 89.80000000000001)
(test (sum-of-squares (list -4 -2)) => (+ 16 4))
(test (sum-of-squares '(-6 0 0)) => 36)
(test (sum-of-squares '()) => 0)


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;this question take me 3 houres because I had to understand if i right in my solution
;==========Type:define-type BINTREE==========
(define-type BINTREE
  [Leaf Number]
  [Node BINTREE BINTREE])

;==========Test:define-type BINTREE==========
(test (Leaf 1) => (Leaf 1))
(test (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) => (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
(test (Node (Leaf 1) (Leaf 2)))

;==========Func:tree-map==========
#|name: tree-map | #input:  function which maps number to number, and a binary tree | #output: binary tree
Maps the given function recursively over the given tree, returning
a tree of the results with the same shape.  Note the higher-order
type.|#
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map f tree)
  (cases tree
    [(Leaf n) (Leaf (f n))]
    [(Node l r) (Node (tree-map f l) (tree-map f r))]))

;==========Test:tree-map==========
(define 1357-tree(Node (Node (Leaf 1) (Leaf 3))(Node (Leaf 5) (Leaf 7))))
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))  => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map add1 1357-tree) => (Node (Node (Leaf 2) (Leaf 4)) (Node (Leaf 6) (Leaf 8))))
(test (tree-map add1 (Leaf 1)) => (Leaf 2))
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map sub1 (Leaf 9)) => (Leaf 8))
(test (tree-map abs (Node (Leaf -142) (Leaf -0.2))) => (Node (Leaf 142) (Leaf 0.2)))
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))


;==========Func:tree-fold==========
#|name: tree-fold | #input: function which receive 2 values of type A and return type A value | #output: type A value
`folds' a tree using a combiner function for combining the two
sub-results in case of a node, and a leaf function for a leaf case.|#
(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))
(define (tree-fold combiner leafer tree)
  (cases tree
    [(Leaf n)   (leafer n)]
    [(Node l r) (combiner (tree-fold combiner leafer l)
                          (tree-fold combiner leafer r))]))

;==========Test:tree-fold==========
(: tree-flatten : BINTREE -> (Listof Number))
(define (tree-flatten tree)
(tree-fold (inst append Number) (inst list Number) tree))
(define 246-tree (Node (Leaf 2) (Node (Leaf 4) (Leaf 6))))
(test (tree-flatten 1357-tree) => '(1 3 5 7))
(test (tree-flatten 246-tree) => '(2 4 6))
(test (tree-flatten (Node 1357-tree 246-tree)) => '(1 3 5 7 2 4 6))
(test (tree-flatten (Leaf 0)) => '(0))
(test (tree-fold + add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => 9)
(test (tree-fold + add1 (Leaf 9)) => 10)

;==========Func:tree-reverse==========
#|Consumes two trees and makes a tree out of them in reverse order.
This is a helper for `tree-reverse' below.|#
(: NodeRev : BINTREE BINTREE -> BINTREE)
(define (NodeRev r l)
  (Node l r))

#|name: tree-reverse | #input: fbinary tree | #output: binary tree
returns a "mirror-image" copy of the given BINTREE|#
(: tree-reverse : BINTREE -> BINTREE)
(define (tree-reverse tree)
  (tree-fold NodeRev Leaf tree))

;==========Test:tree-reverse==========
(test (equal? (reverse (tree-flatten 1357-tree))
              (tree-flatten (tree-reverse 1357-tree))))
(test (equal? (reverse (tree-flatten 246-tree))
              (tree-flatten (tree-reverse 246-tree))))
(test (equal? (reverse (tree-flatten (Leaf 0)))
              (tree-flatten (tree-reverse (Leaf 0)))))
(test (tree-reverse (Leaf 1)) => (Leaf 1))
(test (tree-reverse (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Node (Leaf 2) (Leaf 1)))))) => (Node (Node (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5)))
(test (equal? (reverse (tree-flatten (Node (Leaf 1) (Leaf 2)))) (tree-flatten (tree-reverse (Node (Leaf 1) (Leaf 2))))) => #t)
(test (equal? (reverse (tree-flatten (Node (Leaf 1) (Leaf 2)))) (tree-flatten (tree-reverse (Node (Leaf 2) (Leaf 1))))) => #f)
(test (tree-reverse  (Node (Leaf 2) (Leaf 3))) => (Node (Leaf 3) (Leaf 2)))
(test (tree-reverse  (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Node (Leaf 3) (Leaf 2)) (Leaf 1)))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#| The problem is that the syntax is ambiguous unless we fix the semantics to specify some fixed order of evaluation for subexpressions.
   Without this, the given expression:

      {* {+ {set 1} {set 2}} get}

   can evaluate to 6 if we always evaluate left-to-right,its can be
   unspecified or an error right-to-left, and it can even evaluate
   to 3.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   The following grammar specifies a MAE program as a non-empty
   the sequence of sub-computations, all except the last must `set' the
   result into memory, and the first is not allowed to use `get'.
   It has three non-terminals,
   <MAE> - is for whole programs
   <AE> - is for simple arithmetic expressions
   <AE/get> - is for arithmetic expressions that can contain `get'.

     <MAE>  ::= {seq <AE>}
              | {seq {set <AE>}
                     {set <AE/get>}
                     ...
                     <AE/get>}

     <AE>   ::= <num>
              | { + <AE> <AE> }
              | { - <AE> <AE> }
              | { * <AE> <AE> }
              | { / <AE> <AE> }

     <AE/get> ::= <num>
              | { + <AE/get> <AE/get> }
              | { - <AE/get> <AE/get> }
              | { * <AE/get> <AE/get> }
              | { / <AE/get> <AE/get> }
              | get


example 1:
{seq set {+ 311 610} 547} 
<MAE> => {seq <set> <get>}
=>  {seq {set <AE>}  {seq {set <AE>}  {seq {set <AE>}  {seq {set <AE>} <AE/get>}: {seq {set {+ 311 610} <get>}}
=> {seq {set {+ 3 1}} {set {/ 1 6}} {set {* 1 0}} {set {- 5 4}} {+ <get2> 7}}: {seq {+ 311 610 } 547}

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
|#