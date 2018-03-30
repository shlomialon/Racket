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

