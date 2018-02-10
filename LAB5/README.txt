Brinich,Patrick
Changlani, Karishma

Question 1: 30 mins

When we run the segment of code we get 1 since it's lazy evaluation which means that its not calculating the value of all the function until it needs to it only runs the primitive functions. This means that we the answer we need which is 1 despite the fact that (/ 1 0) is an invalid operation but since it never evaluates it we get our answer as 1 since it expands function definition BEFORE evaluating (/ 1 0). If we used the applicative order we wouldn't get this and instead we get an error since evaluator would first check the first variable's value before applying the second lambda function and would result in an error since it immediately evaluates the value of (/ 1 0) and encounters an error with the division  

Question 2: 1 hr

In normal execution ( non-momoized version ) and momoized version

(define (square x) (* x x))
and 
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (f a) (sum-of-squares (+ a 1) (* a 2)))
The best way to see the difference is to travel down f
So as lazy calculation for both interpreters we go down the recursive functions to get to
(sum-of-squares ( + (square (+ a 1)) (square (* a 2))))

Now if we look at the expansion of 
(square (+ a 1))
which gives us 
(square (* (+ a 1) (+ a 1))
Now this is where both the interpreters differ. 

For the non momoized version the (+ a 1) get's evaluated twice esentially doubling the time necessary to evaluate (+ a 1) 
However, the momoized calculates (+ a 1) and uses thunks to know what the value already is without having to evaluate it twice. Which saves the evaluation a lot of time make momoized faster due to non repeated operations. Same is true of execuation of (* a 2)

Question 3: 1 hr

When we trace through 
(prime-sum-pair '(1 3 5 8) '(20 35 110))
We first look at (amb 1 3 5 8) 
By definition of amb we look at each of those values 1, 3, 5, and 8. 
And for each of those we compare them to (amb 20 35 110) 

First prime-sum-pair will choose 1 and 20 in (+ a b) and we get 21 since require-prime fails we get amb again which now chooses 35 and prime-sum-pair checks again with require function this happens until we reach the end of list (20 35 110) 

This ofcourse gives us the end of the list and amb encounters a failure backtracks to 20 and the first amb moves forward with the next choice. The program checks 20 + 3 which gives us a prime number and require function doesn't return amb and instead returns (3 20)

The next time the function remembers where amb left of last and checks ahead giving us (3 110) and finally (8 35) 

The function gives us 3 full solutions. Amb does not go back to an older solution since that choice was already made by amb and once it reaches it failure with both lists it fails and no more solutions are to be found. 

Question 4: 30 mins 

(define (an-integer-starting-from n)
 (amb n (an-integer-starting-from (+ n 1))))
 
 Modified function for prime-sum-pair

(define (prime-sum-pair-incremental n)
 (let ((a (an-integer-starting-from n))
 (b (an-integer-starting-from n)))
 (require (prime? (+ a b)))
 (list a b)))
 
(prime-sum-pair-incremental 2)




