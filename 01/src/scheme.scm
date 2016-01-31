(define gcd			
   (lambda (a b)				
      (if (= a b)
         a
         (if (> a b)
            (gcd (- a b) b)
            (gcd a (- b a))))))
        
(define fac (lambda (n)
   (if (= n 0)
      1
      (* n (fac (- n 1))))))

(define fib (lambda (n)
   (if (= n 0)
      0
      (if (= n 1)
         1
         (+ (fib (- n 1)) (fib (- n 2)))))))

(define ack (lambda (m n)
   (if (= m 0)
      (+ n 1)
      (if (= n 0)
         (ack (- m 1) 1)
         (ack (- m 1) (ack m (- n 1)))))))
     
(define prime (lambda (n k)
   (if (> (* k k) n) 
       #t
       (if (= (mod  n k) 0) 
           #f
           (prime n (+ k 1))))))

(define isPrime?(lambda (n) 
     (and (> n 1) (prime n 2))))
 
 (define (iterate n f)
   (if (= n 0)
       (lambda (x) x)
       (lambda (x) (f ((iterate (- n 1) f) x ))
         )
    )
)

(define addN5 (lambda (n) (+ n 5)))
(define addN1 (lambda (n) (+ n 1)))
