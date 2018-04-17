#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
    
    (define rember
  (lambda (x lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? x (car lat)) (cdr lat))
      (else
       (cons (car lat) (rember x (cdr lat)))))))

(define multirember
  (lambda (x lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? x (car lat)) (multirember x (cdr lat)))
         (else
          (cons (car lat) (multirember x (cdr lat)))))))))
      

(define firsts
    (lambda (x)
      (cond
        ((null? x) (quote ()))
        (else (cons (car (car x)) (firsts (cdr x)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
      (else
       (cons (car lat)(insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons new lat))
      (else
       (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
  (cond
    ((null? lat) (quote ()))
    ((eq? old (car lat)) (cons new (cdr lat)))
    (else
     (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
      (else
       (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
         (else
          (cons (car lat) (multiinsertR new old (cdr lat)))))))))
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? old (car lat)) (cons new (cons (car lat) (multiinsertL new old (cdr lat)))))
         (else
          (cons (car lat) (multiinsertL new old (cdr lat)))))))))

(define add1
  (lambda (x)
  (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define zeros?
  (lambda (x)
    (eq? x 0)))

(define o+
  (lambda (m n)
    (cond
      ((zeros? n) m)
      (else
       (o+ (add1 m) (sub1 n))))))

(define o-
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else
       (o- (sub1 m) (sub1 n))))))

(define ox
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else
       (o+ m (ox m (sub1 n)))))))

(define o^
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else
       (ox m (o^ m (sub1 n)))))))


(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (o+ (car tup) (addtup (cdr tup)))))))

(define tup+
  (lambda (lat1 lat2)
    (cond
      ((and (null? lat1) (null? lat2)) (quote ()))
      ((null? lat1) lat2)
      ((null? lat2) lat1)
      (else
       (cons (o+ (car lat1) (car lat2)) (tup+ (cdr lat1) (cdr lat2)))))))


(define >
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
       (> (sub1 m) (sub1 n))))))

(define pick
  (lambda (x lat)
    (cond
      ((and (zero? (sub1 x))) (car lat))
      (else
       (pick (sub1 x) (cdr lat))))))

(define strlength
  (lambda (x)
    (cond
      ((null? x) 0)
      (else
       (add1 (strlength (cdr x)))))))

(define no-number
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((number? (car lat)) (no-number (cdr lat)))
         (else
          (cons (car lat) (no-number (cdr lat)))))))))

(define all-number
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((number? (car lat)) (cons (car lat) (all-number (cdr lat))))
         (else
          (all-number (cdr lat))))))))


(define rember*
  (lambda (x lat)
    (cond
      ((null? lat) (quote ()))
      ((atom? (car lat))
       (cond
         ((eq? x (car lat)) (rember* x (cdr lat)))
         (else
          (cons (car lat) (rember* x (cdr lat))))))
      (else
       (cons (rember* x (car lat)) (rember* x (cdr lat)))))))

(define insertR*
  (lambda (new old lat)
  (cond
    ((null? lat) (quote ()))
    ((atom? (car lat))
     (cond
       ((eq? (car lat) old) (cons (car lat) (cons new (insertR* new old (cdr lat)))))
       (else
        (cons (car lat) (insertR* new old (cdr lat))))))
     (else
      (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))))))


(define occur*
  (lambda (x lat)
  (cond
    ((null? lat) 0)
    ((atom? (car lat))
     (cond
       ((eq? (car lat) x) (add1 (occur* x (cdr lat))))
       (else
        (occur* x (cdr lat)))))
    (else
     (+ (occur* x (car lat)) (occur* x (cdr lat)) )))))

(define insertL*
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((atom? (car lat))
       (cond
         ((eq? (car lat) old) (cons new (cons old (insertL* new old (cdr lat)))))
         (else
          (cons (car lat) (insertL* new old (cdr lat))))))
      (else
       (cons (insertL* new old (car lat)) (insertL* new old (cdr lat)))))))

(define member*
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((atom? (car lat))
       (cond
         ((eq? a (car lat)) #t)
         (else
          (member* a (cdr lat)))))
      (else
       (or (member* a (car lat)) (member* a (cdr lat)))))))


(define leftmost
  (lambda (lat)
    (cond
      ((atom? (car lat)) (car lat))
      (else
       (leftmost (car lat))))))

(define eqan?
  (lambda (lat1 lat2)
    (cond
      ((and (number? lat1) (number? lat2)) (= lat1 lat2))
      ((or (number? lat1) (number? lat1)) #f)
      (else
       (eq? lat1 lat2)))))


;(define eqlist?
;  (lambda (lat1 lat2)
;    (cond
;      ((and (null? lat1) (null? lat2)) #t)
;      ((or (null? lat1) (null? lat2)) #f)
;      ((and (atom? (car lat1)) (atom? (car lat2))) (and (eqan? (car lat1) (car lat2)) (eqlist? (cdr lat1) (cdr lat2))))
;      ((or (atom? (car lat1)) (atom? (car lat2))) #f)
;      (else
;       (and (eqlist? (car lat1) (car lat2)) (eqlist? (cdr lat1) (cdr lat2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else
       (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (number? (car aexp)) (number? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (car (cdr aexp)) (quote +)) (o+ (value (car aexp)) (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote x)) (ox (value (car aexp)) (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote ^)) (o^ (value (car aexp)) (value (car (cdr (cdr aexp)))))))))

(define valueone
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (operator aexp) (quote +)) (o+ (valueone (1st-sub-exp aexp)) (valueone (2nd-sub-exp aexp))))
      ((eq? (operator aexp) (quote x)) (ox (valueone (1st-sub-exp aexp)) (valueone (2nd-sub-exp aexp))))
      ((eq? (operator aexp) (quote ^)) (o^ (valueone (1st-sub-exp aexp)) (valueone (2nd-sub-exp aexp)))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define member?
  (lambda (x lat)
    (cond
      ((null? lat) #f)
      (else
       (or (equal? x (car lat)) (member? x (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else
       (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat)) (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))
      (else
       (cons (car lat) (makeset (cdr lat)))))))

(define subset?
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) #t)
      (else
       (cond
         ((member? (car lat1) lat2) (subset? (cdr lat1) lat2))
         (else
          #f))))))

(define subsetand?
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) #t)
      (else
       (and (member? (car lat1) lat2) (subsetand? (cdr lat1) lat2))))))

(define eqset?
  (lambda (lat1 lat2)
    (and (subsetand? lat1 lat2) (subsetand? lat2 lat1))))

(define intersect?
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) #f)
      (else
       (or (member? (car lat1) lat2) (intersect? (cdr lat1) lat2))))))

(define intersect
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) (quote ()))
      ((member? (car lat1) lat2) (cons (car lat1) (intersect (cdr lat1) lat2)))
      (else
       (intersect (cdr lat1) lat2)))))

(define union
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) lat2)
      ((member? (car lat1) lat2) (union (cdr lat1) lat2))
      (else
       (cons (car lat1) (union (cdr lat1) lat2))))))

(define set
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) (quote ()))
      ((member? (car lat1) lat2) (set (cdr lat1) lat2))
      (else
       (cons (car lat1) (set (cdr lat1) lat2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else
       (intersect (car l-set) (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (exp)
    (cond
      ((null? exp) #f)
      ((atom? exp) #f)
      ((null? (cdr exp) #f))
      ((null? (cdr (cdr exp)) #t))
      (else
       #f))))

(define fun?
  (lambda (exp)
    (set? (firsts exp))))

(define first
  (lambda (exp)
    (car exp)))

(define second
  (lambda (exp)
    (car (cdr exp))))

(define build
  (lambda (lat1 lat2)
    (cons lat1 (cons lat2 (quote())))))


(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else
       (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? a (car l)) (cdr l))
        (else
         (cons (car l) ((rember-f test?) a (cdr l))))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) o+)
      ((eq? x (quote x)) ox)
      (else
       o^))))

(define value-f
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (car nexp)) (car (cdr nexp)) (car (cdr (cdr nexp))))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else
         (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(define multiremberT
  (lambda (test?)
    (lambda (lat)
      (cond
        ((null? lat) (quote ()))
        ((test? (car lat)) ((multiremberT test?) (cdr lat)))
        (else
         (cons (car lat) ((multiremberT test?) (cdr lat))))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? oldR (car lat)) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else
       (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote ())0 0))
      ((eq? oldL (car lat)) (multiinsertLR&co new oldL oldR (cdr lat)
                                              (lambda (newlat L R)
                                                (col (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? oldR (car lat)) (multiinsertLR&co new oldL oldR (cdr lat)
                                              (lambda (newlat L R)
                                                (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col(cons (car lat) newlat) L R)))))))


(define even?
  (lambda (n)
    (= (remainder n 2) 0)))

(define evens-only*
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((atom? (car lat))
       (cond
         ((even? (car lat)) (cons (car lat) (evens-only* (cdr lat))))
         (else
          (evens-only* (cdr lat)))))
       (else
        (cons (evens-only* (car lat)) (evens-only* (cdr lat)))))))


(define evens-only*&co
  (lambda (lat col)
    (cond
      ((null? lat) (col (quote ()) 1 0))
      ((atom? (car lat))
       (cond
         ((even? (car lat)) (evens-only*&co (cdr lat)
                                            (lambda (newlat evenmul oddsum)
                                              (col (cons (car lat) newlat) (* evenmul (car lat)) oddsum))))
         (else
          (evens-only*&co (cdr lat)
                          (lambda (newlat evenmul oddsum)
                            (col newlat evenmul (+ oddsum (car lat))))))))
      (else
       (evens-only*&co (car lat)
                       (lambda (lata evenmula oddsuma)
                         (evens-only*&co (cdr lat)
                                         (lambda (latb evenmulb oddsumb)
                                           (col (cons lata latb)
                                                (* evenmula evenmulb)
                                                (+ oddsuma oddsumb))))))))))


(define eq?-b
  (eq?-c (quote b)))

;(multirember 'b '(a b c b))
;(multiinsertR 'c 'b '(a b d b e c f c))
;(multiinsertL 'b 'c '(a b d b e c f c))

;(add1 8)
;(sub1 8)

;(o+ 9 8)
;(o- 9 8)

;(addtup '(1 2 3 4 5 6 7 8 9 10))

;(ox 7 8)

;(tup+ '(1 2 3 4) '(4 3 2 1 8 7 9))
;(> 12 100)
;(> 100 10)
;(> 3 3)

;(pick 2 '(a b c d))
;(pick 0 '(a))

;(strlength '(a b c d f e))

;(no-number '(5 pears 6 prunes 9 datas))
;(all-number '(5 pears 6 prunes 9 datas))

;(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
;(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
;(insertL* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
;(occur* 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

;(member* 'chips '((potato) (chips ((with) fish) (chips))))
;(leftmost '(((hot)) ff))

;(eqan? 'lost 'lost)

;(eqlist? '(beff ((sausage)) (and (soda))) '(beff ((sausage)) (and (soda))))

;(rember 'b '(a b c d e))

;(numbered? '(1 + 2))

;(value '((5 ^ 5) + (1 + 9)))
;(valueone '(+ (+ 1 1) 6))

;(member? '3 '(apple 3 pears 4 9 apple 3 4))
;(set? '(apple 3 pears 4 9 apple 3 4))

;(makeset '(apple peach pear peach plum apple lemon peach))

;(subsetand? '(4 pounds of horseradish) '(4 poudnds of horseradish and f))
;(eqset? '(6 larges chickens with wings) '(wings with chickens larges 6))

;(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))
;(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
;(set '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))

;(intersectall '((6 pears and) (3 peaches and 6 pepers) (8 pears and 6 plums) (and 6 prunes with some apples)))
;(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))
;(firsts '((d 4) (b 0) (b 9) (e 5) (g 4)))
;(revrel '((8 a) (pumpkin pie) (got sick)))

;((rember-f eq?) 'a '(a b c d e))
;(value-f '(^ 5 5))
;((multirember-f eq?) 'b '(a b c b))
;((multiremberT eq?-b) '(a b c b))

;(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(evens-only*&co '((9 1 2 8)3 10 ((9 9) 7 6) 2) (lambda (newl product sum)
                                                (cons sum (cons product newl))))