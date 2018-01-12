#lang planet neil/sicp

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

; Bloadcast a message to all related constraints from Connector
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ; Except sender constraint
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

; Connector
(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      ; 既に設定されている値がない場合のみ値をセット
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Constradiction" (list value newval)))
            (else 'ignored)))

    (define (forget-my-value retractor)
      ; 値をセットしたオブジェクトのみ値をできる
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNCETOR" request))))
    me))

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lsot-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
me)

;; 加算器制約
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      ; a1 と a2 が揃った場合a1 + a2でsumを計算する
      ((and (has-value? a1) (has-value? a2))
       (set-value! sum
                   (+ (get-value a1) (get-value a2))
                   me))
      ; a1 と sum が揃った場合は sum - a1 で a2を計算する
      ((and (has-value? a1) (has-value? sum))
       (set-value! a2
                   (- (get-value sum) (get-value a1))
                   me))
      ; a1 と sum が揃った場合は sum - a2 で a1を計算する
      ((and (has-value? a2) (has-value? sum))
       (set-value! a1
                   (- (get-value sum) (get-value a2))
                   me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
)


(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)

  me)


(define (test-adder)
  (let ((a1 (make-connector))
        (a2 (make-connector))
        (sum (make-connector))
        )
    (adder a1 a2 sum)
    (set-value! a1 1 'user)
    (set-value! sum 10 'user)
    (probe "a2:" a2)
    ))


(define (test-multiplier)
  (let ((m1 (make-connector))
        (m2 (make-connector))
        (product (make-connector)))
    (multiplier m1 m2 product)
    (set-value! m1 2 'user)
    (set-value! product 4 'user)
    (probe "m2" m2) 
    ))


(define (averager a b c)
  (let ((sum (make-connector))
        (m (make-connector)))
    (adder a b sum)
    (multiplier m c sum)
    (constant 2 m)
  'OK))


(define (test-averager)
  (let ((a (make-connector))
        (b (make-connector))
        (c (make-connector)))
    (averager a b c)
    (set-value! a 2 'user)
    (set-value! b 4 'user)
    (probe "averager" c)
    ))


(define (squarer a b)
  (multiplier a a b))

(define (test-squarer)
  (let ((a (make-connector))
        (b (make-connector)))
    (squarer a b)
    (set-value! b 9 'user)
    (probe "squarer" a)
    ))

; ２乗はもとめられるが逆方向の平方根がもとめられない。 multiplier コネクタの一つを残してうめることができないから

(test-adder)
(test-multiplier)
(test-averager)
(test-squarer)