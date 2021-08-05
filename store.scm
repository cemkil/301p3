(module store (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
   
  (provide initialize-store! newref-rec reference? optimize-store newref print-store deref setref! get-store-length
    instrument-newref get-store-as-list)
  
  (define instrument-newref (make-parameter #f))
  
  ;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;
  
  ;;; world's dumbest model of the store:  the store is a list and a
  ;;; reference is number which denotes a position in the list.

  ;; the-store: a Scheme variable containing the current state of the
  ;; store.  Initially set to a dummy variable.
  (define the-store 'uninitialized)

  ;; empty-store : () -> Sto
  ;; Page: 111
  (define empty-store
    (lambda () '()))
  
  ;; initialize-store! : () -> Sto
  ;; usage: (initialize-store!) sets the-store to the empty-store
  ;; Page 111
  (define initialize-store!
    (lambda ()
      (set! the-store (empty-store))))

  ;; get-store : () -> Sto
  ;; Page: 111
  ;; This is obsolete.  Replaced by get-store-as-list below
  (define get-store
    (lambda () the-store))

  (define optimize-store
    (lambda ()
      (set! the-store (optimize-helper '() the-store))))

  (define optimize-helper
    (lambda (temp the-store)
      (if (null? (cdr the-store))
          temp
          (if (eq? -1 (car the-store))
              (optimize-helper temp (cdr the-store))
              (optimize-helper (append temp (list (car the-store))) (cdr the-store))))
      ))

  (define print-store (lambda () (display (car the-store))))

  ;; reference? : SchemeVal -> Bool
  ;; Page: 111
  (define reference?
    (lambda (v)
      (integer? v)))
  
  (define get-store-length (lambda () (length the-store)))
  
  
  ;; newref : ExpVal -> Ref
  ;; Page: 111
  (define newref
    (lambda (val)
      (let ((next-ref (length the-store)))
        (set! the-store
              (append the-store (list val)))
        (when (instrument-newref)
            (eopl:printf 
             "newref: allocating location ~s with initial contents ~s~%"
             next-ref val))                     
        next-ref)))

  (define newref-rec (lambda (times value)
                       (if (> times 0)
                          (let ((x (newref value)))
                            (newref-rec-helper (- times 1) value)
                             x) 23)))
  
  (define newref-rec-helper (lambda (times value)
                       (if (= times 0)
                           (newref value)
                           (let ((y (newref value)))
                             (newref-rec-helper (- times 1) value)
                             ) )))

  ;; deref : Ref -> ExpVal
  ;; Page 111
  (define deref 
    (lambda (ref)
      (list-ref the-store ref)))

  ;; setref! : Ref * ExpVal -> Unspecified
  ;; Page: 112
  (define setref!                       
    (lambda (ref val)
      (set! the-store
        (letrec
          ((setref-inner
             ;; returns a list like store1, except that position ref1
             ;; contains val. 
             (lambda (store1 ref1)
               (cond
                 ((null? store1)
                  (report-invalid-reference ref the-store))
                 ((zero? ref1)
                  (cons val (cdr store1)))
                 (else
                   (cons
                     (car store1)
                     (setref-inner
                       (cdr store1) (- ref1 1))))))))
          (setref-inner the-store ref)))))

  (define report-invalid-reference
    (lambda (ref the-store)
      (eopl:error 'setref
        "illegal reference ~s in store ~s"
        ref the-store)))

  ;; get-store-as-list : () -> Listof(List(Ref,Expval))
  ;; Exports the current state of the store as a scheme list.
  ;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
  ;;   where foo, bar, and baz are expvals.
  ;; If the store were represented in a different way, this would be
  ;; replaced by something cleverer.
  ;; Replaces get-store (p. 111)
   (define get-store-as-list
     (lambda ()
       (letrec
         ((inner-loop
            ;; convert sto to list as if its car was location n
            (lambda (sto n)
              (if (null? sto)
                '()
                (cons
                  (list n (car sto))
                  (inner-loop (cdr sto) (+ n 1)))))))
         (inner-loop the-store 0))))

  )