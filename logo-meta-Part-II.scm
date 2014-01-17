;;; logo-meta.scm      Part of programming project #4

;; Part II
;; Kevin Chen - CS61AS-an
;; Opal Kale - CS61AS-ca

;;; Differences between the book and this version:  Eval and apply have
;;; been changed to logo-eval and logo-apply so as not to overwrite the Scheme
;;; versions of these routines. An extra procedure initialize-logo has been
;;; added. This routine resets the global environment and then executes the
;;; driver loop. This procedure should be invoked to start the Logo
;;; evaluator executing.  Note: It will reset your global environment and all
;;; definitions to the Logo interpreter will be lost. To restart the Logo
;;; interpreter without resetting the global environment, just invoke
;;; driver-loop.  Don't forget that typing control-C will get you out of
;;; the Logo evaluator back into Scheme.

;;; Problems A1, A2, and B2 are entirely in logo.scm
;;; Problems 3, 7, and up require you to find and change existing procedures.

;;;  Procedures that you must write from scratch:

;;; Problem B1    eval-line

(define (eval-line line-obj env)
  (let ((output (logo-eval line-obj env)))
    (cond ((not (equal? output '=NO-VALUE=)) output)
	  ((ask line-obj 'empty?) '=NO-VALUE=)
	  (else (eval-line line-obj env)) )))	

;;; Problem 4    variables  (other procedures must be modified, too)
;;; data abstraction procedures

(define (variable? exp)
  (eq? (first exp) ':))

(define (variable-name exp)
  (bf exp))

;;; Problem A5   handle-infix

(define (de-infix token)
  (assoc token '((+ . sum)
		 (- . difference)
		 (* . product)
		 (/ . quotient)
		 (= . equalp)
		 (< . lessp)
		 (> . greaterp))))

(define (handle-infix value line-obj env)
  (if (ask line-obj 'empty?)
      value
      (let* ((next-token (ask line-obj 'next))
	     (infix (de-infix next-token)))
	(if infix
	    (let ((result (logo-eval (make-line-obj (se (cdr infix)
							value
							(eval-prefix line-obj env)))
				     env)))
	      (ask line-obj 'put-back result)
	      (logo-eval line-obj env) )
	    (begin
	      (ask line-obj 'put-back next-token)
	      value) ))))

;;; Problem B5    eval-definition

(define (eval-definition line-obj env) ;; 9: added env param
  (define (read-loop)
    (display "-> ")
    (flush)
    (let ((line (logo-read)))
      (if (equal? line '(end))
	  '()
	  (cons line (read-loop)) )))
  (define (get-formals line-obj)
    (if (ask line-obj 'empty?)
	'()
	(let ((next-token (ask line-obj 'next)))
	  (if (equal? next-token 'static)
	      '()
	      (if (variable? next-token)
		  (cons (variable-name next-token) (get-formals line-obj))
		  (error "Not a variable: " next-token) )))))
  (define (get-statics line-obj vars vals) ;; 9: added statics
    (if (ask line-obj 'empty?)
	(cons vars vals)
	(let* ((variable (ask line-obj 'next))
	       (value (car (collect-n-args 1 line-obj env #f))))
	  (get-statics line-obj (cons (variable-name variable) vars) (cons value vals)))))
	
  (let ((name (ask line-obj 'next))
	(formals (get-formals line-obj))
	(statics (get-statics line-obj '() '()))
	(body (read-loop)))
    (set! the-procedures
	  (cons (list name 'compound (length formals) (cons formals body) #f statics) ;; A8&9: added step-flag and statics
		the-procedures))
    (display name)
    (display " defined")
    (newline)
    '=NO-VALUE=))
    
;;; Problem 6    eval-sequence

(define (eval-sequence exps env step-flag) ;; A8: added step-flag arg
  (if (null? exps)
      '=NO-VALUE=
      (begin
	(if step-flag ;; A8: added condition
	    (begin
	      (display (car exps))
	      (display " >>>")
	      (flush)
	      (define temp (logo-read))))
	(let ((result (eval-line (make-line-obj (car exps))
				 env)))
	  (cond ((eq? result '=STOP=) '=NO-VALUE=)
		((output? result) (cdr result))
		((eq? result '=NO-VALUE=)
		 (eval-sequence (cdr exps) env step-flag))
		(else (logo-print (list "You don't say what to do with" result))) )))))
	      
(define (output? exp)
  (and (pair? exp) (eq? (car exp) '=OUTPUT=)))

;; Problem A8

(define (step wrd)
  (let ((proc (lookup-procedure wrd)))
    (if proc
	(set-car! (cdr (cdddr proc)) #t)
	(error "Not a procedure" wrd)))
  '=NO-VALUE=)

(define (unstep wrd)
  (let ((proc (lookup-procedure wrd)))
    (if proc
	(set-car! (cdr (cdddr proc)) #f)
	(error "Not a procedure" wrd)))
  '=NO-VALUE=)

(define (proc-step-flag proc)
  (cadr (cdddr proc)))

;; Problem B8

(define (test env t/f)
  (cond ((eq? t/f 'true) (define-variable! " test" #t env #t))
        ((eq? t/f 'false) (define-variable! " test" #f env #t))
        (else (error "Input to test not true or false" t/f)))
  '=NO-VALUE=)

(define (iftrue env instructlist)
  (let ((varval (lookup-variable-value " test" env)))
    (if (eq? varval #t)
	(run env instructlist)
	'=NO-VALUE=)))
        
(define (iffalse env instructlist)
  (let ((varval (lookup-variable-value " test" env)))
    (if  (eq? varval #f)
	 (run env instructlist)
	 '=NO-VALUE=)))

;; Problem 9

(define (statics proc)
  (caddr (cdddr proc)))


;;; SETTING UP THE ENVIRONMENT

(define the-primitive-procedures '())

(define (add-prim name count proc)
  (set! the-primitive-procedures
	(cons (list name 'primitive count proc)
	      the-primitive-procedures)))

(add-prim 'first 1 first)
(add-prim 'butfirst 1 bf)
(add-prim 'bf 1 bf)
(add-prim 'last 1 last)
(add-prim 'butlast 1 bl)
(add-prim 'bl 1 bl)
(add-prim 'word -2 word)
(add-prim 'sentence -2 se)
(add-prim 'se -2 se)
(add-prim 'list -2 list)
(add-prim 'fput 2 cons)

(add-prim 'sum -2 (make-logo-arith +))
(add-prim 'difference 2 (make-logo-arith -))
(add-prim '=unary-minus= 1 (make-logo-arith -))
(add-prim '- 1 (make-logo-arith -))
(add-prim 'product -2 (make-logo-arith *))
(add-prim 'quotient 2 (make-logo-arith /))
(add-prim 'remainder 2 (make-logo-arith remainder))

(add-prim 'print 1 logo-print)
(add-prim 'pr 1 logo-print)
(add-prim 'show 1 logo-show)
(add-prim 'type 1 logo-type)
(add-prim 'make '(2) make)

(add-prim 'step 1 step) ;A8
(add-prim 'unstep 1 unstep) ;A8
(add-prim 'test '(1) test) ;B8
(add-prim 'ift '(1) iftrue) ;B8
(add-prim 'iff '(1) iffalse) ;B8
(add-prim 'iffalse '(1) iffalse) ;B8
(add-prim 'iftrue '(1) iftrue) ;B8
(add-prim 'run '(1) run)
(add-prim 'if '(2) logo-if)
(add-prim 'ifelse '(3) ifelse)
(add-prim 'equalp 2 (logo-pred (make-logo-arith equalp)))
(add-prim 'lessp 2 (logo-pred (make-logo-arith <)))
(add-prim 'greaterp 2 (logo-pred (make-logo-arith >)))
(add-prim 'emptyp 1 (logo-pred empty?))
(add-prim 'numberp 1 (logo-pred (make-logo-arith number?)))
(add-prim 'listp 1 (logo-pred list?))
(add-prim 'wordp 1 (logo-pred (lambda (x) (not (list? x)))))

(add-prim 'stop 0 (lambda () '=stop=))
(add-prim 'output 1 (lambda (x) (cons '=output= x)))
(add-prim 'op 1 (lambda (x) (cons '=output= x)))

(define (pcmd proc) (lambda args (apply proc args) '=no-value=))
(add-prim 'cs 0 (pcmd cs))
(add-prim 'clearscreen 0 (pcmd cs))
(add-prim 'fd 1 (pcmd fd))
(add-prim 'forward 1 (pcmd fd))
(add-prim 'bk 1 (pcmd bk))
(add-prim 'back 1 (pcmd bk))
(add-prim 'lt 1 (pcmd lt))
(add-prim 'left 1 (pcmd lt))
(add-prim 'rt 1 (pcmd rt))
(add-prim 'right 1 (pcmd rt))
(add-prim 'setxy 2 (pcmd setxy))
(add-prim 'setx 1 (lambda (x) (setxy x (ycor)) '=no-value=))
(add-prim 'sety 1 (lambda (y) (setxy (xcor) y) '=no-value=))
(add-prim 'xcor 0 xcor)
(add-prim 'ycor 0 ycor)
(add-prim 'pos 0 pos)
(add-prim 'seth 1 (pcmd setheading))
(add-prim 'setheading 1 (pcmd setheading))
(add-prim 'heading 0 heading)
(add-prim 'st 0 (pcmd st))
(add-prim 'showturtle 0 (pcmd st))
(add-prim 'ht 0 (pcmd ht))
(add-prim 'hideturtle 0 (pcmd ht))
(add-prim 'shown? 0 shown?)
(add-prim 'pd 0 (pcmd pendown))
(add-prim 'pendown 0 (pcmd pendown))
(add-prim 'pu 0 (pcmd penup))
(add-prim 'penup 0 (pcmd penup))
(add-prim 'pe 0 (pcmd penerase))
(add-prim 'penerase 0 (pcmd penerase))
(add-prim 'home 0 (pcmd home))
(add-prim 'setpc 1 (pcmd setpc))
(add-prim 'setpencolor 1 (pcmd setpc))
(add-prim 'pc 0 pc)
(add-prim 'pencolor 0 pc)
(add-prim 'setbg 1 (pcmd setbg))
(add-prim 'setbackground 1 (pcmd setbg))

(add-prim 'load 1 meta-load)

(define the-global-environment '())
(define the-procedures the-primitive-procedures)

;;; INITIALIZATION AND DRIVER LOOP

;;; The following code initializes the machine and starts the Logo
;;; system.  You should not call it very often, because it will clobber
;;; the global environment, and you will lose any definitions you have
;;; accumulated.

(define (initialize-logo)
  (set! the-global-environment (list (cons '() '())))
  (set! the-procedures the-primitive-procedures)
  (driver-loop))

(define (driver-loop)
  (define (helper)
    (prompt "? ")
    (let ((line (logo-read)))
      (if (not (null? line))
  	  (let ((result (eval-line (make-line-obj line)
				   the-global-environment)))
	    (if (not (eq? result '=no-value=))
		(logo-print (list "You don't say what to do with" result))))))
    (helper))
  (logo-read)
  (helper))

;;; APPLYING PRIMITIVE PROCEDURES

;;; To apply a primitive procedure, we ask the underlying Scheme system
;;; to perform the application.  (Of course, an implementation on a
;;; low-level machine would perform the application in some other way.)

(define (apply-primitive-procedure p args)
  (apply (text p) args))


;;; Now for the code that's based on the book!!!


;;; Section 4.1.1

;; Given an expression like (proc :a :b :c)+5
;; logo-eval calls eval-prefix for the part in parentheses, and then
;; handle-infix to check for and process the infix arithmetic.
;; Eval-prefix is comparable to Scheme's eval.

(define (logo-eval line-obj env)
  (handle-infix (eval-prefix line-obj env) line-obj env))

(define (eval-prefix line-obj env)
  (define (eval-helper paren-flag)
    (let ((token (ask line-obj 'next)))
      (cond ((self-evaluating? token) token)
            ((variable? token)
	     (lookup-variable-value (variable-name token) env))
            ((quoted? token) (text-of-quotation token))
            ((definition? token) (eval-definition line-obj env))
	    ((left-paren? token)
	     (let ((result (handle-infix (eval-helper #t)
				       	 line-obj
				       	 env)))
	       (let ((token (ask line-obj 'next)))
	       	 (if (right-paren? token)
		     result
		     (error "Too much inside parens")))))
	    ((right-paren? token)
	     (error "Unexpected ')'"))
            (else
	     (let ((proc (lookup-procedure token)))
	       (if (not proc) (error "I don't know how  to " token))
	       (logo-apply proc
			   (collect-n-args (arg-count proc)
					   line-obj
					   env
					   paren-flag)
			   env))) )))
  (eval-helper #f))

(define (logo-apply procedure arguments env) ;; A+B7: added env parameter
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure) ;; A+B7: completed
	 (eval-sequence (procedure-body procedure)
			(extend-environment (parameters procedure)
					    arguments
					    (statics procedure) ;; 9: added statics arg
					    env)
			(proc-step-flag procedure))) ;; A8: added step-flag
        (else
         (error "Unknown procedure type -- LOGO-APPLY " procedure))))

(define (collect-n-args n line-obj env paren-flag)
  (cond ((pair? n) (cons env (collect-n-args (car n) line-obj env paren-flag)))
	((= n 0) '())
	((and (< n 0) (not (ask line-obj 'empty?)))
	 (if paren-flag
	     (let ((token (ask line-obj 'next)))
	       (ask line-obj 'put-back token)
	       (if (right-paren? token)
		   '()
		   (let ((next (logo-eval line-obj env)))
		     (cons next
			   (collect-n-args (- n 1) line-obj env paren-flag)) )))
	     (collect-n-args (- n) line-obj env paren-flag)))
	(else      
      	 (let ((next (logo-eval line-obj env)))
           (cons next
	      	 (collect-n-args (- n 1) line-obj env paren-flag)) ))))

;;; Section 4.1.2 -- Representing expressions

;;; numbers

(define (self-evaluating? exp) (number? exp))

;;; quote

(define (quoted? exp)
  (or (list? exp)
      (eq? (string-ref (word->string (first exp)) 0) #\")))

(define (text-of-quotation exp)
  (if (list? exp)
      exp
      (bf exp)))

;;; parens

(define (left-paren? exp) (eq? exp left-paren-symbol))

(define (right-paren? exp) (eq? exp right-paren-symbol))

;;; definitions

(define (definition? exp)
  (eq? exp 'to))

;;; procedures

(define (lookup-procedure name)
  (assoc name the-procedures))

(define (primitive-procedure? p)
  (eq? (cadr p) 'primitive))

(define (compound-procedure? p)
  (eq? (cadr p) 'compound))

(define (arg-count proc)
  (caddr proc))

(define (text proc)
  (cadddr proc))

(define (parameters proc) (car (text proc)))

(define (procedure-body proc) (cdr (text proc)))

;;; Section 4.1.3

;;; Operations on environments

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals statics base-env) ;; 9: added statics arg
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) (cons statics base-env)) ;; 9: modified line
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied " vars vals)
          (error "Too few arguments supplied " vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((equal? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env local)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (if (or local
		     (null? (cdr env))) ;; 7: added condition so unbound variables are set in the global frame, as in BerkeleyLogo
		 (add-binding-to-frame! var val frame)
		 (define-variable! var val (cdr env) local) ))
            ((equal? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
