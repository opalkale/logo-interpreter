;;; logo.scm         part of programming project #4

;; PART II
;; Kevin Chen - CS61AS-an
;; Opal Kale - CS61AS-ca

;;; Problem A1   make-line-obj

(define (make-line-obj text)
  (instantiate line-object text))

(define-class (line-object token-list)
  (method (empty?)
    (null? token-list))
  (method (next)
    (let ((next-token (car token-list)))
      (set! token-list (cdr token-list))
       next-token))
  (method (put-back token)
    (set! token-list (cons token token-list)))
  )

;;; Problem A2   logo-type

(define (logo-type val)
  (define (logo-type-helper val)
    (if (pair? (car val))
	(begin
	  (display #\[)
	  (logo-type-helper (car val))
	  (display #\]))
	(display (car val)))
    (if (null? (cdr val))
	'=NO-VALUE=
	(begin
	  (display " ")
	  (logo-type-helper (cdr val))) ))
  (if (not (pair? val))
      (begin
	(display val)
	'=NO-VALUE=)
      (logo-type-helper val) ))	  

(define (logo-print val)   
  (logo-type val)  
  (newline) 
  '=no-value=) 

(define (logo-show val)   
  (logo-print (list val)))   

;;; Problem 4   variables   (logo-meta.scm is also affected)

(define (make env var val)
  (define-variable! var val env #f) ;; added local var
  '=NO-VALUE=)

;;; Here are the primitives RUN, IF, and IFELSE.  Problem B2 provides
;;; support for these, but you don't have to modify them.   

(define (run env exp)
  (eval-line (make-line-obj exp) env))

(define (logo-if env t/f exp) 
  (cond ((eq? t/f 'true) (eval-line (make-line-obj exp) env))
        ((eq? t/f 'false) '=no-value=)
        (else (error "Input to IF not true or false " t/f))))  

(define (ifelse env t/f exp1 exp2)  
  (cond ((eq? t/f 'true) (eval-line (make-line-obj exp1) env))
        ((eq? t/f 'false) (eval-line (make-line-obj exp2) env))   
        (else (error "Input to IFELSE not true or false " t/f))))  


;;; Problem B2   logo-pred

(define (logo-pred pred)
  (lambda args
    (if (apply pred args)
	'TRUE
	'FALSE))) 


;;; Here is an example of a Scheme predicate that will be turned into  
;;; a Logo predicate by logo-pred:  

(define (equalp a b)
  (if (and (number? a) (number? b))  
      (= a b)   
      (equal? a b)))   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;  Stuff below here is needed for the interpreter to work but you  ;;;  
;;;  don't have to modify anything or understand how they work.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;;; The Logo reader

(define left-paren-symbol (string->symbol (make-string 1 #\( )))
(define right-paren-symbol (string->symbol (make-string 1 #\) )))
(define quote-symbol (string->symbol (make-string 1 #\" )))

(define (logo-read)  
  (define lookahead #f)   
  (define (logo-read-help depth)   
    (define (get-char)  
      (if lookahead  
          (let ((char lookahead))   
            (set! lookahead #f)   
            char) 
          (let ((char (read-char)))   
            (if (eq? char #\\)
                (list (read-char))  
                char)))) 
    (define (quoted char)   
      (if (pair? char)   
          char 
          (list char)))  
    (define (get-symbol char)   
      (define (iter sofar char)
        (cond ((pair? char) (iter (cons (car char) sofar) (get-char))) 
              ((memq char  
                     '(#\space #\newline #\+ #\- #\* #\/  
                               #\= #\< #\> #\( #\) #\[ #\] ))
               (set! lookahead char)   
               sofar) 
              (else (iter (cons char sofar) (get-char))) ))   
      (string->word (list->string (reverse (iter '() char)))) )
    (define (get-token space-flag)   
      (let ((char (get-char)))   
              (cond ((eq? char #\space) (get-token #t))  
              ((memq char '(#\+ #\* #\/ #\= #\< #\> #\( #\) ))   
               (string->symbol (make-string 1 char)))
              ((eq? char #\-)   
               (if space-flag  
                   (let ((char (get-char)))   
                     (let ((result (if (eq? char #\space)  
                                       '- 
                                       '=unary-minus=))) 
                       (set! lookahead char)   
                       result)) 
                   '-)) 
              ((eq? char #\[) (logo-read-help (+ depth 1)))  
              ((pair? char) (get-symbol char))
              ((eq? char #\")   
               (let ((char (get-char)))   
                 (if (memq char '(#\[ #\] #\newline))  
                     (begin (set! lookahead char) quote-symbol)
                     (string->symbol (word quote-symbol
					   (get-symbol (quoted char)))))))
	      (else (get-symbol char)) )))
    (define (after-space)
      (let ((char (get-char)))
	(if (eq? char #\space)
	    (after-space)
	    char)))
    (let ((char (get-char)))   
      (cond ((eq? char #\newline)
             (if (> depth 0) (set! lookahead char))   
             '()) 
	    ((eq? char #\space)
	     (let ((char (after-space)))
	       (cond ((eq? char #\newline)
		      (begin (if (> depth 0) (set! lookahead char))
			     '()))
		     ((eq? char #\])
		      (if (> depth 0) '() (error "Unexpected ]")))
		     (else (set! lookahead char)
			   (let ((token (get-token #t)))
			     (cons token (logo-read-help depth)))))))
            ((eq? char #\])   
             (if (> depth 0) '() (error "Unexpected ]")))
            ((eof-object? char) char)   
            (else (set! lookahead char)
                  (let ((token (get-token #f)))
                    (cons token (logo-read-help depth)) ))))) 
  (logo-read-help 0))  


;;; Assorted stuff   

(define (make-logo-arith op)   
  (lambda args (apply op (map maybe-num args))))   

(define (maybe-num val)
  (if (word? val)
      (string->word (word->string val))
      val))

(define tty-port (current-input-port))   

(define (prompt string)   
  (if (eq? (current-input-port) tty-port)
      (begin (display string) (flush))))  

(define (meta-load fn)   
  (define (loader)  
    (let ((exp (logo-read)))   
      (if (eof-object? exp)   
          '() 
          (begin (eval-line (make-line-obj exp)
			    the-global-environment) 
		 (loader))))) 
  (with-input-from-file (symbol->string fn) loader)
  '=no-value=) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRANSCRIPTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If handle-infix was used instead of eval-prefix, then it would produce errors in the order of evaluation. For example, 3 + 4 * 5 is to be computed as (3 + 4) * 5 if eval-prefix were used, as only the "4" is taken from "4 * 5. However, if handle-infix was used instead, then 3 + 4 * 5 would be computed as 3 + (4 * 5) as the entire "4 * 5" would be evaluated.

#|

with eval-prefix:
? 3 + 4 * 5
35

with handle-infix (through logo-eval)
? 3 + 4 * 5
23

|#

;; 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

? make "x 3
? to scope :x
> helper 5
> end
? to helper :y
> print (sentence :x :y)
> end
? scope 4
4 5

|#

;; if lexical scoping were used instead, then the answer would be 3 5, as when "helper" is invoked with the parameters y=5, it would refer to the global frame (where it was invoked) and discover that x=3. Here, "helper" recieves it definition of x from "scope"'s environment.

;; A8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

? to garply
-> print "hello
-> print "goodbye
-> end
garply defined
? garply
hello
goodbye
? step "garply
? garply
(print "hello) >>>
hello
(print "goodbye) >>>
goodbye
? unstep "garply
? garply
hello
goodbye

|#

;; B8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

? test "true
? ift [print 3]
3
? test "false
? ift [print 3]
? iff [print 3]
3
? to test1
-> iff [print 3]
-> end
test1 defined
? test1
3
? to test2
-> test "true
-> ift [print 3]
-> end
test2 defined
? test2
3
? ift [print 3]

|#

;; 9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

? to count :increase static :counter 2+3
-> make "counter :counter + :increase
-> print :counter
-> end
count defined
? count 20
25
? count 1
26
? print :counter
*** Error:
    Unbound variable counter

|#
