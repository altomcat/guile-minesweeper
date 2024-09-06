(define-module (minesweeper)
  #:use-module (srfi srfi-1)
  ;; #:use-module (srfi srfi-11) 		; let(*)-values
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 format)
  #:export (minefield-create
	    minefield-show
	    mine?
	    minefield-state-rows
	    minefield-state-cols
	    minefield-state-board
	    minefield-state-traveled-board
	    minefield-state?
	    ))

(define-record-type <minefield-state>
  (make-minefield-state rows cols board traveled-board)
  minefield-state?
  (rows minefield-state-rows)
  (cols minefield-state-cols)
  (board minefield-state-board)
  (traveled-board minefield-state-traveled-board))

(define (minefield-create rows cols)  
  (define minefield (make-typed-array 's8 0 rows cols))
  (array-set! minefield -1 0 1)
  (array-set! minefield -1 0 3)
  (array-set! minefield -1 1 2)
  (array-set! minefield -1 2 2)

  (define traveled-field (make-array 0 rows cols))
  
  (define (calc-vicinity minefield i j)
    (let ((cur (array-ref minefield i j))
          (ij '((-1 -1) (0 -1) (1 -1) (-1 0) (1 0) (-1 1) (0 1) (1 1))))
      (unless (mine? minefield i j)
        (let ((res (fold + 0
                         (map (lambda (v)
                                (let ((ri (+ i (car v)))
                                      (rj (+ j (cadr v))))
                                  (or (and (mine? minefield ri rj)
                                           1)
                                      0)))
                              ij))))
          (when (> res 0)
            (array-set! minefield res i j))))))
  
  ;; calculate the vicinity of mines
  (for-each (lambda (i)
	      (for-each (lambda (j)
			  (calc-vicinity minefield i j))
			(iota cols)))
	    (iota rows))
  ;; show the board in the console
  (minefield-show minefield)
  (make-minefield-state rows cols minefield traveled-field))

(define (mine? minefield i j)
  (equal? -1 (and (array-in-bounds? minefield i j)
		  (array-ref minefield i j))))

(define (free? minefield i j)
  (and (not (mine? minefield i j))
       (>= (array-ref minefield i j ) 0)))

(define (mine-discover minefield traveled-field i j)
  (mine? i j))

(define (minefield-show minefield)
  (define (toString value)
    (cond
     ((< value 0) "*")
     ((= value 0) ".")
     (else value)))
  (let* ((dimensions (array-dimensions minefield))
	 (rows (car dimensions))
	 (cols (cadr dimensions)))
    (for-each (lambda (i)
		(for-each (lambda (j)
			    (format #t "~2a " (toString (array-ref minefield i j))))
			  (iota cols))
		(newline))
	      (iota rows))))

(define (index->position minefield id)
  (let* ((dimensions (array-dimensions minefield))
	 (cols (cadr dimensions))
	 (j  (euclidean-quotient id cols))
	 (i (modulo id cols)))
    (list i j)))

(define (index->position2 cols id)
  (let ((j  (euclidean-quotient id cols))
	(i (modulo id cols)))
    (list i j)))

;; Unit tests
;;
(define m (minefield-state-board (minefield-create 4 5)))
(test-begin "tests")
(test-assert (equal? (index->position m 0) '(0 0)))
(test-assert (equal? (index->position m 12) '(2 2)))
(test-assert (equal? (index->position m 19) '(4 3)))
(test-end "tests")

;; (newline)
;; (format #t "minefield => ~a" board)
;; (newline)
;; (map (lambda (coor)
;;        (let ((i (car coor))
;; 	     (j (cadr coor)))
;; 	 (format #t "free cell? ~a i=~a j=~a~%" (free? board i j) i j)))
;;      (list '(0 0) '(0 1) '(0 2) '(2 0)))
;; ;;(format #t "free cell? ~a i=~a j=~a" (free? board 0 0) 0 0)
;; (newline)


;; (define minefield
;;   #2s8((0 -1  0 -1  0)
;; 	 (0  0 -1  0  0)
;; 	 (0  0 -1  0  0)
;; 	 (0  0  0  0  0)))
