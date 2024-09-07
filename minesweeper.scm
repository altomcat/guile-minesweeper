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
            minefield-state-win?
	    minefield-state-loose?
	    set-traveled-field
	    ))

(define-record-type <minefield-state>
  (make-minefield-state rows cols board traveled-board win? loose?)
  minefield-state?
  (rows minefield-state-rows)
  (cols minefield-state-cols)
  (board minefield-state-board)
  (traveled-board minefield-state-traveled-board)
  (win? minefield-state-win? set-minefield-state-win!)
  (loose? minefield-state-loose? set-minefield-state-loose!))

(define (minefield-create rows cols)  
  (define minefield (make-typed-array 's8 0 rows cols))
  (array-set! minefield -1 0 1)
  (array-set! minefield -1 0 3)
  (array-set! minefield -1 1 2)
  (array-set! minefield -1 2 2)

  (define traveled-field (make-array #f rows cols))
  
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
  (make-minefield-state rows cols minefield traveled-field #f #f))

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

(define (set-traveled-field minefield-state id)  
  (let* ((cols (minefield-state-cols minefield-state))
	 (board (minefield-state-board minefield-state))
	 (traveled-board (minefield-state-traveled-board minefield-state))
	 (pos (index->position cols id))
	 (row (car pos))
	 (col (cadr pos)))
    (array-set! traveled-board #t row col)
    (when (= (array-ref board row col) -1)
      (set-minefield-state-loose! minefield-state #t))))

(define (index->position cols id)
  (let ((row (euclidean-quotient id cols))
	(col (modulo id cols)))
    (list row col)))

;; Unit tests
;;
(define board-state (minefield-create 4 5))
(define m (minefield-state-board board-state))
(define t (minefield-state-traveled-board board-state))
(define cols (minefield-state-cols board-state))

(set-traveled-field board-state 0)
(set-traveled-field board-state 10)
(set-traveled-field board-state 19)

(test-begin "tests")

(test-assert (equal? (index->position cols 0) '(0 0)))
(test-assert (equal? (index->position cols 10) '(2 0)))
(test-assert (equal? (index->position cols 19) '(3 4)))

(test-assert (equal? (array-ref t 0 0) #t))
(test-assert (equal? (array-ref t 2 0) #t))
(test-assert (equal? (array-ref t 3 4) #t))

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
