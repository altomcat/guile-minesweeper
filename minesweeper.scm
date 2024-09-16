(define-module (minesweeper)
  #:use-module (srfi srfi-1)
  ;; #:use-module (srfi srfi-11) 		; let(*)-values
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 format)
  #:export (minefield-create
	    minefield-build
	    minefield-random
	    minefield-show
	    minefield-state-rows
	    minefield-state-cols
	    minefield-state-board
            minefield-state-traveled-board
	    minefield-state-flagged-board
	    minefiel-state-max-flags
	    set-minefield-max-flags!
	    minefield-state?
            minefield-state-win?
	    minefield-state-loose?
	    set-traveled
	    set-flagged
	    mine-discover
	    flagged?
	    traveled?
	    traveled-count
	    ))

(define-record-type <minefield-state>
  (make-minefield-state rows cols board
			traveled-board flagged-board
			max-flags win? loose?)
  minefield-state?
  (rows minefield-state-rows)
  (cols minefield-state-cols)
  (board minefield-state-board)
  (traveled-board minefield-state-traveled-board)
  (flagged-board minefield-state-flagged-board)
  (max-flags minefield-max-flags set-minefield-max-flags!)
  (win? minefield-state-win? set-minefield-state-win!)
  (loose? minefield-state-loose? set-minefield-state-loose!))

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

(define (minefield-random rows cols max-mines)
  (let ((minefield (make-typed-array 's8 0 rows cols))
	(traveled (make-array #f rows cols))
	(flagged (make-array #f rows cols))
	(seed (random-state-from-platform)))

    (if (>= max-mines (* rows cols))
	(display "Too much mines for the board...")
	(let loop ((i 0))
	  (if (< i max-mines)
	      (let ((row (random rows seed))
		    (col (random cols seed)))
		(if (= (array-ref minefield row col) 0)
		    (begin 
		      (array-set! minefield -1 row col)
		      (loop (1+ i)))
		    (loop i))))))
    ;; calculate the vicinity of mines
    (for-each (lambda (i)
		(for-each (lambda (j)
			    (calc-vicinity minefield i j))
			  (iota cols)))
	      (iota rows))

    ;; show the board in the console
    (minefield-show minefield)
    (make-minefield-state rows cols minefield traveled flagged 0 #f #f)))

(define (minefield-build board)
  (let* ((dimensions (array-dimensions board))
	 (rows (car dimensions))
	 (cols (cadr dimensions))
	 (minefield (make-typed-array 's8 0 rows cols))
	 (traveled (make-array #f rows cols))
	 (flagged (make-array #f rows cols)))
    (array-copy! board minefield)
    
    ;; calculate the vicinity of mines
    (for-each (lambda (i)
		(for-each (lambda (j)
			    (calc-vicinity minefield i j))
			  (iota cols)))
	      (iota rows))
    
    ;; show the board in the console
    (minefield-show minefield)
    (make-minefield-state rows cols
			  minefield traveled
			  flagged 0
			  #f #f)))

(define (minefield-create rows cols)  
  (define minefield (make-typed-array 's8 0 rows cols))
  (array-set! minefield -1 0 1)
  (array-set! minefield -1 0 3)
  (array-set! minefield -1 1 2)
  (array-set! minefield -1 2 2)

  (define traveled (make-array #f rows cols))
  (define flagged (make-array #f rows cols))

  ;; calculate the vicinity of mines
  (for-each (lambda (i)
	      (for-each (lambda (j)
			  (calc-vicinity minefield i j))
			(iota cols)))
	    (iota rows))
  ;; show the board in the console
  (minefield-show minefield)
  (make-minefield-state rows cols minefield traveled flagged 0 #f #f))

(define (mine? board row col)
  (and (array-in-bounds? board row col)
       (= (array-ref board row col) -1)))

(define (blank? board row col)
  (and (array-in-bounds? board row col)
       (= (array-ref board row col) 0)))

(define (free? board row col)
  (and (array-in-bounds? board row col)
       (>= (array-ref board row col) 0)))

(define (flagged? flagged-board row col)
  (and (array-in-bounds? flagged-board row col)
       (array-ref flagged-board row col)))

(define (traveled? traveled-board row col)
  (and (array-in-bounds? traveled-board row col)
       (array-ref traveled-board row col)))

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

(define (set-traveled minefield-state id)  
  (let* ((cols (minefield-state-cols minefield-state))
	 (traveled (minefield-state-traveled-board minefield-state))
	 (flagged (minefield-state-flagged-board minefield-state))
	 (pos (index->position cols id))
	 (row (car pos))
	 (col (cadr pos)))
    (when (flagged? flagged row col)
      (array-set! flagged #f row col))
    (array-set! traveled #t row col)
    (check-win-or-loose minefield-state row col)))

(define (mine-discover minefield-state id)
  (let* ((cols (minefield-state-cols minefield-state))
	 (pos (index->position cols id))
	 (row (car pos))
	 (col (cadr pos)))
    (mine-discover-helper minefield-state row col 100)
    (check-win-or-loose minefield-state row col)))

(define (mine-discover-helper minefield-state row col count)
  (let ((rel-pos '((-1 -1) (0 -1) (1 -1) (-1 0) (1 0) (-1 1) (0 1) (1 1)))
	(board (minefield-state-board minefield-state))
        (traveled (minefield-state-traveled-board minefield-state))
	(flagged (minefield-state-flagged-board minefield-state)))
    (format #t "Start discovering row=~a col=~a~%" row col)
    (when  (and (blank? board row col)
		(>= count 0))		; the counter is for debugging
      (array-set! traveled #t row col)
      (array-set! flagged #f row col)
      (for-each (lambda (v)
		  (let ((ri (+ row (car v)))
			(rj (+ col (cadr v))))
		    (when (and (free? board ri rj)
			       (not (traveled? traveled ri rj)))
		      (format #t "count=~a v=~a ri=~a rj=~a~%" count v ri rj)
		      (format #t "blank?=~a traveled?=~a~%" (blank? board ri rj)
			      (traveled? traveled ri rj))
		      (array-set! traveled #t ri rj)
		      (array-set! flagged #f ri rj)
		      (mine-discover-helper minefield-state ri rj (1- count))))) 
		rel-pos))))

(define (check-win-or-loose minefield-state row col)
  (let ((board (minefield-state-board minefield-state)))
    (cond
     ((and (mine? board row col)
	   (= (traveled-count minefield-state) 1))
      (display "BAD LUCK, RESTART GAME!\n")
      'game-restart)
     ((mine? board row col)
      (display "YOU LOOSE!\n")
      (set-minefield-state-loose! minefield-state #t)
      'game-loose)
     ((win-game? minefield-state)
      (display "YOU WIN!\n")
      (set-minefield-state-win! minefield-state #t)
      'game-win))))

(define (set-flagged minefield-state id)
  (let* ((cols (minefield-state-cols minefield-state))
	 (traveled (minefield-state-traveled-board minefield-state))
	 (flagged (minefield-state-flagged-board minefield-state))
	 (pos (index->position cols id))
	 (row (car pos))
	 (col (cadr pos)))
    (unless (traveled? traveled row col)
      ;;      (display "not traveled\n")
      (if (flagged? flagged row col)
	  (array-set! flagged #f row col)
	  (array-set! flagged #t row col)))))

(define (index->position cols id)
  (let ((row (euclidean-quotient id cols))
	(col (euclidean-remainder id cols)))
    (list row col)))

(define (win-game? minefield-state)
  (let ((t (traveled-count minefield-state))
	(m (mine-count minefield-state))
	(total-cells (* (minefield-state-rows minefield-state)
			(minefield-state-cols minefield-state))))
    (equal? (+ m t) total-cells)))

(define (traveled-count minefield-state)
  (let ((board (minefield-state-traveled-board minefield-state))
	(rows (minefield-state-rows minefield-state))
	(cols (minefield-state-cols minefield-state))
	(count 0))
    
    (for-each (lambda (i)
		(for-each (lambda (j)
			    (when (equal? (array-ref board i j) #t)
			      (set! count (1+ count))))
			  (iota cols)))
	      (iota rows))
    count)
  )

(define (mine-count minefield-state)
  (let ((board (minefield-state-board minefield-state))
	(rows (minefield-state-rows minefield-state))
	(cols (minefield-state-cols minefield-state))
	(count 0))
    
    (for-each (lambda (i)
		(for-each (lambda (j)
			    (when (equal? (array-ref board i j) -1)
			      (set! count (1+ count))))
			  (iota cols)))
	      (iota rows))
    count)
  )


;; Unit tests
;;

(test-begin "tests")

(define board-state (minefield-create 4 5))
(define m (minefield-state-board board-state))
(define t (minefield-state-traveled-board board-state))
(define cols (minefield-state-cols board-state))

(set-traveled board-state 0)
(set-traveled board-state 10)
(set-traveled board-state 19)
(mine-count board-state)

(test-assert (equal? (index->position cols 0) '(0 0)))
(test-assert (equal? (index->position cols 10) '(2 0)))
(test-assert (equal? (index->position cols 19) '(3 4)))

(test-assert (equal? (array-ref t 0 0) #t))
(test-assert (equal? (array-ref t 2 0) #t))
(test-assert (equal? (array-ref t 3 4) #t))

(test-assert (equal? (traveled-count board-state) 3))
(test-assert (equal? (mine-count board-state) 4))
(test-end "tests")

