(use-modules ((gui) #:prefix gui:)
	     (ice-9 format)
	     (minesweeper))

;; easy 8x8 or 9x9 10 mines
(define COLS 8)
(define ROWS 8)
(define MINES 10)
(define FLAGS 10)

;; intermediate 16x16 40 mines
;; (define COLS 16)
;; (define ROWS 16)
;; (define MINES 40)
;; (define FLAGS 40)

;; expert 30x16 99 mines
;; (define COLS 30)
;; (define ROWS 16)
;; (define MINES 99)
;; (define FLAGS 99)


(gui:init-raylib ROWS COLS)

(define (run-game rows cols mines flags acc)
  (let* ((minefield (minefield-random rows cols mines flags))
	 (game (gui:init-game rows cols))
	 (intro (and (= acc 0)
		     (gui:intro-game game)))
	 (result (gui:main-game game minefield)))
    (display (gui:minesweeper-game-clock game))
    (gui:end-game game)
    (when (equal? result 'restart)
      (run-game rows cols mines flags (1+ acc)))))

(define start
  (and (run-game ROWS COLS MINES FLAGS 0)
       (gui:end-raylib)))
