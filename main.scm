(use-modules ((gui) #:prefix gui:)
	     (ice-9 format)
	     (minesweeper))

(define ROWS 10)
(define COLS 10)

(gui:init-raylib ROWS COLS)

(define (run game acc)
  (let* ((minefield (minefield-random ROWS COLS 10))
	 (intro (and (= acc 0)
		     (gui:intro-game game)))
	 (result (begin
		   ;; define the number of available flags
		   (set-minefield-max-flags! minefield 3)
		   (gui:main-game game minefield))))
    (display (gui:minesweeper-game-clock game))
    (gui:end-game game)
    (when (equal? result 'restart)
      (run (gui:init-game ROWS COLS) (1+ acc)))))

(define start
  (and (run (gui:init-game ROWS COLS) 0)
       (gui:end-raylib)))
