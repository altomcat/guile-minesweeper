(use-modules ((gui) #:prefix gui:)
	     (ice-9 format)
	     (minesweeper))

(define ROWS 10)
(define COLS 10)
(define MINES 10)
(define FLAGS 3)

(gui:init-raylib ROWS COLS)

(define (run-game rows cols mines flags acc)
  (let* ((minefield (minefield-random rows cols mines flags))
	 (game (gui:init-game rows cols))
	 (intro (and (= acc 0)
		     (gui:intro-game game)))
	 (result (begin
		   ;; define the number of available flags and mines
                   (set-minefield-max-flags! minefield flags)
		   (set-minefield-max-mines! minefield mines)
		   (gui:main-game game minefield))))
    (display (gui:minesweeper-game-clock game))
    (gui:end-game game)
    (when (equal? result 'restart)
      (run-game rows cols mines flags (1+ acc)))))

(define start
  (and (run-game ROWS COLS MINES FLAGS 0)
       (gui:end-raylib)))
