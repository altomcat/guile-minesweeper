(use-modules ((gui) #:prefix gui:)
	     (ice-9 format)
	     (minesweeper))

(define ROWS 4)
(define COLS 5)

(define my-game
  (gui:init-game ROWS COLS))

(define minefield-data
  (minefield-create ROWS COLS))

;; define the number of available flags
(set-minefield-max-flags! minefield-data 8)

(format #t "minesweeper? => ~a~%" (gui:minesweeper-game-width my-game))

(gui:intro-game my-game)
(gui:main-game my-game minefield-data)
(gui:end-game my-game)
