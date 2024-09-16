(use-modules ((gui) #:prefix gui:)
	     (ice-9 format)
	     (minesweeper))

;; (define minefield-test1
;;   #2s8(( 0 -1  0 -1  0)
;;        ( 0  0 -1  0  0)
;;        ( 0  0 -1  0  0)
;;        ( 0  0  0  0  0)))

;; (define minefield-test2
;;   #2s8(( 0  0  0  0  0  0 -1  0  0  0)
;;        ( 0  0  0  0  0  0  0  0  0  0)
;;        ( 0  0  0 -1  0  0  0  0  0  0)
;;        ( 0  0  0  0  0  0  0  0  0  0)
;;        ( 0  0  0  0  0  0 -1 -1 -1  0)
;;        ( 0  0  0  0  0  0  0  0  0  0)
;;        (-1  0  0  0 -1  0  0  0  0 -1)
;;        ( 0  0  0  0 -1  0  0  0  0  0)
;;        ( 0  0  0  0  0  0  0  0 -1  0)
;;        ( 0  0  0  0  0  0  0  0  0 -1)))

;; (define minefield-test minefield-test2)

(define ROWS 10)
(define COLS 10)

(define my-game
  ;; (let* ((dimensions (array-dimensions minefield-test))
  ;; 	 (rows (car dimensions))
  ;; 	 (cols (cadr dimensions)))
  ;;   (gui:init-game rows cols))
  (gui:init-game ROWS COLS))

(define minefield-data
  ;; (minefield-build minefield-test)
  (minefield-random ROWS COLS 10))

;; define the number of available flags
(set-minefield-max-flags! minefield-data 3)

;; (format #t "minesweeper? => ~a~%"
;; 	(gui:minesweeper-game-width my-game))

(gui:intro-game my-game)
(gui:main-game my-game minefield-data)
(gui:end-game my-game)
