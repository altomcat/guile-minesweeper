(use-modules ((gui)
	      #:prefix gui:))

(define screenWidth 800)
(define screenHeight 450)

(define my-game
  (gui:init-game screenWidth screenHeight))

(gui:intro-game my-game)
(gui:main-game my-game)
(gui:end-game my-game)
