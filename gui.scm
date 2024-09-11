(define-module (gui)
  #:use-module (minesweeper)
  #:use-module (raylib)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (init-game
	    intro-game
	    main-game
	    end-game
	    minesweeper-game?
	    minesweeper-game-width
	    minesweeper-game-height
	    minesweeper-game-assets))

;; define margins
(define START-X 10)
(define START-Y 10)

(define EMPTY-CELL 0)
(define FLAG-CELL 5)
(define HIDDEN-CELL 6)
(define MINE-CELL 7)

;; define a macro to hide boiler-plate code
(define-syntax-rule (define-accessor name sym)
  (define (name obj)
    (assoc-ref obj sym)))

(define-accessor sea-snd-from 'sea-snd)
(define-accessor sonar-snd-from 'sonar-snd)
(define-accessor congrats-snd-from 'congrats-snd)
(define-accessor all-sprites-from 'all-sprites)

(define-record-type <minesweeper-game>
  (make-minesweeper-game width-screen height-screen assets sprite-recs)
  minesweeper-game?
  (width-screen minesweeper-game-width)
  (height-screen minesweeper-game-height)
  (assets minesweeper-game-assets)
  (sprite-recs minesweeper-game-sprite-recs))

(define (init-game rows cols)  
  (define SCREEN-WIDTH (+ (* 2 10) (* 64 cols)))
  (define SCREEN-HEIGHT (+ (* 2 50) (* 64 rows)))
  
  (InitWindow SCREEN-WIDTH SCREEN-HEIGHT "Minesweeper")
  (InitAudioDevice)

  (SetTargetFPS 60)

  (define MAX-SPRITES 8)

  (define sprites (LoadTexture "assets/minesweeper-sprites.png"))
  (define SPRITE-WIDTH (Texture-width sprites))
  (define SPRITE-HEIGHT (euclidean-quotient (Texture-height sprites) MAX-SPRITES))

  ;; calculate rectangle occupied with cells
  (define (make-cell-recs rows cols)
    (list->vector (map (lambda (i)
			 (let* ((x (+ (* 64.0 (modulo i cols))))
				(y (+ (* 64.0 (euclidean-quotient i cols)))))
			   ;; (format #t "x=~a y=~a~%" x y)
			   (make-Rectangle x y 64 64)))
		       (iota (* rows cols)))))
  
  (let* ((sprite-recs (map (lambda (n)
			     (make-Rectangle 0
					     (* n SPRITE-HEIGHT)
					     SPRITE-WIDTH
					     SPRITE-HEIGHT))
			   (iota MAX-SPRITES)))
	 (assets
          (list (cons 'sea-snd (LoadSound "assets/mer.wav"))
		(cons 'sonar-snd (LoadSound "assets/mine.wav"))
		(cons 'congrats-snd (LoadSound "assets/congrats.wav"))
		(cons 'all-sprites (list sprites sprite-recs))))
	 (recs (make-cell-recs rows cols)))
    (display recs)
    (make-minesweeper-game SCREEN-WIDTH SCREEN-HEIGHT assets recs)))

;; play intro music
(define (intro-game minesweeper-game)
  (let ((assets (minesweeper-game-assets minesweeper-game)))
    (PlaySound (sea-snd-from assets))
    ))

;; main Raylib loop
(define (main-game minesweeper-game minefield-state)
  (let loop ((exitWindow #f)) 
    (unless exitWindow
      (manage-key-event minesweeper-game minefield-state)

      (BeginDrawing)
      (ClearBackground RAYWHITE)

      (if (minefield-state-win? minefield-state)
	  (let ((congrats-snd (congrats-snd-from
			       (minesweeper-game-assets minesweeper-game))))
            (DrawText "YOU WIN!!!" 10 300 20 GREEN)
	    (unless (IsSoundPlaying congrats-snd)
	      (PlaySound congrats-snd))))
      
      (if (minefield-state-loose? minefield-state)	
	  (DrawText "YOU LOOSE!!!" 10 300 20 LIGHTGRAY))
      
      (draw-board minesweeper-game minefield-state)
      
      (unless (or (minefield-state-win? minefield-state)
		  (minefield-state-loose? minefield-state))
	(DrawText "Playing Minesweeper ..." 10 300 20 LIGHTGRAY))

      (EndDrawing)
      (loop (WindowShouldClose)))))

(define (draw-board minesweeper-game minefield-state)
  (let* ((all-sprites (all-sprites-from
		       (minesweeper-game-assets minesweeper-game)))
	 (sprite-texture (car all-sprites))
	 (board-state (minefield-state-board minefield-state))
	 (traveled-board (minefield-state-traveled-board minefield-state))
         (rows (minefield-state-rows minefield-state))
	 (cols (minefield-state-cols minefield-state)))
    
    (for-each (lambda (i)
		(for-each (lambda (j)
			    (let* ((state (array-ref board-state i j))
				   (id
				    (cond
				     ((not (array-ref traveled-board i j)) HIDDEN-CELL)
				     ((= state -1) MINE-CELL)
				     (else state))))
			      (DrawTextureRec
			       sprite-texture
			       (list-ref (cadr all-sprites) id)
			       (make-Vector2 (+ (* j 64) START-X)
					     (+ (* i 64) START-Y))
			       WHITE)))
			  (iota cols)))
	      (iota rows))))


(define (manage-key-event minesweeper-game minefield-state)
  (let ((assets (minesweeper-game-assets minesweeper-game))
	(game (minesweeper-game-sprite-recs minesweeper-game))
	(recs (minesweeper-game-sprite-recs minesweeper-game))
	(total-cells (* (minefield-state-rows minefield-state)
			(minefield-state-cols minefield-state)))
	(game-over? (or (minefield-state-loose? minefield-state)
			(minefield-state-win? minefield-state))))
    (cond
     (game-over?)
     ((IsMouseButtonPressed MOUSE_BUTTON_LEFT)
      (let ((mousePoint (GetMousePosition)))
	(for-each (lambda (id)
		    (when (CheckCollisionPointRec mousePoint
						  (vector-ref recs id))
		      (format #t "hit cell [~a]~%" id)
		      (set-traveled-field minefield-state id)))
		  (iota total-cells))))
     ((IsMouseButtonPressed MOUSE_BUTTON_RIGHT)
      (PlaySound (sonar-snd-from  assets)))
     ((IsKeyPressed KEY_ENTER)
      (PlaySound (congrats-snd-from assets))))))

(define (end-game minesweeper-game)
  (let ((assets (minesweeper-game-assets minesweeper-game)))
    (UnloadTexture (car (all-sprites-from assets)))
    (UnloadSound (sea-snd-from assets))
    (UnloadSound (sonar-snd-from assets))
    (UnloadSound (congrats-snd-from assets))
    (CloseAudioDevice)
    (CloseWindow)))

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
