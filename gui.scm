(define-module (gui)
  #:use-module (raylib)
  #:use-module (srfi srfi-9)
  #:export (init-game
	    intro-game
	    main-game
	    end-game))

(define-record-type <minesweeper-game>
  (make-minesweeper-game width-screen height-screen assets)
  minesweeper-game?
  (width-screen minesweeper-game-width)
  (height-screen minesweeper-game-height)
  (assets minesweeper-game-assets))

(define (init-game width-screen height-screen)
  (InitWindow width-screen height-screen "Minesweeper")
  (InitAudioDevice)

  (SetTargetFPS 60)
  
  (let ((assets
         (list (cons 'sea-snd  (LoadSound "assets/mer.wav"))
	       (cons 'sonar-snd  (LoadSound "assets/mine.wav"))
	       (cons 'congrats-snd  (LoadSound "assets/congrats.wav"))
	       (cons 'sprites  (LoadTexture "assets/minesweeper-sprites.png")))))
    (make-minesweeper-game width-screen height-screen assets)))

;; (define MAX-FRAMES 4)
;; (define sprites (LoadTexture "assets/minesweeper-sprites.png"))
;; (define SPRITE-WIDTH (Texture-width sprites))
;; (define SPRITE-HEIGHT (euclidean-quotient (Texture-height sprites) MAX-FRAMES))

(define mines-field (make-array #f 10 10))
(define mousePoint (make-Vector2 0 0))

(define (intro-game minesweeper-game)
  (let ((assets (minesweeper-game-assets minesweeper-game)))
    (PlaySound (assoc-ref assets 'sea-snd))))

(define (main-game minesweeper-game)
  (let loop () 
    (unless (WindowShouldClose)
      (manage-key-event (minesweeper-game-assets minesweeper-game))
      (BeginDrawing)
      (ClearBackground RAYWHITE)
      (EndDrawing)
      (loop))))

(define (manage-key-event my-assets)
  (cond
   ((IsMouseButtonPressed MOUSE_BUTTON_LEFT)
    (PlaySound (assoc-ref my-assets 'sonar-snd)))
   ((IsKeyPressed KEY_ENTER)
    (PlaySound (assoc-ref my-assets 'congrats-snd)))))

(define (end-game minesweeper-game)
  (let ((assets (minesweeper-game-assets minesweeper-game)))
    (UnloadTexture (assoc-ref assets 'sprites))
    (UnloadSound (assoc-ref assets 'sea-snd))
    (UnloadSound (assoc-ref assets 'sonar-snd))
    (CloseAudioDevice)
    (CloseWindow)))
