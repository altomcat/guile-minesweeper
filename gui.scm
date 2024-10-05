(define-module (gui)
  #:use-module (minesweeper)
  #:use-module (raylib)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (init-raylib
            init-game
            intro-game
            main-game
            end-game
            end-raylib
            minesweeper-game?
            minesweeper-game-width
            minesweeper-game-height
            minesweeper-game-assets
            minesweeper-game-clock
            minesweeper-game-state
            clock?
            clock-elapsed-time
            clock-ref-time))

;; define board margins
(define START-X 50)
(define START-Y 50)

(define MAX-SPRITES 12)

(define EMPTY-CELL 0)
(define FLAGGED-CELL 9)
(define HIDDEN-CELL 10)
(define MINE-CELL 11)

;; define a macro to hide boiler-plate code
(define-syntax-rule (define-accessor name sym)
  (define (name obj)
    (assoc-ref obj sym)))

(define-accessor sea-snd-from 'sea-snd)
(define-accessor sonar-snd-from 'sonar-snd)
(define-accessor congrats-snd-from 'congrats-snd)
(define-accessor all-sprites-from 'all-sprites)

(define-record-type <clock>
  (make-clock elapsed-time ref-time)
  clock?
  (elapsed-time clock-elapsed-time clock-elapsed-time-set!)
  (ref-time clock-ref-time clock-ref-time-set!))

(define-record-type <minesweeper-game>
  (make-minesweeper-game width-screen height-screen
                         assets sprite-recs
                         clock state)
  minesweeper-game?
  (width-screen minesweeper-game-width)
  (height-screen minesweeper-game-height)
  (assets minesweeper-game-assets)
  (sprite-recs minesweeper-game-sprite-recs)
  (clock minesweeper-game-clock)
  (state minesweeper-game-state set-minesweeper-game-state!))

(define (init-raylib rows cols)
  (let ((w (get-screen-width cols))
        (h (get-screen-height rows)))
    (InitWindow w h "Minesweeper")
    (InitAudioDevice)
    (SetTargetFPS 60)))

(define (end-raylib)
  (CloseAudioDevice)
  (CloseWindow))

(define (get-screen-width cols)
  (+ (* 2 START-X) (* 64 cols)))

(define (get-screen-height rows)
  (+ (* 2 START-Y) (* 64 rows)))

(define (init-game rows cols)
  (define sprites (LoadTexture "assets/minesweeper-sprites.png"))
  (define SPRITE-WIDTH (Texture-width sprites))
  (define SPRITE-HEIGHT (euclidean-quotient (Texture-height sprites) MAX-SPRITES))

  ;; calculate rectangle occupied with cells
  (define (make-cell-recs rows cols)
    (list->vector (map (lambda (i)
                         (let* ((x (+ (* 64 (modulo i cols))
                                      START-X))
                                (y (+ (* 64 (euclidean-quotient i cols))
                                      START-Y)))
                           (make-Rectangle x y 64 64)))
                       (iota (* rows cols)))))

  (let* ((w (get-screen-width cols))
         (h (get-screen-height rows))
         (sprite-recs (map (lambda (n)
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
         (recs (make-cell-recs rows cols))
         (t (make-clock 0 0)))
    (make-minesweeper-game w h assets recs t 'running)))

;; play intro music
(define (intro-game minesweeper-game)
  (let ((assets (minesweeper-game-assets minesweeper-game)))
    (PlaySound (sea-snd-from assets))))

;; main Raylib loop
(define (main-game minesweeper-game minefield-state)
  (set-clock-ref minesweeper-game)
  (let loop ((exitWindow (WindowShouldClose)))
    (unless exitWindow
      (manage-key-event minesweeper-game minefield-state)
      
      ;; lucky me for the first click or restart from shortkey !
      (when (minefield-state-restart? minefield-state)
        (let ((res (restart minesweeper-game minefield-state)))
          (loop #t)))

      (BeginDrawing)
      (ClearBackground RAYWHITE)

      (draw-board minesweeper-game minefield-state)

      (cond
       ((minefield-state-win? minefield-state)
        (let ((congrats-snd (congrats-snd-from
                             (minesweeper-game-assets minesweeper-game))))
          
          (DrawText "YOU WIN!!!" START-X 10 20 GREEN)
          (when (not (equal? (minesweeper-game-state minesweeper-game)
                             'stop))
            (set-minesweeper-game-state! minesweeper-game 'stop)
            (freeze-elapsed-time minesweeper-game)
            (PlaySound congrats-snd))))
       ((minefield-state-loose? minefield-state)
        (DrawText "YOU LOOSE!!!" START-X 10 20 RED)
        (when (not (equal? (minesweeper-game-state minesweeper-game)
                           'stop))
          (set-minesweeper-game-state! minesweeper-game 'stop)
          (freeze-elapsed-time minesweeper-game)))
       ((not (or (minefield-state-win? minefield-state)
                 (minefield-state-loose? minefield-state)))
        (cond
         ((equal? (minesweeper-game-state minesweeper-game) 'running)
          (DrawText "Playing Minesweeper..." START-X 10 20 LIGHTGRAY))
         ((equal? (minesweeper-game-state minesweeper-game) 'restart)
          (DrawText "Restarting..." START-X 10 20 LIGHTGRAY))
         ((equal? (minesweeper-game-state minesweeper-game) 'pause)
          (DrawText "Pause..." START-X 10 20 LIGHTGRAY)))))

      ;; update left flags and clock
      (let* ((width (minesweeper-game-width minesweeper-game))
             (clock (minesweeper-game-clock minesweeper-game))
             (t (+ (clock-elapsed-time clock)
                   (if (equal? (minesweeper-game-state minesweeper-game) 'running)
                       (- (GetTime) (clock-ref-time clock))
                       0)))
             (flags (minefield-state-max-flags minefield-state))
             (s (format #f " ~3,'0d  ~a" flags (elapsed-time->string t)))
             (x (- width (* 10 (string-length s)) START-X)))
        (DrawText s x 10 20 LIGHTGRAY))

      (EndDrawing)
      (loop (or (game-restart? minesweeper-game)
                (WindowShouldClose)))))
  (format #t "This is the end...~%")
  (let ((res (minesweeper-game-state minesweeper-game)))
    (format #t "Return ~a\n" res)
    (set-minesweeper-game-state! minesweeper-game 'running)
    (freeze-elapsed-time minesweeper-game)
    res))

(define (draw-board minesweeper-game minefield-state)
  (let* ((all-sprites (all-sprites-from
                       (minesweeper-game-assets minesweeper-game)))
         (sprite-texture (car all-sprites))
         (board-state (minefield-state-board minefield-state))
         (traveled (minefield-state-traveled-board minefield-state))
         (flagged (minefield-state-flagged-board minefield-state))
         (rows (minefield-state-rows minefield-state))
         (cols (minefield-state-cols minefield-state)))

    (for-each (lambda (i)
                (for-each (lambda (j)
                            (let* ((state (array-ref board-state i j))
                                   (id
                                    (cond
                                     ((flagged? flagged i j) FLAGGED-CELL)
                                     ((not (traveled? traveled i j)) HIDDEN-CELL)
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
     (game-over?
      (when (IsKeyPressed KEY_R)
	(set-minesweeper-game-state! minesweeper-game 'restart)
	(set-minefield-state-restart! minefield-state #t)))
     ((IsMouseButtonPressed MOUSE_BUTTON_LEFT)
      (let loop ((mouse-point (GetMousePosition)))
	(for-each (lambda (id)
		    (when (CheckCollisionPointRec mouse-point
						  (vector-ref recs id))
		      (format #t "left click on cell [~a]~%" id)
		      (set-traveled minefield-state id)))
		  (iota total-cells))))
     ((or (IsMouseButtonPressed MOUSE_BUTTON_RIGHT)
	  (IsKeyPressed KEY_F))
      (let ((mouse-point (GetMousePosition)))
	(for-each (lambda (id)
		    (when (CheckCollisionPointRec mouse-point
						  (vector-ref recs id))
		      (format #t "right click oncell [~a]~%" id)
		      (toggle-flagged minefield-state id)))
		  (iota total-cells))))
     ((IsKeyPressed KEY_P)
      (if (equal? (minesweeper-game-state minesweeper-game) 'running)
	  (begin
	    (freeze-elapsed-time minesweeper-game) ; run => pause
	    (set-minesweeper-game-state! minesweeper-game 'pause))
	  (begin
	    (set-clock-ref minesweeper-game) ; pause => run
	    (set-minesweeper-game-state! minesweeper-game 'running))))
     ((IsKeyPressed KEY_R)
      (set-minesweeper-game-state! minesweeper-game 'restart)))))

(define (end-game minesweeper-game)
  (let ((assets (minesweeper-game-assets minesweeper-game)))
    (UnloadTexture (car (all-sprites-from assets)))
    (UnloadSound (sea-snd-from assets))
    (UnloadSound (sonar-snd-from assets))
    (UnloadSound (congrats-snd-from assets))))

(define (restart game board)
  (let* ((rows (minefield-state-rows board))
         (cols (minefield-state-cols board))
         (max-mines (minefield-state-max-mines board))
         (max-flags (+ (minefield-state-max-flags board)
		       (traveled-count board)))
	 (t (make-clock 0 0))
         (new-board (minefield-random rows cols max-mines max-flags)))
    (reset-clock game)
    (main-game game new-board)))

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

(define (elapsed-time->string t)
  "Returns a clock formatted string from current elapsed time."
  (let* ((hour (inexact->exact
                (floor (euclidean/ t 3600))))
         (minute (inexact->exact
                  (floor (/ (euclidean-remainder t 3600) 60))))
         (second (inexact->exact
                  (floor (euclidean-remainder t 60)))))
    (format #f "Elapsed time : ~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(define (set-clock-ref game)
  "Set reference clock to current time."
  (let ((clock (minesweeper-game-clock game)))
    (clock-ref-time-set! clock (GetTime))))

(define (reset-clock game)
  "Reset clock."
  (let ((clock (minesweeper-game-clock game)))
    (clock-elapsed-time-set! clock 0)
    (clock-ref-time-set! clock 0)))

(define (freeze-elapsed-time game)
  "Update elapsed time based on reference clock."
  (let* ((clock (minesweeper-game-clock game))
         (elapsed-time (+ (clock-elapsed-time clock)
                          (- (GetTime) (clock-ref-time clock)))))
    (clock-elapsed-time-set! clock elapsed-time)))

(define (game-restart? game)
  (equal? (minesweeper-game-state game) 'restart))
