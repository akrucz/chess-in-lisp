;;;; Austin Kruczek
;;;; Chess in LISP
;;;; Version 1.0

;;; Core game functions

(defun checkp (id kingtile board)
"Determines if a player's king is in check."
   (let ((bool nil) (nowpiece nil))
      (loop for r from 1 to 8
         do (loop for c from 1 to 8
               do (setf nowpiece (find-piece r c board))
               if (if (< id 0) (> nowpiece 0) (< nowpiece 0))
                  if (member kingtile (get-legal-moves (list r c) board) :test #'equal)
                     do (setf bool t) (return))
         if bool do (return bool))))

;; TODO: this and stalematep can be combined so it doesn't have to calc filtered-a-moves twice
(defun checkmatep (id kingtile board)
"If in check and has moves which only result in check, then checkmate (t)."
   (and (checkp id kingtile board) (null (filtered-a-moves id kingtile board))))

(defun list-piece-ids (board)
"Gets a list of the piece ids left on the board. Used for draw conditions."
   (let ((lis ()))
      (loop for row in board
         do (loop for piece in row
               if (not (= 0 piece))
                  do (push piece lis))
         finally (return lis))))

(defun num-left (board)
"Gets the number of pieces remaining on the board."
   (length (list-piece-ids board)))

(defun drawp (draw-turn-count board)
"Indicates whether the game is a draw."
   (let ((piece-lis (list-piece-ids board)) (len nil))
   (setf len (length piece-lis))
   (cond ((>= draw-turn-count 50) (print "50 turn no-take limit reached."))
         ((= 2 len))
         ((> 5 len)
            (cond ((= 2 len) (print "Insufficient material to checkmate."))
                  ((and (= 4 len) (member -4 piece-lis :test #'=) (member 4 piece-lis :test #'=)))
                  ((and (= 3 len) (some #'(lambda (x) (or (= (abs x) 3) (= (abs x) 4))) piece-lis))
                      (print "Insufficient material to checkmate."))
                  (t nil)))
         (t nil))))

(defun stalematep (id kingtile board)
"If not in check and has moves which only result in check, then stalemate (t)."
   (and (not (checkp id kingtile board)) (null (filtered-a-moves id kingtile board))))

(defun player-turn (player board)
"Handles the logic required to process a player's turn. Returns the move if valid, else nil."
   (let ((attempted-move nil) (adjusted-move nil))
      (print (str+ player " 's turn:")) (print "")
      (setf attempted-move (string (read)))
      (setf adjusted-move (make-move-readable attempted-move))
      (if adjusted-move (process-move adjusted-move board) nil)))

(defun wrong-move-prints ()
"Prints a response to the console if a given move is invalid."
   (print "Invalid move. Try again.") (print ""))

;; If automatic-game, then whatever is set to adversary-sym
;; gets ranked-moves and the other player gets random ones.
(defun play-chess (&key (p1 'human)
                        (p2 'adversary)
                        (adversary-sym 'adversary)
                        (adversary-depth 0)
                        (automatic-game nil)
                        (p1lis '(♙ ♘ ♗ ♖ ♕ ♔))
                        (p2lis '(♟ ♞ ♝ ♜ ♛ ♚)))
"Initiates game of chess and runs until an end condition is reached."
   (let ((end-res nil))
   (if (equal p1 p2) (print "Cannot set :p1 and :p2 to the same symbol.")
   (progn
   (let ((board (make-board)) (tempboard (make-board)) (draw-turn-count 1)
         (prev-num-left 32) (now-num-left 32) (turn-count 1) (okay nil)
         (id 1) (res nil) (move nil) (ischeckmate nil)
         (ischeck nil) (iscastle nil) (promotion nil)
         (nowking nil) (nowplayer nil) (nowpiece nil) (stalemate nil))

      (setf nowplayer p1)
      (print-ui board :p1sym p1 :p2sym p2 :p1lis p1lis :p2lis p2lis)

      (loop do (setf *random-state* (make-random-state t))

         if okay
                   do (setf now-num-left (num-left board))
                        (print (str+ "Draw turn count = " draw-turn-count))
                        (print (str+ "Turn count = " turn-count))
                        (incf turn-count)
                        (if (= prev-num-left now-num-left) (incf draw-turn-count)
                  (setf draw-turn-count 1))
                        (setf okay nil)

            do (setf prev-num-left now-num-left)

            if (drawp draw-turn-count board) do (print "Draw.")
               (setf end-res 'draw)
               (return) end

            do (setf tempboard board)
		(setf promotion nil)
		(setf id (if (eq nowplayer p1) 1 -1))
		(setf nowking (find-king id board))
		(setf ischeck (checkp id nowking board))

            if ischeck do (setf ischeckmate (checkmatep id nowking board)) end

            if ischeckmate
               do (print (str+ (if (eq nowplayer p1) p2 p1) "  wins."))
                  (setf end-res (if (eq nowplayer p1) 'p2 'p1))
                  (return) end

            if ischeck do (print (str+ nowplayer "  is in check.")) end

            if (not ischeck) do (setf stalemate (stalematep id nowking board))

            if stalemate
               do (print (str+ "Stalemate on " nowplayer " 's turn."))
                  (setf end-res 'stalemate)
                  (return) end

            if automatic-game
               if (eq nowplayer adversary-sym)
                  do (setf res (car (best-move-after-n-turns id nowking board :n adversary-depth)))
               else
                  do (setf res (make-random-move id nowking board))

            if (not automatic-game)
               if (eq nowplayer adversary-sym)
                  if (< turn-count 5)
                     do (setf res (make-random-move id nowking board))
                  else
                     do (setf res (car (best-move-after-n-turns id nowking board :n adversary-depth)))
               else do (setf res (player-turn nowplayer board))

            if (not (null res))
               do (setf move (car res))
                  (setf iscastle (cadr res))
                  (setf nowpiece (find-piece (caar move) (cadar move) board)) end

            if (and (not (null move)) (or (and (eq nowplayer p1) (< 0 nowpiece))
                                          (and (eq nowplayer p2) (> 0 nowpiece))))
               do (setf board (update-board move board (decide-update-nums move board iscastle)))
                  (setf nowking (find-king id board))
                  (setf ischeck (checkp id nowking board))

            else do (setf res nil) end

            if ischeck
               do (print "You put yourself in check.")
                  (setf res nil) end

            if (null res)
               do (setf board tempboard)
                  (wrong-move-prints)
                  (print move)

            else do (print-ui board :p1sym p1 :p2sym p2 :p1lis p1lis :p2lis p2lis)
                    (setf promotion (promote-pawn id board (or automatic-game (eq adversary-sym nowplayer))))
                    (if promotion (setf board promotion))
                    (setf nowplayer (if (eq nowplayer p1) p2 p1))
                    (setf okay t) end

            if promotion do (print-ui board :p1sym p1 :p2sym p2 :p1lis p1lis :p2lis p2lis)))

   (print "")))
   end-res)) ;; returns end result (p1/p2 wins, stalemate, draw...)
