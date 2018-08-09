;;;; Austin Kruczek
;;;; Chess in LISP
;;;; Version 1.0

;;; Adversary Functions

;; Adversary calculates an integer for move desirability by:
;; 1) Factoring in piece values for possible takes in next turn.
;; 2) Factoring in piece values of pieces in list of possibly lost pieces next turn and the length of said list.
;; 3) Factoring in if the move puts the opponent in check and/or checkmate.
;; 4) Factoring in if the move puts itself in check and/or checkmate?...
;; 5) Finally uses its own move rank algorithms to determine the opponents best move and calc a future move based on that.

(defun a-moves (id board)
"Gets legal move lists (not counting check) of all a given player's (id) pieces. Returns an a-list of them."
   (let ((movelists ()) (nowpiece nil) (lessthan0 (< id 0)))
      (loop for r from 1 to 8
         do (loop for c from 1 to 8
               do (setf nowpiece (find-piece r c board))
               if (or (and lessthan0 (> 0 nowpiece)) (and (not lessthan0) (< 0 nowpiece)))
                  do (push (list (list r c) (get-legal-moves (list r c) board)) movelists))
         finally (return movelists))))

(defun a-moves-!check (id kingtile board)
"Filters the a-move list of moves that result in check, then returns it."
   (let ((amoves (a-moves id board)) (valid-amoves ()) (temp-alis nil) (tempboard board) (tiles nil))
      (loop for a-movelis in amoves
	  do (setf temp-alis (list (car a-movelis) ()))
	  do (loop for tile in (cadr a-movelis)
	        do (setf tiles (list (car temp-alis) tile))
	           (setf tempboard (update-board tiles board (decide-update-nums tiles board nil))) ; TODO: castling
	        if (not (checkp id (find-king id tempboard) tempboard))
	           do (push tile (cadr temp-alis)) end
	        do (setf tiles nil) (setf tempboard board))
	  do (push temp-alis valid-amoves)
	     (setf temp-alis nil)
	  finally (return valid-amoves))))

(defun remove-nil-amoves (a-moves)
"Filters the a-moves list of pieces that have no moves."
   (loop for a-movelis in a-moves
      if (null (cadr a-movelis))
         do (setf a-moves (remove a-movelis a-moves))
      finally (return a-moves)))

(defun filtered-a-moves (id kingtile board)
"Filters a-moves based on a-moves-!check and remove-nil-amoves."
   (remove-nil-amoves (a-moves-!check id kingtile board)))

(defun make-random-move (id kingtile board)
"Generates a random move from the result of a-moves. Doesn't take check into account for validity."
   (let ((legal-moves (filtered-a-moves id kingtile board)) ;use optional arg for this? a-moves/a-moves-check?
         (selected nil) (generated-move nil) (blah ()))
      (setf selected (nth (random (length legal-moves)) legal-moves))
      (setf generated-move (list (car selected) (nth (random (length selected)) (cadr selected))))
      (process-move generated-move board)))

;; TODO: recalculating a-moves every time fix.
(defun num-of-moves (id board)
"Returns the integer sum of the number of moves a player can make."
   (let ((kingtile (find-king id board)) (a-moves nil))
      (setf a-moves (filtered-a-moves id kingtile board))
      (loop with total = 0 for a-movelis in a-moves
         do (setf total (+ total (length (cadr a-movelis))))
         finally (return total))))

(defun find-a-takes (id kingtile board)
"Find all the takes a given player can do."
   (let ((kingtile (find-king id board)) (a-moves nil)
         (piece nil) (lessthan0 (< id 0)) (a-takes nil))
      (setf a-moves (filtered-a-moves id kingtile board))
      (loop for a-movelis in a-moves
         do (push (list (car a-movelis) ()) a-takes)
         do (loop for tile in (cadr a-movelis)
               do (setf piece (find-piece (car tile) (cadr tile) board))
               if (or (and lessthan0 (< piece 0)) (and (not lessthan0) (> piece 0)))
                  do (push tile (cadr (car a-takes))))
         finally
            (loop for a-movelis in a-takes
               if (null (cadr a-movelis))
                  do (setf a-takes (remove a-movelis a-takes))
               finally (return a-takes)))))

(defun num-of-takes (id kingtile board) ;; or pass a-takes...
"Calculates the number of takes a given player can do."
   (let ((atakes (find-a-takes id kingtile board)))
      (loop with total = 0 for a-takelis in atakes
         do (setf total (+ total (length (cadr a-takelis))))
         finally (return total))))

(defun rank-move (tiles id kingtile board n) ;; or pass a-takes...
"A clunky way to arithmetize a rank for a given move using rank-after-n-turns and other simplistic calculations."
   (let ((tempboard board) (temp-piece nil) (move nil) (other-id (if (< id 0) 1 -1)) (rank 0)
         (other-a-moves nil) (other-king nil) (can-be-taken-after nil) (can-be-taken-before nil))
      (setf other-king (find-king other-id tempboard))
      (setf other-a-moves (filtered-a-moves other-id other-king tempboard))
      (setf can-be-taken-before (some #'(lambda (x) (member (cadr tiles) (cadr x) :test #'equal)) other-a-moves))
      (setf tempboard (update-board tiles board (decide-update-nums tiles board nil))) ;;nil b/c no castling yet
      (setf other-a-moves (filtered-a-moves other-id other-king tempboard))
      (setf can-be-taken-after (some #'(lambda (x) (member (cadr tiles) (cadr x) :test #'equal)) other-a-moves))
      (cond ((checkmatep other-id other-king tempboard) (setf rank 100))
            (t (setf rank (+ (abs (find-piece (caadr tiles) (cadadr tiles) board)) ;; found-piece should be opposite color than given
                             (/ (num-of-moves id tempboard) 25)                    ;; weighting the num-of-moves
                             (/ (num-of-takes id kingtile tempboard) 10)))))       ;; weighting the num-of-takes
      (if can-be-taken-after (setf rank (- rank 10)))
      (if (and can-be-taken-after (= 7 (abs (find-piece (caar tiles) (cadar tiles) board)))) (setf rank (- rank 20)))
      (if (some #'(lambda (x) (member (find-queen id tempboard) (cadr x) :test #'equal)) other-a-moves) (setf rank (- rank 25)))
      (if (and can-be-taken-before (not can-be-taken-after))(setf rank (+ rank 20)))
      (if (equal (car tiles) kingtile) (setf rank (- rank 10)))
      (if (= 1 (abs (find-piece (caar tiles) (cadar tiles) board))) (setf rank (+ rank 5)))
   (+ rank (rank-after-n-turns n id (find-king id tempboard) tempboard)))) ;; returns the rank

;; rank-amove ex = '(((1 2)(3 4)) 21) where car is the move tiles and cadr is the rank.
(defun a-rank-moves (id kingtile board amoves!check n)

   (let ((a-ranked-moves ()) (temp-tiles nil))
      (loop for a-movelis in amoves!check
         do (loop for tile in (cadr a-movelis)
               do (setf temp-tiles (list (car a-movelis) tile))
               do (push (list temp-tiles (rank-move temp-tiles id kingtile board n)) a-ranked-moves))
         finally (return a-ranked-moves))))

(defun best-move-after-n-turns (id kingtile board &key (n 0))
"Attempts to find the best move based on the boards from 0 to n turns away."
   (let ((amoves!check (filtered-a-moves id kingtile board)) (choice nil) (temp-choice nil))
      (let ((a-ranked (a-rank-moves id kingtile board amoves!check n)))
         (loop for a-ranklis in a-ranked
            if (or (null choice) (< (cadr choice) (cadr a-ranklis)))
               do (setf choice a-ranklis)
            finally
               (return (list (list (car choice) nil) (cadr choice))))))) ;; nil b/c castling not allowed yet

(defun rank-after-n-turns (n id kingtile board)
"Utilizes best-move-after-n-turns to sum a score of best moves into the future and returns that sum."
   (if (= n 0)
       0
   ;; else
      (progn
      (let ((nowplayer-best (best-move-after-n-turns id kingtile board)) (rank 0)
            (opponent-best nil) (opponent-id (if (= 1 id) -1 1))
            (tempboard board) (temp-best nil) (opponent-king nil))

      (setf rank (cadr nowplayer-best))

      ;; current player's turn
      (setf tempboard (update-board (caar nowplayer-best) board
            (decide-update-nums (caar nowplayer-best) board (cadar nowplayer-best))))

      (setf opponent-king (find-king opponent-id tempboard))
      (if (checkmatep opponent-id opponent-king tempboard)
          (+ rank 100)

      ;; else
         (progn
            (setf opponent-best (best-move-after-n-turns opponent-id (find-king opponent-id tempboard) tempboard))

            ;;opponent's turn
            (setf tempboard (update-board (caar opponent-best) tempboard
                  (decide-update-nums (caar opponent-best) tempboard (cadar opponent-best))))

            (+ rank (rank-after-n-turns (1- n) id (find-king id tempboard) tempboard))))))))
