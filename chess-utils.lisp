;;;; Austin Kruczek
;;;; Chess in LISP
;;;; Version 1.0

;;; Chess Utility Functions

;; Examples of valid input moves:
;;    Pawn a2-A4, pawnA2-a4, Pa2-a4, a2-a4, a2a4

;; Take a move like a2-c4, then split to find indices. The "tiles" become index lists.
;; a2-a4 = ( (1 2) (3 4) ), where caar = 1, cadar = 2, caadr = 3, cadadr = 4
(defun col-to-num (col)
"Converts a column given as a string to its program-specific integer equivalent."
   (loop with num = 1
         for item in '(A B C D E F G H)
         until (equal col (string-upcase (string item)))
	     do (setf num (1+ num))
         finally (return num)))

;; TODO: make error-handling more robust here
(defun separate-pos (move)
"Takes a move as a string and returns something like ('a2' 'a4')."
   (setf move (remove #\- move))
   (let ((len (length move)))
      (let ((targpos (subseq move (- (length move) 2) (length move)))
            (nowpos  (subseq move (- (length move) 4) (- (length move) 2))))
         (list nowpos targpos))))

;; TODO: make error-handling more robust here
(defun get-indexes (pos-list)
"Takes the result of (separate-positions...) and returns ((row1 col1) (row2 col2))."
   (if (null pos-list) nil
      (list (list (parse-integer (subseq (car pos-list) 1))
               (col-to-num (subseq (car pos-list) 0 1)))
            (list (parse-integer (subseq (cadr pos-list) 1))
               (col-to-num (subseq (cadr pos-list) 0 1))))))

(defun in-boundsp (move)
"Determines if a move like '((1 2) (3 4)) is in bounds."
   (not (some (lambda (x) (or (< x 1) (> x 8))) (append (car move) (cadr move)))))

(defun make-move-readable (move)
"Uses get-indexes and separate-pos to make the move readable by other functions."
   (get-indexes (separate-pos move)))

(defun process-move (tiles board)
"Handles logic to process a move-string."
   (if (or (null (car tiles)) (null (cadr tiles)) (not (in-boundsp tiles))) nil
      (let ((piece nil) (targpiece nil) (abspiece nil))
         (setf piece (find-piece (caar tiles) (cadar tiles) board))
         (setf targpiece (find-piece (caadr tiles) (cadadr tiles) board))
         (setf abspiece  (abs piece))
         (cond ((null tiles) nil)
               ((equal (car tiles) (cadr tiles)) nil)
               ((and (not (= abspiece 9))
                     (or (= 0 piece) (and (< 0 piece) (< 0 targpiece)) (and (> 0 piece) (> 0 targpiece))))
                  nil)
               ;; Below: if valid move, return list of from & to tiles & whether or not it is valid castling
               ((or (= 1 abspiece) (= 2 abspiece)) (if (pawn-move-validp tiles board)   (list tiles nil) nil))
               ((= 3 abspiece)                     (if (knight-move-validp tiles board) (list tiles nil) nil))
               ((= 4 abspiece)                     (if (bishop-move-validp tiles board) (list tiles nil) nil))
               ((or (= 5 abspiece) (= 6 abspiece)) (if (rook-move-validp tiles board)   (list tiles nil) nil))
               ((= 7 abspiece)                     (if (queen-move-validp tiles board)  (list tiles nil) nil))
               ((or (= 8 abspiece) (= 9 abspiece))
                   (cond ((king-move-validp tiles board) (list tiles nil))
                         ;((castling-validp tiles board)  (list tiles t)) ; TODO: castling
                         (t nil)))
               (t nil)))))

(defun get-legal-moves (tile board)
"Returns a list of legal tiles a piece can to move to. Takes something like (1 1) as parameter."
   (let ((lis ()) (tempvar nil))
      (loop with r = 1 for row in board
         do (loop with c = 1 for col in row
               do (setf tempvar (process-move (list tile (list r c)) board))
                  (if tempvar (setf lis (cons (cadar tempvar) lis)))
                  (setf c (1+ c)))
            (setf r (1+ r))
         finally (return lis))))

(defun make-board ()
"Creates a list of lists of ints. First list index is row. Nested lists' item indices are columns."
   (let ((main '(6 3 4 7 9 4 3 6)) (pawns '(2 2 2 2 2 2 2 2)) (zeros '(0 0 0 0 0 0 0 0)))
      (list main pawns zeros zeros zeros zeros
         (mapcar #'(lambda (x) (setf x (- 0 x))) pawns)
         (mapcar #'(lambda (x) (setf x (- 0 x))) main))))

(defun make-board-as-array ()
"Creates a 2D-array representation of a chessboard."
   (make-array '(8 8) :initial-contents (make-board)))

(defun decide-update-nums (tiles board iscastle)
"Returns something like (0 4), where 0 is the target tile's piece and 4 is the original tile's piece."
   (let ((nowrow  (caar   tiles))
         (nowcol  (cadar  tiles))
         (targrow (caadr  tiles))
         (targcol (cadadr tiles))
         (piece (find-piece (caar tiles) (cadar tiles) board)))
      (cond (iscastle (list (find-piece targrow targcol board) (if (< 0 piece) 8 -8)))
            ((= piece 2) (list 0 1)) ((= piece -2) (list 0 -1))
            ((= piece 6) (list 0 5)) ((= piece -5) (list 0 -5))
            ((= piece 9) (list 0 8)) ((= piece -9) (list 0 -8))
             (t (list 0 piece)))))

(defun update-tile (row col set-as board)
"Updates a single piece in the board with the val of 'set-as'"
   (let ((new-board ()) (temp-lis ()))
      (loop with r = 1 for rowlis in board
            if (= r row)
               do (loop with c = 1 for piece in rowlis
                        if (= c col) do (setf piece set-as) end
                        do (setf c (1+ c))
                           (setf temp-lis (append temp-lis (list piece)))
                        finally (setf new-board (cons temp-lis new-board)))
                  (setf temp-lis ())
		   (setf r (1+ r))
            else
               do (setf r (1+ r))
                  (setf new-board (cons rowlis new-board))
            finally (return new-board))))

(defun update-board (tiles board set-as-lis)
"Takes two coordinates (tiles). First coord set to set-as, second coord set to first coord's piece val."
   (let ((nowrow  (caar   tiles))
         (nowcol  (cadar  tiles))
         (targrow (caadr  tiles))
         (targcol (cadadr tiles))
         (temp-piece nil))
      (setf temp-piece (find-piece nowrow nowcol board))
      (setf board (update-tile nowrow nowcol (car set-as-lis)
            (reverse (update-tile targrow targcol (cadr set-as-lis) board))))
      (reverse board))) ;; returns the board in the proper orientation.

;; TODO: Not yet implemented. Castling is still not permitted.
(defun update-from-castle (tiles board)
"Updates tiles upon castling.")

;; TODO: more robust input/error handling
(defun promote-pawn (id board is-adversary)
"Takes a player and the current board. Depending on the player, a different row is checked."
   (let ((row (if (= id 1) 8 1)) (pawnval id) (choice nil))
      (loop for col from 1 to 8
         if (= pawnval (find-piece row col board))
            if is-adversary
               do (setf choice 7)
            else
               do (print "Promote: N = 3, B = 4, R = 5, Q = 7, Enter int below:")
                  (print "")
		   (setf choice (read)) end
         if choice do (return (reverse (update-tile row col (if (= 1 id) choice (- 0 choice)) board))))))
