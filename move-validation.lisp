;;;; Austin Kruczek
;;;; Chess in LISP
;;;; Version 1.0

;;; Move Validation Functions

(defun path-check-horizontal (tiles board)
"If a piece is preventing a horizontal move, this returns nil, else returns t."
   (let ((nowrow  (caar   tiles))
         (nowcol  (cadar  tiles))
         (targcol (cadadr tiles))
         (bool t) (to nil) (from nil)
         (piece (find-piece (caar tiles) (cadar tiles) board))
         (temppiece nil)
         ;; Gets the row in which the piece is moving
         (using-row (nth (1- (caar tiles)) board)))
   (cond ((< nowcol targcol) (setf to (1- targcol)) (setf from nowcol))
          (t                 (setf to (1- nowcol))  (setf from targcol)))
   (loop with c = from until (= c to)
      do (setf temppiece (nth c using-row))
      if (not (= 0 temppiece))
         do (setf bool nil) (return)
      else do (setf c (1+ c)))
   bool))

(defun path-check-vertical (tiles board)
"If a piece is preventing a vertical move, this returns nil, else returns t."
   (let ((nowrow  (caar  tiles))
         (nowcol  (cadar tiles))
         (targrow (caadr tiles))
         (bool t) (to nil) (from nil))
      (cond ((< nowrow targrow) (setf to (1- targrow)) (setf from nowrow))
             (t                 (setf to (1- nowrow))  (setf from targrow)))
      ;; Must check the column which is being moved through from nowrow to targrow
      ;; (or vice versa depending on which is greater).
      (loop with r = from until (= r to)
         if (not (= 0 (nth (1- nowcol) (nth r board))))
            do (setf bool nil) (return) end
         do (setf r (1+ r))
         finally (return bool))))

;; Not my algorithm.
(defun path-check-diagonal (tiles board)
"If a piece is preventing a diagonal move, this returns nil, else returns t."
   (let ((nowrow  (caar   tiles))
         (nowcol  (cadar  tiles))
         (targrow (caadr  tiles))
         (targcol (cadadr tiles))
         (bool t) (dircol nil) (dirrow nil))
      (setf dircol (if (> targcol nowcol) 1 -1))
      (setf dirrow (if (> targrow nowrow) 1 -1))
      (loop for i from 1 to (1- (abs (- targrow nowrow)))
         if (not (= 0 (find-piece (+ nowrow (* i dirrow)) (+ nowcol (* i dircol)) board)))
            do (setf bool nil) (return))
      bool))

(defun find-piece (row col board)
"Gets piece on the tile @ row & col."
   (nth (1- col) (nth (1- row) board)))

(defun find-king (id board)
"Takes 1 or -1. Finds & returns the tile that piece is on. Returns something like (1 5) row 1 col 5."
   (let ((tile nil))
      (loop with r = 1 for row in board
         do (loop with c = 1 for piece in row
               if (or (and (> 0 id) (or (= -8 piece) (= -9 piece)))
                      (and (< 0 id) (or (= 8  piece) (=  9 piece))))
                  do (setf tile (list r c)) (return)
               else do (setf c (1+ c)))
         if (not (null tile)) do (return tile)
         else do (setf r (1+ r)))))

(defun find-queen (id board)
"Takes 1 or -1. Finds & returns the tile that piece is on. Returns something like (1 5) row 1 col 5."
   (let ((tile nil))
      (loop with r = 1 for row in board
         do (loop with c = 1 for piece in row
               if (or (and (> 0 id) (= -7 piece))
                      (and (< 0 id) (=  7 piece)))
                  do (setf tile (list r c)) (return)
               else do (setf c (1+ c)))
         if (not (null tile)) do (return tile)
         else do (setf r (1+ r)))))

(defun pawn-move-validp (tiles board)
"Determines if a pawn's move is valid."
   (let ((nowrow  (caar   tiles))
         (nowcol  (cadar  tiles))
         (targrow (caadr  tiles))
         (targcol (cadadr tiles))
         (nowpiece nil) (targpiece nil))
      (setf nowpiece  (find-piece nowrow  nowcol  board))
      (setf targpiece (find-piece targrow targcol board))
      (cond ((and (= nowpiece  2) (= targpiece 0) (= (+ nowrow 2) targrow) (= nowcol targcol)
               (path-check-vertical tiles board)))
            ((and (= nowpiece -2) (= targpiece 0) (= (- nowrow 2) targrow) (= nowcol targcol)
               (path-check-vertical tiles board)))
            ((and (= targpiece 0) (= nowcol targcol)
                  (or (and (< nowpiece 0) (= targrow (- nowrow 1)))
                      (and (> nowpiece 0) (= targrow (+ nowrow 1))))))
            ((and (> 0 targpiece) (< 0 nowpiece) (= (1+ nowrow) targrow)
                  (or  (= (1+ nowcol) targcol) (= (1- nowcol) targcol))))
            ((and (< 0 targpiece) (> 0 nowpiece) (= (1- nowrow) targrow)
                  (or  (= (1+ nowcol) targcol) (= (1- nowcol) targcol))))
             (t nil))))

(defun knight-move-validp (tiles board)
"Determines if a knight's move is valid."
   (let ((nowrow  (caar   tiles))
         (nowcol  (cadar  tiles))
         (targrow (caadr  tiles))
         (targcol (cadadr tiles)))
      (if (or (and (or (= (+ nowrow 2) targrow) (= (- nowrow 2) targrow))
                   (or (= (1+  nowcol) targcol) (= (1-  nowcol) targcol)))
              (and (or (= (1+  nowrow) targrow) (= (1-  nowrow) targrow))
                   (or (= (+ nowcol 2) targcol) (= (- nowcol 2) targcol))))
       t nil)))

;; Not my algorithm.
(defun bishop-move-validp (tiles board)
"Determines if a bishop's move is valid."
   (let ((nowrow  (caar   tiles))
         (nowcol  (cadar  tiles))
         (targrow (caadr  tiles))
         (targcol (cadadr tiles)))
      (cond ((and (= (abs (- targrow nowrow)) (abs (- nowcol targcol)))
                  (path-check-diagonal tiles board)))
             (t nil))))

(defun rook-move-validp (tiles board)
"Determines if a rook's move is valid."
   (let ((nowrow  (caar   tiles))
         (nowcol  (cadar  tiles))
         (targrow (caadr  tiles))
         (targcol (cadadr tiles)))
      (cond ((and (= nowrow targrow) (or (> nowcol targcol) (< nowcol targcol))
                  (path-check-horizontal tiles board)))
            ((and (= nowcol targcol) (or (> nowrow targrow) (< nowrow targrow))
                  (path-check-vertical   tiles board)))
             (t nil))))

(defun queen-move-validp (tiles board)
"Determines if a queen's move is valid."
   (cond ((bishop-move-validp tiles board))
         ((rook-move-validp tiles board))
          (t nil)))

;; TODO: add logic for king-side/queen-side castling
(defun king-move-validp (tiles board)
"Determines if a king's move is valid."
   (let ((nowrow  (caar   tiles))
         (nowcol  (cadar  tiles))
         (targrow (caadr  tiles))
         (targcol (cadadr tiles))
         (piece (find-piece (caar tiles) (cadar tiles) board)))
      (cond ((or (and (> piece 0) (> (find-piece (caadr tiles) (cadadr tiles) board) 0))
                 (and (< piece 0) (< (find-piece (caadr tiles) (cadadr tiles) board) 0)))
               nil)
            ((and (or (= nowrow targrow) (= (1+ nowrow) targrow) (= (1- nowrow) targrow))
                  (or (= nowcol targcol) (= (1+ nowcol) targcol) (= (1- nowcol) targcol))))
            (t nil))))


(defun castling-validp (tiles board)
"Determines is a given move is valid castling."
;; TODO this is going to need path-checking/take-checking (the king is moving through an illegal tile)
   (let ((king (find-piece (caar tiles) (cadar tiles) board)))
      (if (= (abs king) 8) nil
         (let ((checknum (if (< king 0) -6 6)))
            (let ((col (if (> (cadar tiles) (cadadr tiles)) 1 8)))
               (cond ((and (path-check-horizontal tiles board)
                           (= (find-piece (caadr tiles) col board) checknum)))
                     (t nil)))))))
