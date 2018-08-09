;;;; Austin Kruczek
;;;; Chess in LISP
;;;; Version 1.0

;;; UI Functions

(defun num-to-name (piece &key (p1sym 'b) (p2sym 'w)
                             (p1lis '(♙ ♘ ♗ ♖ ♕ ♔))
                             (p2lis '(♟ ♞ ♝ ♜ ♛ ♚)))
"Piece is already an integer. This converts it to the appropriate string."
      (cond ((null piece) "   ")
            ((or (= piece 1) (= piece 2))   (str+ " " (nth 0 p1lis) " "))
            ((= piece 3)                    (str+ " " (nth 1 p1lis) " "))
            ((= piece 4)                    (str+ " " (nth 2 p1lis) " "))
            ((or (= piece 5) (= piece 6))   (str+ " " (nth 3 p1lis) " "))
            ((= piece 7)                    (str+ " " (nth 4 p1lis) " "))
            ((or (= piece 8) (= piece 9))   (str+ " " (nth 5 p1lis) " "))
            ((or (= piece -1) (= piece -2)) (str+ " " (nth 0 p2lis) " "))
            ((= piece -3)                   (str+ " " (nth 1 p2lis) " "))
            ((= piece -4)                   (str+ " " (nth 2 p2lis) " "))
            ((or (= piece -5) (= piece -6)) (str+ " " (nth 3 p2lis) " "))
            ((= piece -7)                   (str+ " " (nth 4 p2lis) " "))
            ((or (= piece -8) (= piece -9)) (str+ " " (nth 5 p2lis) " "))
            (t "   ")))

;; Strings used for prints to the console that
;; function as a simplistic UI.
(defparameter *line*    "—————————————————————————————————")
(defparameter *col-str* "  A   B   C   D   E   F   G   H  ")

(defun board-to-strlis (twoD &key (p1sym 'w) (p2sym 'b)
                                     (p1lis '(♙ ♘ ♗ ♖ ♕ ♔))
                                     (p2lis '(♟ ♞ ♝ ♜ ♛ ♚))
                                     (is-array nil))
"Takes a board and converts it to a list of strings with each representing a row."
   (let ((lis ()) (str "") (count 0))
      (setf lis (cons *line* lis))
         (loop for r from 0 to 7
            do (loop for c from 0 to 7
                  do (setf str (str+ str "|" (num-to-name (if is-array (@r&c r c twoD) ;;else
                                                                       (nth c (nth r twoD)))
                                                            :p1sym p1sym :p2sym p2sym
                                                            :p1lis p1lis :p2lis p2lis))))
            do (setf lis (cons (str+ str "|") lis))
               (setf lis (cons *line* lis))
               (setf str "")
            finally (return lis))))

(defun print-ui (twoD &key (p1sym 'w) (p2sym 'b)
                           (p1lis '(♙ ♘ ♗ ♖ ♕ ♔))
                           (p2lis '(♟ ♞ ♝ ♜ ♛ ♚)))
"Converts a given 2D-list or 2D-array to a list of strings, then prints them."
   (let ((lis (append (board-to-strlis twoD :p1sym p1sym :p2sym p2sym
                                            :p1lis p1lis :p2lis p2lis
                                            :is-array (arrayp twoD))
                      (list *col-str*))))
      (loop with count = 8 for item in lis
         if (and (null (eq item *line*)) (null (eq item *col-str*)))
            do (print (str+ " " count "  " item))
               (setf count (1- count))
         else do (print (str+ "    " item))
         finally (print ""))))
