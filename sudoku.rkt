#lang racket/gui

(require racket/gui)

(define speed 1.000025)

(define (get-row sudoku-field row)
  (if (= row 1)
      (first sudoku-field)
      (get-row (rest sudoku-field) (- row 1))))

(define (get-column sudoku-field column)
  (define (iter res row)
    (if (> row 9) res
        (iter (append res (list (get-value sudoku-field row column))) (+ row 1))))
  (iter '() 1))

(define (get-square sudoku-field row column)
  (define square (+ (* 3 (floor (/ (- row 1) 3))) (floor (/ (- column 1) 3)) 1))
  (define start-row (+ (* 3 (floor (/ (- row 1) 3))) 1))
  (define start-column (+ (* 3 (floor (/ (- column 1) 3))) 1))
  (define (iter3 vals column)
    (if (= column 1)
        (list (first vals) (first (rest vals)) (first (rest (rest vals))))
        (iter3 (rest vals) (- column 1))))
  (define (iter2 sudoku-field row column res)
    (if (= row 0) res
        (iter2 (rest sudoku-field) (- row 1) column (append res (iter3 (first sudoku-field) column)))))
  (define (iter sudoku-field row column)
    (if (= row 1)
        (iter2 sudoku-field 3 column '())
        (iter (rest sudoku-field) (- row 1) column)))
  (iter sudoku-field start-row start-column))

(define (get-value sudoku-field row-number column)
  (define (iter row-values column)
    (if (= column 1) (first row-values)
        (iter (rest row-values) (- column 1))))
  (iter (get-row sudoku-field row-number) column))

(define (set sudoku-field row column value)
    ;iterates over the row and as soon the needed cell was found, change it
    (define (iter2 founded-row col result)
        (if (null? founded-row) (list result)
            (if (= col 1)
                ;if we found the right column (cell) change it and add            
                (iter2 (rest founded-row) (- col 1) (append result (list value)))
                ;otherwise just add
                (iter2 (rest founded-row) (- col 1) (append result (list (first founded-row))))
            )
        )
    )
    ;iterates over the sudoku-field to process each row and modify the specific row that matches the row index.
    (define (iter sudoku-field row column updated-sudoku-field)
        (if (null? sudoku-field) updated-sudoku-field
            (if (= row 1)
                ;change the cell number with iter2 if the row=1 and then add to updated-sudoku-field
                (iter 
                    (rest sudoku-field) 
                    (- row 1)
                    column 
                    (append updated-sudoku-field (iter2 (first sudoku-field) column '()))
                )
                ;otherwise just add the row to the updated-sudoku-field 
                (iter 
                    (rest sudoku-field) 
                    (- row 1) 
                    column 
                    (append updated-sudoku-field (list (first sudoku-field))))
            )
        )
    )
    (iter sudoku-field row column '())
)

;combines all check function to see, if the value is allowed to be put in the cell
(define (value-okay? sudoku-field row column)
  (and (not (= 0 (get-value sudoku-field row column)))
       (row-okay? (get-row sudoku-field row) column (get-value sudoku-field row column))
       (column-okay? (get-column sudoku-field column) row (get-value sudoku-field row column))
       (square-okay? (get-square sudoku-field row column) row column (get-value sudoku-field row column))))

(define (row-okay? row-values column value)
  (define (iter row-values column-number res)
    (if (null? row-values) res
        (if (= column-number 1)
            (iter (rest row-values) (- column-number 1) res)
            (iter (rest row-values) (- column-number 1) (append res (list (first row-values)))))))
  (foldl (lambda (n m)
           (if (= n 0) m
               (if (= n value) #f m)))
         #t (iter row-values column '())))

(define (column-okay? vals row value) (row-okay? vals row value))

(define (square-okay? vals row column value)
  (row-okay? vals (+ (* 3 (remainder (- row 1) 3)) (remainder (- column 1) 3) 1) value))

(define (step-solve-sudoku puzzle slow)
    (define (iter sudoku-field row column back)
        (if slow 
            (begin
               (print-puzzle (list sudoku-field (first (rest puzzle))) row column)
               (sleep speed)
            ) 
           ;if attribute slow false, instantaneous result
            (sleep 0))
        ;;; (display row)
        ;;; (display " ")
        ;;; (display column)
        ;;; (display "\n")
        (cond 
            ((= row 10) (list sudoku-field (first (rest puzzle)))) ; Puzzle is solved!    ----------------------------------------

            ;we check if the cell has already got a number (fixed value)
            ((get-value (first (rest puzzle)) row column) 
                ;if so, we check if we back track
                (if back
                    ;if we backtrack, we check, if we should change the row (get back) or just a column
                    (if (= column 1) (iter sudoku-field (- row 1) 9 #t) (iter sudoku-field row (- column 1) #t))
                    ;otherwise just keep going, check the next cell (checks as well if we should change a row)
                    (if (= column 9) (iter sudoku-field (+ row 1) 1 #f) (iter sudoku-field row (+ column 1) #f))
                )
            )

            ;if the value is allowed to be put in the cell and we are not back tracking
            ((and (value-okay? sudoku-field row column) (not back)) 
                (if (= column 9) 
                    ;come to next row if we reach the max amount of columns(9) (last cell of the row)
                    (iter sudoku-field (+ row 1) 1 #f) 
                    ;otherwise come to the next cell
                    (iter sudoku-field row (+ column 1) #f)))

            ((< (get-value sudoku-field row column) 9) ; Try a different value in that space
                (iter (set sudoku-field row column (+ (get-value sudoku-field row column) 1)) row column #f))

            ;else condition for the whole cond block
            (else
                (if (= column 1) 
                    ;set back tracking to true and go back. As we go back, we need to change the numbers back to 0
                    ;change the row
                    (iter (set sudoku-field row column 0) (- row 1) 9 #t)
                    ;change the column (go one cell back)
                    (iter (set sudoku-field row column 0) row (- column 1) #t)
                )
            )
        )
    )
    (iter (first puzzle) 1 1 #f)
)
    
 ;; This attaches the list of boolean values to the sudoku.  The bools are used to see if a value in a sudoku was given.
(define (create-sudoku-obj sudoku-field)
    (define (iter sudoku-field-bools row column)
        (cond 
            ((> column 9) (iter sudoku-field-bools (+ 1 row) 1))
            ((> row 9) (list sudoku-field sudoku-field-bools))

            ;if the cell is 0 (empty)            
            ((= 0 (get-value sudoku-field row column)) 
                ;set the cell to false and check the next cell  
                (iter (set sudoku-field-bools row column #f) row (+ column 1))
            )

            ;otherwise (if the cell is already taken with a number) 
            (else 
                ;set it to true (there is something) and check the next cell
                (iter (set sudoku-field-bools row column #t) row (+ column 1)))))
  (iter sudoku-field 1 1)
)

;;; Example sudokus listed below
(define sudoku-ex1
  (create-sudoku-obj
   '((0 0 0 9 7 0 0 0 0) 
    (0 4 0 2 5 0 1 0 7) 
    (0 0 7 6 0 0 4 0 3) 
    (0 1 2 8 0 0 6 0 0) 
    (9 7 0 0 4 0 0 3 5) 
    (0 0 4 0 0 2 9 1 0) 
    (2 0 1 0 0 7 5 0 0) 
    (4 0 9 0 8 1 0 6 0) 
    (0 0 0 0 2 9 0 0 0))))

(define sudoku-ex2
  (create-sudoku-obj
   '((0 0 0 2 6 0 7 0 1) 
    (6 8 0 0 7 0 0 9 0) 
    (1 9 0 0 0 4 5 0 0) 
    (8 2 0 1 0 0 0 4 0) 
    (0 0 4 6 0 2 9 0 0) 
    (0 5 0 0 0 3 0 2 8) 
    (0 0 9 3 0 0 0 7 4) 
    (0 4 0 0 5 0 0 3 6) 
    (7 0 3 0 1 8 0 0 0))))

(define sudoku-ex3
  (create-sudoku-obj
   '((5 8 0 6 0 0 4 0 0) 
    (7 0 0 0 0 3 6 0 0) 
    (0 0 0 0 9 1 0 8 0) 
    (0 0 0 0 0 0 0 0 0) 
    (0 5 0 1 8 0 0 0 3) 
    (0 0 0 3 0 6 0 4 5) 
    (0 4 0 2 0 0 0 6 0) 
    (9 0 3 0 0 0 0 0 0) 
    (0 2 0 0 0 0 1 0 0))))

(define sudoku-ex4
  (create-sudoku-obj
   '((0 0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0 0))))

;; --- End of examples ---


  
;; This will determine which sudoku will be solved
;; Contains two fields: one with numbers and one with booleans (check create-sudoku-object)
(define puzzle sudoku-ex1)


;;; ----- GUI IMPLEMENTATION BELOW -----


(define screen-height 600)
(define screen-width 600)

(define frame (new frame% 
                        [label "Sudoku Solver"] 
                        [width screen-width] 
                        [height (+ 100 screen-height)]
                )
)


(define main-panel (new horizontal-panel% 
                                    [parent frame]
                                    [alignment '(center center)]
                                    [min-height screen-height]
                                    [min-width screen-width]
                                    [stretchable-width false]	 
                                    [stretchable-height false]
                    )
)

(define canvas-global '())
(define dc-global '())

(new canvas% 
            [parent main-panel]
            [min-height screen-height]
            [min-width screen-width]
            [paint-callback
                (lambda (canvas dc)
                    (set! canvas-global canvas)
                    (set! dc-global dc)
                    (draw-grid canvas dc screen-height)
                    (draw-sub-grid canvas dc screen-height)
                    (print-puzzle puzzle 0 0)
                )
            ]
)


;;; Draw the main-grid structure on canvas.   
(define (draw-grid canvas dc field-size)
  (send dc set-pen "black" 4 'solid)
  (send dc draw-line (* 1/3 field-size) 0 (* 1/3 field-size) field-size)
  (send dc draw-line (* 2/3 field-size) 0 (* 2/3 field-size) field-size)
  (send dc draw-line 0 (* 1/3 field-size) field-size (* 1/3 field-size))
  (send dc draw-line 0 (* 2/3 field-size) field-size (* 2/3 field-size)))

;;; Draw the sub-grid structure on canvas.
(define (draw-sub-grid canvas dc field-size)
  (send dc set-pen "black" 1 'solid)
  (for ((i 9))
    (send dc draw-line (* (/ i 9) field-size) 0 (* (/ i 9) field-size) field-size)
    (send dc draw-line 0 (* (/ i 9) field-size) field-size (* (/ i 9) field-size))))


;scroll down till print-puzzle


;;; Check if current number was given at program start.
(define (is-given? table x y)
  (if (eq? (get-value table (+ 1 y) (+ 1 x)) #t) #t #f))

;;; Print each value in table based on position
(define 
        (print-ind 
                    val 
                    bools-table 
                    x 
                    y 
                    dc 
                    field-size 
                    row 
                    column
        )
    (define (helper col)
        (begin
            (if (and (= x (- column 1)) (= y (- row 1)))
                (send dc set-text-foreground "red")
                (send dc set-text-foreground col))
            (if (not (= 0 column))
                (begin
                    (send dc set-brush (new brush% [color "white"]))
                    (let (
                            (x (* (/ x 9) field-size)) 
                            (y (* (/ y 9) field-size))
                        )
                    (send dc set-pen "white" 1 'transparent)
                    (send dc draw-rectangle (+ x 5) (+ y 5) (/ screen-width 11) (/ screen-height 11)))
                    
                    (send dc draw-text 
                                        (if (equal? val " ") val (number->string val)) 
                                        (+ (* (/ x 9) field-size) 20) 
                                        (+ (* (/ y 9) field-size) 15)
                    )
                )
                (send dc draw-text 
                                    (if (equal? val " ") val (number->string val))
                                    (+ (* (/ x 9) field-size) 20) 
                                    (+ (* (/ y 9) field-size) 15)
                )
            )
        )
    )
  (if (equal? val 0) (set! val " ") #f)
  (if (not (is-given? bools-table x y))
      (helper "blue")     
      (helper "black")
    )
)

(define 
        (p-lst 
                row-lst
                bools-table 
                y 
                dc 
                field-size 
                row 
                column
        )

    (for ((val row-lst) (x 9))
        (print-ind 
                    val 
                    bools-table 
                    x 
                    y 
                    dc 
                    field-size 
                    row 
                    column
        )
    )
)

(define 
        (p-nested-lst 
                        lst 
                        bools-table 
                        dc 
                        field-size
                        row
                        column
        )
    (for ((item lst) (y 9)) 
        (p-lst 
                item 
                bools-table 
                y 
                dc 
                field-size 
                row 
                column
        )
    )
)


;;; Print entire table to canvas
(define (print-puzzle puzzle row column)
    (send dc-global set-font (make-font #:size 25 #:family 'default ))
    (p-nested-lst 
                (first puzzle) ;sudoku-field with numbers
                (first (rest puzzle)) ;sudoku-field-bools (we use first to remove the outer list)
                dc-global
                screen-height
                row
                column
    )
)


(define button-panel (new horizontal-panel% [parent frame]
                                     [alignment '(center bottom)]
                                     [min-height 100]
                                     [min-width 600]))

(define solve-button (new button% [parent button-panel] 
                          [label "Solve"]
                          [min-width 290]
                          [min-height 100]
                          [callback  
                            (lambda (button event) 
                                (begin 
                                    (print-puzzle (step-solve-sudoku puzzle #f) 0 0)
                                )
                            )])
)

(define step-button (new button% [parent button-panel] 
                          [label "Slow Solve"]
                          [min-width 290]
                          [min-height 100]
                          [callback  
                            (lambda (button event) 
                                (begin
                                    (print-puzzle (step-solve-sudoku puzzle #t) 0 0)
                                )
                            )])
)

(send frame show #t)
