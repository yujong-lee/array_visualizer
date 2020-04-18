#lang racket
(require racket/draw)

(define thickness 1.99)

(define (create-pens colors widths)
  (cond
    [(empty? colors) empty]
    [else (cons
           (make-pen
             #:color (first colors)
             #:width (first widths)
             #:style `solid)
               (create-pens (rest colors) (rest widths)))]))
 	 
(define (create-colors alpha)
  (list
   (make-color 0 0 0 alpha) ;pure-black
   (make-color 229 229 229 alpha) ;light-grey
   (make-color 153 153 153 alpha) ;dark-grey
   (make-color 255 255 255 alpha))) ;pure white

(define (create-widths scale)
  (list thickness thickness thickness thickness))

(define (create-fonts)
  (list
   (make-font #:size 17 #:family 'script #:weight '550 #:smoothing `smoothed) ;default font
   (make-font #:size 11 #:family 'modern #:weight '400 #:smoothing `smoothed) ;idx font
   (make-font #:size 11 #:family 'modern #:weight '400 #:smoothing `smoothed))) ;comment font

(define (each-starting-point array block-size margin result)
  (for ([i (range (length array))])
    (set! result (cons (cons 0 (* (+ block-size margin) i)) result)))
  (reverse result))
          
(define (draw-list list starting-point block-size coner dc color-list pen-list font-list margin)

  (send dc set-pen (list-ref pen-list 0)) ;pure black
  (send dc set-font (list-ref font-list 0)) ;default 
  (send dc set-brush (list-ref color-list 3) 'solid) ;white
  (send dc set-origin (+ coner (car starting-point)) (+ coner (cdr starting-point)))

  
  (send dc draw-line 0 0 (* block-size (length list)) 0)
  (send dc draw-line 0 block-size(* block-size (length list)) block-size)
  (send dc draw-line(* block-size (length list)) 0 (* block-size (length list)) block-size)

  (for ([val list]
        [idx (range (length list))])
    (let-values ([(text-width text-height x y) (send dc get-text-extent (~r val))]
                 [(idx-width idx-height z w) (send dc get-text-extent (~r idx))])
 
    (send dc draw-line(* idx block-size) 0 (* idx block-size) block-size)
    (send dc draw-text (~r val) (- (* (+ idx 0.5) block-size) (/ text-width 2)) (/ (- block-size text-height) 2))
    
    (send dc set-font (list-ref font-list 1)) ;idx font 
    (send dc draw-text (~r idx) (- (* (+ idx 0.5) block-size) (/ idx-width 2)) block-size)
    (send dc set-font (list-ref font-list 0))))) ;default 

(define (highest-number xs)
  (define (max x1 x2)
    (if (> x1 x2) x1 x2))
  (foldl max (first xs) (rest xs)))

(define (move-parallel starting-points width)
  (map
   (lambda (point) (cons (+ (car point) width) (cdr point)))
     starting-points))

(define (max-name-size name-list dc)
  (define width-list empty)
  (define height-list empty)
  
  (for ([name name-list])
    (let-values ([(name-width name-height x y) (send dc get-text-extent name)])
      (set! width-list (cons name-width width-list))
      (set! height-list (cons name-height height-list))))

  (cons (highest-number width-list) (highest-number height-list)))

(define (visualize array name-list) ; array consist of lists.
  (let*
     ([block-size 50]
      [margin 40] ;20 20 error
      [coner 25]
      [array-count (length array)]
      [max-arr-elements (highest-number (map
                                          (lambda (list) (length list))
                                              array))]
      [full-width (+ (* block-size max-arr-elements) (* 2 coner) 70)] ; 70 is proportion to (car name-size)but can't write that directly
      [full-height (+ (* block-size array-count) (* margin (- array-count 1)) #;(inexact->exact (ceiling (* thickness array-count))) (* 2 coner))]
      ;comment not really consider. later consider with font too.

      [result (make-bitmap full-width full-height)]
      [dc (new bitmap-dc% [bitmap result])]

      [color-list (create-colors 1)]
      [width-list (create-widths 1)]
      [pen-list (create-pens color-list width-list)]
      [font-list (create-fonts)]
      [starting-points (each-starting-point array block-size margin empty)]
      [name-size (max-name-size name-list dc)])

      (send dc set-smoothing 'aligned)
      ;(send dc set-origin (coner coner)
      (for ([each-starting-point starting-points]
            [name name-list]
            [i (range (length name-list))])
              (send dc draw-text name (+ (car each-starting-point) coner) (+ (cdr each-starting-point) coner (/ (- block-size (cdr name-size)) 2))))

      (for ([each-starting-point (move-parallel starting-points (+ (car name-size) 20))]
            [list array])
        (draw-list list each-starting-point block-size coner dc color-list pen-list font-list margin))

       (send result save-file "ex_ver2.png" 'png)))