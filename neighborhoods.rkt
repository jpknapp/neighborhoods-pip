#! /usr/bin/env racket
#lang racket
;; data structures: coordinate, neighborhood
(struct coord (lat lng))
(struct hood (name coords))

;; get input files from command line arguments
(unless (>= (vector-length (current-command-line-arguments)) 2)
    (display "incorrect number of arguments")
    (exit))
(define hoods_filename (vector-ref (current-command-line-arguments) 0))
(define points_filename (vector-ref (current-command-line-arguments) 1))

;; determine if a string is eof or empty
(define (is-empty-str? str)
  (or (eof-object? str) (zero? (string-length str))))

;; create a coord from a string in the format "[lat],[lng]"
(define (string->coord str)
  (let ((strlist (string-split (string-trim str) ",")))
    (coord (string->number (car strlist)) (string->number (cadr strlist)))))

;; read neighborhoods and their coordinates list from a file recursively, output list
(define (read-hoods-from-file file)
  (let ((line (read-line file)))
    (if (is-empty-str? line)
      empty
      (cons (hood (string-trim (string-trim line) ":") (read-coords-list file)) (read-hoods-from-file file)))))

;; read coords from a file recursively until blank line or eof is encountered, output list
(define (read-coords-list file)
  (let* ((line (read-line file))
         ;; check line for : (sample points file has 'Point n:' before each point)
         (coordstr
          (if (and (not (is-empty-str? line)) (regexp-match ":" line))
              (cadr (string-split line ":"))
              line)))
    (if (is-empty-str? coordstr)
      empty
      (cons (string->coord coordstr) (read-coords-list file)))))

;; create list of neighborhoods
(define hoods (call-with-input-file hoods_filename read-hoods-from-file))

;; make list containing pairs of coords (segments). make sure first coord is lower lat than second
(define (hood-coord-pairs coords)
  (build-list (sub1 (length coords)) (lambda (i)
                                       ;; make sure first coord is lower than second
                                       (let ((c1 (list-ref coords i))
                                             (c2 (list-ref coords (+ 1 i))))
                                         (if (< (coord-lat c1) (coord-lat c2)) 
                                             (cons c1 c2)
                                             (cons c2 c1))))))

;; nudge test point by this much if it lies on a polygon point
(define e 0.000001)

;; not-equal
(define (neq? x y) (not (eq? x y)))

;; check if given coord is within the given polygon
;; ray-casting algorithm https://en.wikipedia.org/wiki/Point_in_polygon
(define (coords-in-polygon? c poly)
  (odd?
   (for/fold ((i 0)) ((seg poly))
     (+ i (if (ray-cross-seg? c seg) 1 0)))))

;; check if a ray from the given coord crosses the given segment
(define (ray-cross-seg? r s)
  (let* ((Ax (coord-lng (car s))) (Ay (coord-lat (car s)))
         (Bx (coord-lng (cdr s))) (By (coord-lat (cdr s)))
         (Px (coord-lng r)) (Pyo (coord-lat r))
         (Py (+ Pyo (if (or (eq? Pyo Ay)
                            (eq? Pyo By))
                        e 0))))
    (cond ((or (< Py Ay) (> Py By)) #f)
          ((> Px (max Ax Bx)) #f)
          ((< Px (min Ax Bx)) #t)
          (else
           (let ((seg-AB (if (neq? Ax Bx)
                          (/ (- By Ay) (- Bx Ax))
                          +inf.0))
                 (seg-AP (if (neq? Ax Px)
                           (/ (- Py Ay) (- Px Ax))
                           +inf.0)))
             (if (>= seg-AP seg-AB) #t #f))))))

;; kick off the process by reading points from sample file
(call-with-input-file points_filename (lambda (file)
                                        (let ((test-coords (read-coords-list file)))
                                          ;; loop through sample coords
                                          (for ((c test-coords)
                                                (i (in-range 1 (+ 1 (length test-coords)))))
                                            (let ((match "<none>"))
                                              ;; check every neighborhood to see if sample coord is contained within
                                              (for ((h hoods))
                                                ;; break out of the loop if a match is found
                                                #:break (neq? match "<none>")
                                                (if (coords-in-polygon? c (hood-coord-pairs (hood-coords h)))
                                                    (set! match (hood-name h))
                                                    empty)
                                                )
                                              (printf "Point ~s: " i)
                                              (display match)
                                              (newline)
                                              )))))