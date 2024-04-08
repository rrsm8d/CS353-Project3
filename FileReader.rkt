#lang racket

; PRE: takes an input filename
; POST: Returns a list of strings, containing each line.
(define (read-file filename)
  (file->lines filename)
  )

(provide read-file)