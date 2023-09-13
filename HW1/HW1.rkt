;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #t)))
;1
(define-struct film(title genre rating running-time opening-date nomination))
;a film is a (make-film String String String Natural date Natural)
;title is the name of the movie STRING
(define-struct date(year month day))
;(define-struct opening-date(year month day))
;date is a (make-date Natrual Natrual Natural)
;genre is the genre of the movie STRING
;rating intended auiance for the movie STRING
;running-time how long the movie runs for INTIGER (minutes)
;opening-date stores 3 natural of date movie opening
;year 4 digit intiger
;month intiger 1-12 (12: December 1: January)
;day natural 1-31 representing a day of month


;M0 = Interstellar
;M1 = Wolf on Wall Street
;M2 = The Departed


(define InterstellarDate (make-date 2014 10 26)) ;Holds date for interstellar
(define M0(make-film "Interstellar" "Sci-fi" "R" 169 InterstellarDate 9))

(film-opening-date M0)
(film-opening-date M0)
(date-day InterstellarDate)

(define WolfDate (make-date 2013 12 25)) ;Holds date for Wolf on Wall Street
(define M1(make-film "Wolf On Wall Street" "Comedy" "R" 180 WolfDate 2))


(define DepartedDate (make-date 2006 10 6)) ;Holds date for The Departed
(define M2(make-film "The Departed" "Drama" "NR" 169 DepartedDate 4))

(define T-TIME(make-date 2013 4 5))

(define N-TIME(make-date 2015 6 7))

(define H-TIME(make-date 2019 9 9))


;2
;


;3

;high-brow? consumes film return true if movie is a Drama, 150 min long and has a raiting of NC-17 or NR
; Film genre runtime and raiting -> Boolean

(define (high-brow? movie)
  (cond [(and (string=? (film-genre movie) "Drama") (> (film-running-time movie) 150)) true]
        [(and (> (film-nomination movie) 0) (or (string=? (film-rating movie) "NC-17") (string=? (film-rating movie) "NR"))) true]
        [else false]))

(high-brow? M1)
(high-brow? M2)

;check-expect (high-brow? M2) true)
;(check-expect (high-brow? M1) false)


;4
;Number Number -> Number
;Takes in 2 movies nominations and computes the total amount of nominations
(define (total-nominations movie1 movie2)
  (+ (film-nomination movie1) (film-nomination movie2)))

(check-expect(total-nominations M0 M1) 11)

;5 WTF
;(require struct-update)
;film number -> film
;consumes a film and a number of nominations and replaces the nominations in the orginial data
;(define (update-nominations movie newNominations)
;  (define-struct-updaters movie)
;  (film-nomination-set (movie newNominations)))

;6
(define newDate(make-date 2003 05 05))
(define newDate2(make-date 2022 05 05))
(define (opened-after? movie befDate)
  (cond [(and (and (< (date-year befDate) (date-year(film-opening-date movie)))
              (< (date-month befDate) (date-month(film-opening-date movie))))
             (< (date-day befDate) (date-day(film-opening-date movie))))true]
        [else false]))
        
        ;[(< (date-month befDate) (date-month(film-opening-date movie))) true]
        ;[(< (date-day befDate) (date-day(film-opening-date movie))) true]))

(opened-after? M1 newDate)
(opened-after? M1 newDate2)




