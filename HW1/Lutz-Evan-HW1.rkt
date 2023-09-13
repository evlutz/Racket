;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Marsh-Ben-HW1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;1
;a film is a (make-film String String String Natural date Natural)

(define-struct film(title genre rating running-time opening-date nominations))

;title is the name of the movie STRING
;genre is the genre of the movie STRING
;rating is the intended auiance for the movie STRING
;running-time is how long the movie runs for in minutes NATURAL 
;opening-date is opening date as data type DATE


;date is a (make-date Natural Natural Natural)
(define-struct date(year month day))
;year is a 4 digit NATURAL
;month is a number 1-12 (1 = January | 12 = December) NATURAL
;day is a number 1-31 representing a day of month NATURAL


;Interstellar = Interstellar
;Wolf = Wolf on Wall Street
;Departed = The Departed



(define WolfDate (make-date 2013 12 25)) ;Holds date for Wolf (Wolf on Wall Street)
(define InterstellarDate (make-date 2014 10 26)) ;Holds date for Interstellar
(define DepartedDate (make-date 2006 10 6)) ;Holds date for Departed
(define T-TIME(make-date 2013 4 5)) ;Holds date for THOR
(define N-TIME(make-date 2015 6 7)) ;Holds date for NOTEBOOK
(define H-TIME(make-date 2019 9 9)) ;Holds date for HALLOWEEN



(define Interstellar(make-film "Interstellar" "Sci-fi" "R" 169 InterstellarDate 9))
(define Wolf(make-film "Wolf On Wall Street" "Comedy" "R" 180 WolfDate 2))
(define Departed(make-film "The Departed" "Drama" "NR" 169 DepartedDate 4))
(define THOR(make-film "thor" "action" "NC-17" 120 T-TIME 2))
(define NOTEBOOK(make-film "notebook" "drama" "NR" 200 N-TIME 9))
(define HALLOWEEN(make-film "halloween" "horror" "R" 120 H-TIME 1))

;2

;make-film -> film
;film? -> BOOLEAN
;title -> STRING
;genre -> STRING
;rating -> STRING
;running-time -> NATURAL 
;opening-date -> date
;nominations -> NATURAL


;3

;high-brow? consumes film return true if movie is a Drama, 150 min long and has a raiting of NC-17 or NR
;film -> Boolean

(define (high-brow? movie)
  (cond[(and(string=? (film-genre movie) "drama")(>= (film-running-time movie) 150))#true]
       [(and (>= (film-nominations movie) 1) (or (string=? (film-rating movie) "NR") (string=? (film-rating movie) "NC-17")))#true]
       [else #false]))

(check-expect (high-brow? THOR)#true)
(check-expect (high-brow? NOTEBOOK)#true)
(check-expect (high-brow? HALLOWEEN)#false)


;4
;FILM FILM -> Number
;Takes in 2 movies nominations and computes the total amount of nominations

(define(total-nominations movie movie2)
  (+(film-nominations movie)(film-nominations movie2)))

(check-expect (total-nominations THOR NOTEBOOK) 11)
(check-expect (total-nominations THOR HALLOWEEN) 3)

;5
;Film Number -> Film
;consumes a film and a number of nominations and replaces the nominations in the orginial data

(define (update-nominations movie nomnum)
  (make-film (film-title movie) (film-genre movie) (film-rating movie) (film-running-time movie) (film-opening-date movie) nomnum))


(check-expect (update-nominations THOR 10) (make-film "thor" "action" "NC-17" 120 (make-date 2013 4 5) 10))
(check-expect (update-nominations NOTEBOOK 15)(make-film "notebook" "drama" "NR" 200 (make-date 2015 6 7) 15))
(check-expect (update-nominations Interstellar 3)(make-film "Interstellar" "Sci-fi" "R" 169 (make-date 2014 10 26) 3))


;6
;Film Date -> Boolean
;Takes in a date and a film-struct and produces a boolean value based on if the film was released before the date
;True = Released before
;False = Released after
;givendate = Data type date
;movie = Data type film

(define (opened-after? movie givendate)
  (cond[(> (date-year(film-opening-date movie))(date-year givendate))#true]
       [(< (date-year(film-opening-date movie))(date-year givendate))#false]
       [(= (date-year(film-opening-date movie))(date-year givendate))(cond[(> (date-month(film-opening-date movie))(date-month givendate))#true]
                                                                          [(< (date-month(film-opening-date movie))(date-month givendate))#false]
                                                                          [(= (date-month(film-opening-date movie))(date-month givendate))(cond[(> (date-day(film-opening-date movie))(date-day givendate))#true]
                                                                                                                                               [else #false])])]))


(check-expect (opened-after? THOR T-TIME) #false)
(check-expect (opened-after? NOTEBOOK DepartedDate) #true)
(check-expect (opened-after? Departed N-TIME) #false)

                                                                                                                                             
                                                                                                                                               
                                                                          
                                                                     




