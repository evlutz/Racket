;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lutz-Evan-HW2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #t)))
;(define-struct tornado(scale distance max-winds))
(define-struct tornado(scale distance max-winds))
;scale =  Fujita scale rating ("F0", "F1", ... , "F5")
;distance = distance travled by tornado in miles
;max-winds = max winds speeds (MPH) NATURAL



(define-struct hurricane(name category max-winds velocity heading))
;name = name of hurricane STRING
;category = number 1-5 NATURAL
;max-winds = Max wind speeds in MPH NATURAL
;velocity = Speed of storm in MPH NATURAL
;heading = ? STRING


(define-struct thunderstorm(heading velocity rainfall max-winds))
;heading = ? STRING
;velocity = Speed of storm in MPH NATURAL
;rainfall = inches of rainfell NATURAL
;max-winds = Max wind speeds in MPH NATURAL


(define Reno(make-tornado "F5" 16.2 296))

(define SANDY(make-hurricane "Sandy" 3 115 28 "NNE"))

(define TS(make-thunderstorm "N" 7 4 20))

(tornado-scale Reno)


;windstorm is one of thunderstorm
;windstorm is one of hurrican
;windstorm is one of tornado


