;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Kalaja-E-Lab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

;;Windstorm is one of the following
;; -Tornado
;; -Hurricane
;; -Thunderstorm

;; A tornado is a (make-tornado String Natural Natural)
;;    interp: represents a tornado where
;;            scale is the Fujita scale rating ("F0", "F1", ... , "F5")
;;            distance is the distance it travels (miles)
;;            max-winds is the maximum winds (mph)
(define-struct tornado (scale distance max-winds))

;; A hurricane is a (make-hurricane String Natural Number)
;;     interp: represents a hurricane where
;;             name is the name of the hurricane
;;             catgeory is a number between 1 and 5, inclusive
;;             max-winds is the maximum winds sustained (mph)
;;             velocity is the velocity of the storm (mph)
;;             heading is the storm's heading
(define-struct hurricane (name category max-winds velocity heading))

;; A thunderstorm is a (make-thunderstorm String Natural Natural)
;;     interp: represents a thunderstorm where
;;             heading is the storm's heading
;;             velocity is the velocity of the storm (mph)
;;             rainfall is the number of inches of rainfall
;;             max-winds is the maximum winds sustained (mph)
(define-struct thunderstorm (heading velocity rainfall max-winds))


(define katrina(make-hurricane "katrina" 2 200 150 "NWE"))
(define tasmaniandevil(make-tornado "F5" 200 150))
(define heavydownpour(make-thunderstorm "NW" 200 12 500))

;(define (tornadofunc tornado)
;  (tornado-scale ...)
;  (tornado-distance ...)
;  (tornado-max-winds ...))

;(define(windstormdfunc windstorm)
;(cond[(tornado? windstorm)(tornadofunc windstorm)]
;     [(hurricane? windstorm)(hurricanefunc windstorm)]
;     [(thunderstorm? windstorm)(thunderstormfunc windstorm)]))

(define (violent? windstorm)
(cond[(tornado? windstorm)(cond[(or(string=?(tornado-scale windstorm)"F4")(string=?(tornado-scale windstorm)"F5"))#true]
                               [else #false])]                               
      [(hurricane? windstorm)(cond[(or(=(hurricane-category windstorm)4)(=(hurricane-category windstorm)5))#true]
                                  [else #false])]
      [(thunderstorm? windstorm)(cond[(and(>(thunderstorm-rainfall windstorm)5)(>(thunderstorm-max-winds windstorm)50))#true]
                                     [else #false])]
      [else #false]))



(violent? katrina)

(define (change-max-winds windstorm new-max-winds)
  (cond[(tornado? windstorm)(make-tornado(tornado-scale windstorm)(tornado-distance windstorm)new-max-winds)]
        [(hurricane? windstorm)(make-hurricane(hurricane-name windstorm)(hurricane-category windstorm)new-max-winds(hurricane-velocity windstorm)(hurricane-heading windstorm))]
        [(thunderstorm? windstorm)(make-thunderstorm(thunderstorm-heading windstorm)(thunderstorm-velocity windstorm)(thunderstorm-rainfall windstorm)new-max-winds)]))



(change-max-winds tasmaniandevil 30)
(change-max-winds katrina 30)
(change-max-winds heavydownpour 30)


;6 ###############################

(define los(cons "always"(cons "be"(cons "closing" empty))))
(define TestList(cons "Monkey"(cons "Ox"(cons "Narrow"(cons "Easy"(cons "Yip" empty))))))

;; ListOfNumber is one of:
; - One of 2 cases
; - atomic distinct: empty
; - Concatinate & Compound: (cons String ListOfString)
; - self-reference: (rest list) is ListOfString
; - (acrostic(rest list)


;;ListOfString -> String
;;Produces a string containing the first letter of each string in the list

(define (acrostic list)
  (cond[(empty? list) ""]
       [else (string-append(substring (first list) 0 1)
                           (acrostic(rest list)))]))

(acrostic los)
(check-expect (acrostic (cons "Monkey"(cons "Ox"(cons "Narrow"(cons "Easy"(cons "Yip" empty)))))) "MONEY")
(check-expect (acrostic '()) "")

;7 #############################





(define (ickle-strings list)
  (cond [(empty? list) empty]
        [(cons? list) (if (string-contains-ci? "ickle" (first list))
                         (cons (first list) (ickle-strings (rest list)))
                         (ickle-strings (rest list)))]))



(define lister (cons "path"(cons "sickle" empty)))
(ickle-strings lister)

;(define ListOfStrings(cons '()))


 

;8 ##############################

;#
;Write a data definition to represent a list of natural numbers (call it 
;ListOfNatural). Then develop a function lengths-of-strings that 
;consumes a ListOfString and produces a ListOfNatural. The function produces a 
;list of the lengths of each of the strings in the given ListOfString.
;(define ListOfNaturals(cons '()))

(define lon(cons 1 '()))

(define (lengths-of-strings list)
  (cond [(empty? list) 0]
        [else
        (cons (string-length (first list))
                   (lengths-of-strings (rest list)))]))

;(define lister (cons "path"(cons "sickle" empty)))

(lengths-of-strings lister)

;String -> Number
;produce the # of characters in a string

(define (getLength string list)
  (+ 1 (string-length string)))

;(define (getLength string  list)
; (cond [(empty? list) empty]
 ;       [else ... string
;
 ;        (  (rest list)))]))



;(define ListOfStrings())





