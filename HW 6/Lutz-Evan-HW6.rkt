;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Lutz-Evan-HW6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;ewlutz
;Evan Lutz

;1

(define-struct user (username mailbox))
;(make-user(STRING ListOfMessages))
;username is the user's usersname
;mailbox is a ListOfMessages


(define-struct message(username text read?))
;(make-message(STRING STRING BOOLEAN))
;username is the senders username
;text is the message
;read? indicated wether it was read


(define lutz(make-user "ewlutz" empty))
  
(define marsh(make-user "brmarsh" (list (make-message "ewlutz" "lmao" true))))

(define rex(make-user "rexdaisy2003" empty))

(define diwalk(make-user "diwalk" empty))




;; ListOfUser is one of:
; - empty
; - ListOfUser

;; ListOfMessage is one of:
; - empty
; - ListOfMessages


;2

;(define mailsys empty)
(define mailsys (list lutz marsh rex diwalk))
(define newuser(make-user "Newuser" empty))



;3
;STRING -> VOID
;add a new user with the given username to the mail system
;(define (add-new-user username))

(define (add-new-user username)
  (set! mailsys (cons (make-user username empty) mailsys))
  )


;4
;STRING STRING STRING -> VOID
;Name of sender, Name of recipiant, and text
;stores a unread message in the recipents mailbox

(define (findUser lou recipient)
  (first (filter (local[(define (isUser? user1) (if (string=? (user-username user1) recipient)
    true false))]isUser? ) lou )))

(define (addMessage message recipient)
  (set-user-mailbox! (findUser mailsys recipient) (cons message (user-mailbox  (findUser mailsys recipient)))))

;(findUser mailsys "ewlutz")
;(addMessage "sheesh" "ewlutz")

 
(define (send-email-message sender recipient text)
   (addMessage(make-message (user-username sender) text false) recipient))

(send-email-message diwalk "ewlutz" "u up lol?")
;(send-email-message marsh "ewlutz" "kaboom")
;(user-mailbox lutz)

;5
;STRING -> lom
;username, to ListOfMessages

(define (get-all-unread-messages ign)
  (filter (local[(define (unread? message)
                   (not (message-read? message)))] unread?)
          (user-mailbox (findUser mailsys ign))))


(get-all-unread-messages "ewlutz")
(get-all-unread-messages "brmarsh")



;6
;VOID -> user

(define (most-total-messages)
  (map (local [(define (numbOfMessages user)
                 (length (user-mailbox user)))] numbOfMessages)
       mailsys))

(foldr > 0 )

(define ())








