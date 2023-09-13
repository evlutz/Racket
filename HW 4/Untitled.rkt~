;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
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


;; ListOfUser is one of:
; - empty
; - ListOfUser

;; ListOfMessage is one of:
; - empty
; - ListOfMessages


;2

(define mailsys empty)
(define newuser(make-user "Newuser" empty))



;3
;STRING -> VOID
;add a new user with the given username to the mail system
;(define (add-new-user username))

(define (add-new-user username)
  (set! mailsys (cons (make-user username empty) mailsys))
  )


;4

;stores a unread message in the recipents mailbox
(define (send-email-message sender recipient text)
  (set! user-mailbox-recipient (cons (make-message sender text false) user-recipient-mailbox)))






