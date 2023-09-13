;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lutz-Evan-HW3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #t)))
;HW 3
;ewlutz
;Evan Lutz
;brmarsh
;Ben Marsh

;1
(define-struct Merchandise(name kind autographed? quantity price))
;(define-struct Merchandise(STRING STRING BOOLEAN NATURAL NATURAL))


;name -> name of the various item sold,\
;kind -> (action figure, board game, costume, manga/comic book, trading card, etc.)
;autograph? -> whether or not the item is autographed?
;quantity -> quantity of items sold
;price -> price of a single item
(define NETWORTH 200)


(define Home(make-Merchandise "Marc New York" "Jacket" false 1 100))
(define Shoes(make-Merchandise "Nike Air Max's" "Shoe" false 3 119))
(define Woopi(make-Merchandise "WPI Superfan" "Shirt" true 100 70))
(define Patriots(make-Merchandise "Patriots" "Shirt" true 250 105))
(define Redsox(make-Merchandise "David Ortiz" "Jersey" true 200 30))
(define MagicTheGathering(make-Merchandise "Cards" "trading-cards" false 200 30))
(define MagicTheGathering2(make-Merchandise "Cards" "trading-cards" false 250 30))
(define Sorry(make-Merchandise "Sorry" "board-game" false 250 20))
(define Ronald(make-Merchandise "Ronald McDonald" "costume" false 10 100))

;2
;(define  (merchdfunc merch)
;  (merch-name...) -> STRING
;  (merch-kind...) -> STRING
;  (merch-autographed?...) -> BOOLEAN
;  (merch-quantity...) -> NUMBER
;  (merch-price...) -> NUMBER


;3
; Receipt is a ListOfMerchandise
;Produce true if list includes all Merchandise
;Produce false if list does not include all Merchandise


(define Nordstrom(cons Home(cons Shoes (cons Woopi empty))))
(define BookStore(cons Home (cons Sorry (cons Sorry(cons Woopi empty)))))
(define Dicks(cons Woopi(cons Patriots (cons MagicTheGathering (cons MagicTheGathering2(cons Redsox empty))))))
(define Wicked(cons Home (cons Ronald (cons Sorry (cons Sorry(cons Woopi empty))))))
(define Spirit(cons Home (cons Ronald empty)))
(define TradingCards(cons MagicTheGathering(cons MagicTheGathering empty)))
(define alom(list Ronald))


;4
;(define  (fn-for-lom lom)
;  (cond [(empty? lom) empty]
;        [else
;         (...#combination ...(first lom)
;                       (fn-for-lom (rest lom)))]))



;Merchandise -> BOOLEAN
;if the merch is autographed return true
;(define (autographed? merch)
;  (if (merch(Merchandise-autographed?)) true false))


;5

;lom -> lom
;(define (list-cheap-autograph Recipt NUMBER)
;takes in a recipt and a threshold # return a recipt with all Merchandise signed and < max

(define (list-cheap-autograph lom max)
  (cond [(empty? lom) empty]
        [(cons? lom)
         (if (belowNgraphed? (first lom) max)
                         (cons (first lom) (list-cheap-autograph (rest lom) max))
                         (list-cheap-autograph (rest lom) max))]))

; Merchandise max -> BOOLEAN
;Takes in a murchandise and decides if the Merchandise is autographed & price is below a set point
(define (belowNgraphed? merch max)
  (if (and (Merchandise-autographed? merch) (<= (Merchandise-price merch) max))
      true false))

(check-expect (list-cheap-autograph Dicks 350) (cons Woopi (cons Patriots (cons Redsox empty))))
(check-expect (list-cheap-autograph Dicks 100) (cons Woopi ( cons Redsox empty)))
(check-expect (list-cheap-autograph Dicks 50) (cons Redsox empty))
(check-expect (list-cheap-autograph Spirit 2) empty)

;6 
;lom -> NUMBER
;Takes in a receipt and returns the total number of items in the order that are trading cards taking into account the quantity

(define (count-trading-cards lom)
  (cond [(empty? lom) 0]
        [(cons? lom)
         (if (costume?(first lom))
                         (+ (Merchandise-quantity(first lom)) (count-trading-cards (rest lom)))
                         (count-trading-cards (rest lom)))]))


;Merchandise -> BOOLEAN
;takes in a merchandise in return true if merchandise is a "trading-card"
(define (costume? merch)
  (string=? (Merchandise-kind merch) "trading-cards"))

(check-expect (count-trading-cards Dicks) 450)
(check-expect (count-trading-cards TradingCards) 400)
 

;7
;lom -> NUMBER
;consumes a receipt and produces the total cost of all the merchandise items
(define (receipt-total lom)
  (cond [(empty? lom) 0]
        [(cons? lom)
          (+ (total(first lom)) 
                         (receipt-total (rest lom)))]))

;Merchandise -> NUMBER
;consumes a merchandise and calculates the total cost of the item taking quantity into account
(define (total merch)
  (* (Merchandise-price merch)(Merchandise-quantity merch)))


(check-expect(receipt-total Nordstrom) 7457)
(check-expect(receipt-total Spirit) 1100)
(check-expect(receipt-total empty) 0)


;8
;lom -> NUMBER
;consumes calculates the total cost of all the board games contained in the receipt.
(define (board-games-cost lom)
  (cond [(empty? lom) 0]
        [(cons? lom)
         (if (string=? (Merchandise-kind(first lom)) "board-game")
                         (+ (total(first lom)) (board-games-cost (rest lom)))
                         (board-games-cost (rest lom)))]))

(check-expect(board-games-cost Wicked) 10000)
(check-expect(board-games-cost Dicks) 0)


;9 
;lom -> NUMBER
;function  consumes a receipt and a number to discount produces the total cost of the receipt, with the discount applied only to costume merchandise
(define (halloween-sale lom discount)
  (cond [(empty? lom) 0]
        [(string=? "costume" (Merchandise-kind(first lom)))
              (+ (* (total(first lom)) discount) (halloween-sale(rest lom) discount))]
          [(cons? lom)
                 (+ (* (Merchandise-price(first lom))(Merchandise-quantity(first lom))) 
                         (halloween-sale (rest lom) discount))]))
 

(check-expect (halloween-sale Spirit .25) (+ 100 (* 10 25)))
(check-expect (halloween-sale alom .25) 250)
(check-expect (halloween-sale Dicks .25) (receipt-total Dicks))


















