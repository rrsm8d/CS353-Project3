#lang racket
(require "TagValues.rkt")

; Keeping track of an accounts total purchases and payments
(define total-purchases 0.0)
(define total-payments 0.0)

; PRE: A tagged pair
; POST: Returns the index of the timestamp. Helper function
(define (get-timestamp-index list)
  (cond
    [(equal? (first list) 'purchase) 3]
    [(equal? (first list) 'payment) 4])
  )

; PRE: The starting balance of an account
; POST: Prints the totals of all transactions and balance, being the payments, purchases, and ending account balance.
(define (print-totals old-balance)
  (printf "Total Purchases: ")
  (printf (~r total-purchases #:precision '(= 2)))
  (newline)
  (printf "Total Payments: ")
  (printf (~r total-payments #:precision '(= 2)))
  (newline)
  (printf "Ending Balance: " )
  (printf (~r (+ old-balance (- total-purchases total-payments)) #:precision '(= 2)))
  (newline)
  (printf (~a "*" #:pad-string "*" #:max-width 160 #:min-width 160 #:align 'center))
  )

; PRE: A tagged pair for purchases
; POST: Prints the purchase with a specific format, updates total-purchases.
(define (print-purchase purchase)
  (let ((new-list (list (list-ref purchase 3) (list-ref purchase 1) (list-ref purchase 4) (list-ref purchase 5)))) ; Timestamp, Purchase, Merchant, amount
    (map (lambda (item) (printf (~a item #:max-width 40 #:limit-marker "..." #:min-width 20 #:align 'left))) new-list) ; Print the formatted transaction
    (newline)
    (set! total-purchases (+ total-purchases (list-ref purchase 5)))
    )
  )

; PRE: A tagged pair for cash payments
; POST: Prints the payment with a specific format, updates total payments.
(define (print-payment-cash payment)
  (let ((new-list (list (list-ref payment 4) (list-ref payment 2) (list-ref payment 5) (list-ref payment 6)))) ; Timestamp, Payment, Payment type, amount
    (map (lambda (item) (printf (~a item #:max-width 40 #:limit-marker "..." #:min-width 20 #:align 'left))) new-list) ; Print the formatted transaction
    (newline)
    (set! total-payments (+ total-payments (list-ref payment 6)))
    )
  )

; PRE: A tagged pair for credit, debit, and check payments
; POST: Prints the payment with a specific format, updates total payments.
(define (print-payment-other payment) ; TO-DO: Combine with print-payment-cash and add some condition to prevent repetition
  (let ((new-list (list (list-ref payment 4) (list-ref payment 2) (list-ref payment 5) (list-ref payment 7)))) ; Timestamp, Payment, Payment type, amount
    (map (lambda (item) (printf (~a item #:max-width 40 #:limit-marker "..." #:min-width 20 #:align 'left))) new-list) ; Print the formatted transaction
    (newline)
    (set! total-payments (+ total-payments (list-ref payment 7)))
    )
  )

; PRE: A tagged pair for any transaction
; POST: Call the appropriate print function for this transaction
(define (print-transaction transaction)
  (cond
    [(equal? (first transaction) 'purchase) (print-purchase transaction)]
    [(and (equal? (first transaction) 'payment) (equal? (second transaction) 'cash)) (print-payment-cash transaction)]
    [(and (equal? (first transaction) 'payment) (not (equal? (second transaction) 'cash))) (print-payment-other transaction)])
  )

; PRE: An account ID
; POST: Returns a list of tagged pairs, all being transactions for the given account ID
(define (all-transactions-for-account id)
  (filter (lambda (sublist)
            (or (equal? (list-ref sublist 2) id) (equal? (list-ref sublist 3) id)))
          transactions-list)
  )

; PRE: A list of tagged pairs for transactions
; POST: Returns the list now sorted by their timestamp values
(define (sorted-transactions transactions)
  (sort transactions
    (lambda (a b)
      (< (list-ref a (get-timestamp-index a)) (list-ref b (get-timestamp-index b))))) ; Different timestamp positions for purchases and payments
  )

; PRE: A tagged pair for an account
; POST: Print all transactions for this account
(define (print-account-transactions account)
  (printf "STATEMENT OF ACCOUNT\n")
  (define old-balance (list-ref account 3))
  (map (lambda (item) (printf (~a item #:max-width 40 #:limit-marker "..." #:min-width 20 #:align 'left))) (rest account)) ; Account ID, Name, Current balance
  (newline)
  (newline)
  (map print-transaction (sorted-transactions (all-transactions-for-account (second account))))
  (newline)
  (newline)
  (print-totals old-balance)
  (set! total-purchases 0.0) ; Reset balance trackers for the next account
  (set! total-payments 0.0)
  (newline)
)

; PRE: A list of tagged pairs for accounts
; POST: Prints transaction details for every account
(define (print-all-account-transactions accounts)
  (map print-account-transactions accounts)
  "Finished running. Check for an output file!"
)

; If you wish to change the output filename, change it HERE!
(with-output-to-file "TRANSACTIONS.TXT"
  (lambda () (print-all-account-transactions accounts-list))
  #:exists 'replace)

;(print-all-account-transactions accounts-list)