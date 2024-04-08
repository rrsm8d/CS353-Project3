#lang racket
(require "FileReader.rkt")

; Type tags (Probably doesn't need to be defined?)
(define user-account 'account)
(define user-purchase 'purchase)
(define user-payment 'payment)
(define user-payment-cash 'cash)
(define user-payment-check 'check)
(define user-payment-credit 'credit)
(define user-payment-debit 'debit)
(define user-transaction 'transaction) ; failsafe

; PRE: A specified tag and list of values
; POST: Returns a pair containing the type-tag and list of values
(define (attach-tag tag values)
  (cons tag values)
  )

; PRE: A tag and list of items to tag it with
; POST: Attach a tag to every item in the list
(define (attach-tag-to-list tag list)
  (map
   (lambda (sublist)
     (attach-tag tag sublist))
   list)
  )

#|
 Partially generated with claude-3-sonnet. PROMPT: 
 Assume this is for the racket programming language. Given the string "123 \"John doe\" 456.789"
 How would you convert the string into a list, where the name in quotes is a single item not delimited by the space between it?
 Try to achieve this output list: '(123, "John doe", 456.789)
 If possible, try to keep your output short by using a regular expression.
|#

; PRE: A string to parse
; POST: Return the string as a list of strings. Delimited by spaces and tabs, unless within quotes
(define (parse-string input-str)
  (regexp-match* #rx"\"([^\"]*)\"|([^ \t\"]+)" input-str) ; OLD REGEX: #rx"[^ \"]+|\"[^\"]*\""
  )
#|
EXPLANATION: 
\"([^\"]*)\" Matches the double quotes, for names and such.
| to have an alternate pattern
([^ \t\"]+) Matches non-whitespace, non-tab characters. The + is to account for the weird inconsistent formatting in TRANSACTIONS.TXT
|#


; PRE: A string, representing a line read from ACCOUNTS.TXT
; POST: Returns a list, with that line now seperated into individual elements.
(define (seperate-values line)
  (define new-list (parse-string line)) ; TO-DO: Change this to a let.

  ; Entries with quotes, such as "John Doe", are left looking like "\"John doe\"" after the regexp filter
  ; This will clean the entries, and convert strings with numerical values into integers or floats.
  (define cleaned-new-list
    (map
     (lambda (value)
           (if (string->number value)
               (string->number value)
               (string-trim value "\"")))
       new-list)
    )
  
  cleaned-new-list
  )

; PRE: A list of strings, representing all lines from ACCOUNTS.TXT
; POST: Return a new list of lists, those sublists containing values (int, string, float)
(define (clean-values-list lines-list)
  (map seperate-values lines-list)
  )

; PRE: a filename
; POST: reads from the file, creates a list of lists, attach tag to sublists.
(define (read-and-tag-accounts filename)
  (let ((clean-list (clean-values-list (read-file filename))))
    (attach-tag-to-list user-account clean-list))
  )

; PRE: a filename
; POST: reads from the file, creates a list of lists, attach MULTIPLE tags to sublists.
(define (read-and-tag-transactions filename)
  (let ((clean-list (clean-values-list (read-file filename))))
      (map (lambda (sublist)
             (cond ; Apply tags for purchases and payments
               [(equal? (first sublist) "Purchase") (attach-tag user-purchase sublist)]
               [(equal? (first sublist) "Payment") (cond ; Apply additional tags for specific payment types.
                                                     [(equal? (fourth sublist) "Cash") (attach-tag user-payment (attach-tag user-payment-cash sublist))]
                                                     [(equal? (fourth sublist) "Check") (attach-tag user-payment (attach-tag user-payment-check sublist))]
                                                     [(equal? (fourth sublist) "Credit") (attach-tag user-payment (attach-tag user-payment-credit sublist))]
                                                     [(equal? (fourth sublist) "Debit") (attach-tag user-payment (attach-tag user-payment-debit sublist))])]
               [else (attach-tag user-transaction sublist)] ; Should never happen, but added as a safety
               ))
           clean-list))
  )

; If you wish to test with different input files, change the filenames HERE!
(define accounts-list (read-and-tag-accounts "ACCOUNTS.TXT"))
(define transactions-list (read-and-tag-transactions "TRANSACTIONS.TXT"))
(provide accounts-list)
(provide transactions-list)