#lang racket

; Define the Either monad
(define (Left x) (cons 'Left x))
(define (Right x) (cons 'Right x))
(define (is-Left x) (eq? (car x) 'Left))
(define (is-Right x) (eq? (car x) 'Right))
(define (from-Left x) (cdr x))
(define (from-Right x) (cdr x))

(define (string-index str char)
  (let ([match (regexp-match-positions (regexp-quote char) str)])
    (if match (caar match) #f)))

(define (butlast lst)
  (reverse (cdr (reverse lst))))


; Define a helper function to match regex patterns
(define (match-regex pattern str)
  (if (regexp-match pattern str) (Right str) (Left "Syntax error")))

; Define parsing functions for non-terminals
(define (parse-id str)
  (match-regex #rx"^[a-zA-Z][a-zA-Z0-9]*" str))

(define (parse-num str)
  (match-regex #rx"^[+-]?[0-9]+" str))

; Define helper function to check if a string starts with a given prefix
(define starts-with? (lambda (str prefix) (string-prefix? str prefix)))

; Define helper function to remove a prefix from a string
(define (remove-prefix str prefix)
  (substring str (string-length prefix)))

; Define a helper function to check if a string is a reserved word
(define (is-reserved-word? str)
  (member str '("if" "while" "goto" "gosub" "return" "break" "end")))

; Define parsing functions for non-terminals
(define (parse-etail str)
  (cond
    [(starts-with? str "+") (parse-expr (remove-prefix str "+"))]
    [(starts-with? str "-") (parse-expr (remove-prefix str "-"))]
    [(starts-with? str "*") (parse-expr (remove-prefix str "*"))]
    [(starts-with? str "/") (parse-expr (remove-prefix str "/"))]
    [else (Right str)])) ; epsilon case, no operation to perform

(define (parse-expr str)
  (cond
    [(or (is-Right (parse-id str)) (is-Right (parse-num str)))
     (let ([end (or (string-index str " ") (string-length str))])
       (parse-etail (substring str end)))]
    [(and (starts-with? str "(") (string-suffix? str ")"))
     (parse-expr (substring str 1 (- (string-length str) 1)))]
    [else (Left "Syntax error in expr")]))

(define (parse-boolean str)
  (cond
    [(starts-with? str "true") (Right (remove-prefix str "true"))]
    [(starts-with? str "false") (Right (remove-prefix str "false"))]
    [(is-Right (parse-expr str))
     (let ([remaining (from-Right (parse-expr str))])
       (cond
         [(starts-with? remaining "=") (parse-expr (remove-prefix remaining "="))]
         [else (Left "Syntax error in boolean")])
     )]
    [else (Left "Syntax error in boolean")]))

(define (parse-stmt str)
  (cond
    [(starts-with? str "if (")
     ; Handle if statement
     (let* ([bool-end (string-index str ")")]
            [bool-str (substring str 4 bool-end)]
            [bool-result (parse-boolean bool-str)]
            [remaining (substring str (+ bool-end 1))])
       (if (is-Right bool-result)
           (parse-stmt remaining)
           (Left "Syntax error in if statement")))]
    [(starts-with? str "while (")
     ; Handle while statement
     (let* ([bool-end (string-index str ")")]
            [bool-str (substring str 7 bool-end)]
            [bool-result (parse-boolean bool-str)]
            [remaining (substring str (+ bool-end 1))])
       (if (is-Right bool-result)
           (let ([body-result (parse-linelist (remove-prefix (from-Right bool-result) ")"))])
             (if (is-Right body-result)
                 (if (starts-with? (from-Right body-result) "endwhile")
                     (Right (remove-prefix (from-Right body-result) "endwhile"))
                     (Left "Missing endwhile"))
                 body-result))
           (Left "Syntax error in while statement")))]
    [(starts-with? str "id =")
     ; Handle assignment statement
     (let ([id-str (substring str 0 (string-index str " "))])
       (if (not (is-reserved-word? id-str))
           (parse-expr (remove-prefix str "id ="))
           (Left "Syntax error: Reserved word used as label")))]
    [(starts-with? str "read ")
     ; Handle read statement
     (let ([id-result (parse-id (remove-prefix str "read "))])
       (if (is-Right id-result)
           (Right (remove-prefix (from-Right id-result) "id;"))
           (Left "Syntax error in read statement")))]
    [(starts-with? str "write ")
     ; Handle write statement
     (let ([expr-result (parse-expr (remove-prefix str "write "))])
       (if (is-Right expr-result)
           (Right (remove-prefix (from-Right expr-result) "expr;"))
           (Left "Syntax error in write statement")))]
    [(starts-with? str "goto ")
     ; Handle goto statement
     (let ([id-result (parse-id (remove-prefix str "goto "))])
       (if (is-Right id-result)
           (Right (remove-prefix (from-Right id-result) "id;"))
           (Left "Syntax error in goto statement")))]
    [(starts-with? str "gosub ")
     ; Handle gosub statement
     (let ([id-result (parse-id (remove-prefix str "gosub "))])
       (if (is-Right id-result)
           (Right (remove-prefix (from-Right id-result) "id;"))
           (Left "Syntax error in gosub statement")))]
    [(starts-with? str "return")
     (Right (remove-prefix str "return;"))]
    [(starts-with? str "break")
     (Right (remove-prefix str "break;"))]
    [(starts-with? str "end")
     (Right (remove-prefix str "end;"))]
    [else (Left "Syntax error in stmt")]))
    
(define (parse-line str)
  (cond
    [(and (starts-with? str "id:") (not (is-reserved-word? (substring str 0 (string-index str ":"))))
         (let ([parsed-str (remove-prefix str "id:")])
           (parse-linetail parsed-str)))]
    [else (parse-stmt str)]))
      
 ; Handling linetail
(define (parse-linetail str)
  (let loop ([remaining str])
    (cond
      [(string=? remaining "") (Right remaining)]
      [(is-Right (parse-stmt remaining))
       (loop (from-Right (parse-stmt remaining)))]
      [else (Left "Syntax error in linetail")])))

(define (parse-linelist lines line-number)
  (if (null? lines)
      (Right '())
      (let ([result (parse-line (car lines))])
        (if (is-Right result)
            (parse-linelist (cdr lines) (+ line-number 1))
            (Left (format "Syntax error on line ~a" line-number))))))

(define (parse-program lines)
  (cond
    [(and (not (null? lines)) (string=? (car (reverse lines)) "$$"))
     (parse-linelist (butlast lines) 1)]
    [else (Left "Program does not end with $$")]))

(define (parse filename)
  (let ([lines (file->lines filename)])
    (if (is-Right (parse-program lines))
        "Accept"
        (from-Left (parse-program lines)))))

; Test
(display (parse "code1.txt"))