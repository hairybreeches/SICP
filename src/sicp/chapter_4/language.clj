(ns sicp.chapter-4.language
  (:use sicp.chapter-4.amb-code)
  (:use sicp.chapter-4.interpreter.repl))

(defn parse [sentence]
  (get-all-results
    require-code
    member?

    '(define nouns '(noun student professor cat class))

    '(define verbs '(verb studies lectures eats sleeps))

    '(define articles '(article the a))

    '(define (parse-word word-list)
      (require (not (null? *unparsed*)))
      (require (member? (cdr word-list) (car *unparsed*)))
      (let ((found-word (car *unparsed*)))
        (set! *unparsed* (cdr *unparsed*))
        (list (car word-list) found-word)))

    '(define (parse-noun-phrase)
      (list 'noun-phrase
            (parse-word articles)
            (parse-word nouns)))

    '(define (parse-sentence)
      (list 'sentence
            (parse-noun-phrase)
            (parse-word verbs)))

    '(define *unparsed* '())

    '(define (parse input)
       (set! *unparsed* input)
       (let ((sent (parse-sentence)))
         (require (null? *unparsed*))
         sent))

    (list 'parse sentence)))




