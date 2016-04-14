(ns sicp.chapter-4.language
  (:use sicp.chapter-4.amb-code)
  (:use sicp.chapter-4.sequences)
  (:use sicp.chapter-4.interpreter.repl))

(defn parse [sentence]
  (get-all-results
    require-code
    member?

    '(define nouns '(noun student professor cat class))

    '(define verbs '(verb studies lectures eats sleeps))

    '(define articles '(article the a))

    '(define prepositions '(prep for to in by with))

    '(define adjectives '(adjective red small complex))

    '(define (parse-word word-list)
      (require (not (null? *unparsed*)))
      (require (member? (cdr word-list) (car *unparsed*)))
      (let ((found-word (car *unparsed*)))
        (set! *unparsed* (cdr *unparsed*))
        (list (car word-list) found-word)))

    '(define (parse-descriptor-sequence)
       (define (maybe-extend descriptor-sequence)
         (amb descriptor-sequence
              (maybe-extend
                (list 'descriptor-sequence
                      descriptor-sequence
                      (parse-word adjectives)))))

       (maybe-extend (parse-word articles)))

    '(define (parse-simple-noun-phrase)
      (list 'simple-noun-phrase
            (parse-descriptor-sequence)
            (parse-word nouns)))

    '(define (parse-prepositional-phrase)
       (list 'prep-phrase
             (parse-word prepositions)
             (parse-noun-phrase)))

    '(define (parse-noun-phrase)
       (define (maybe-extend noun-phrase)
         (amb noun-phrase
              (maybe-extend
                (list 'noun-phrase
                      noun-phrase
                      (parse-prepositional-phrase)))))
       (maybe-extend (parse-simple-noun-phrase)))

    '(define (parse-verb-phrase)
       (define (maybe-extend verb-phrase)
         (amb verb-phrase
              (maybe-extend
                (list
                  'verb-phrase
                  verb-phrase
                  (parse-prepositional-phrase)))))
       (maybe-extend (parse-word verbs)))

    '(define (parse-sentence)
      (list 'sentence
            (parse-noun-phrase)
            (parse-verb-phrase)))


    '(define *unparsed* '())

    '(define (parse input)
       (set! *unparsed* input)
       (let ((sent (parse-sentence)))
         (require (null? *unparsed*))
         sent))

    (list 'parse sentence)))




