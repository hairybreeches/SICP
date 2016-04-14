(ns sicp.chapter-4.language
  (:use sicp.chapter-4.amb-code)
  (:use sicp.chapter-4.sequences)
  (:use sicp.chapter-4.interpreter.repl))

(defn generate-sentence []
  (execute
    require-code
    member?
    an-element-of
    '(define nouns '(noun student professor cat class))

    '(define verbs '(verb studies lectures eats sleeps))

    '(define articles '(article the a))

    '(define prepositions '(prep for to in by with))

    '(define (random-element things)
       (require (not (null? things)))
       (ramb (car things) (random-element (cdr things))))

    '(define (parse-word word-list)
       (list
         (car word-list)
         (random-element (cdr word-list))))

    '(define (parse-simple-noun-phrase)
      (list 'simple-noun-phrase
            (parse-word articles)
            (parse-word nouns)))

    '(define (parse-prepositional-phrase)
       (list 'prep-phrase
             (parse-word prepositions)
             (parse-noun-phrase)))

    '(define (parse-noun-phrase)
       (define (maybe-extend noun-phrase)
         (ramb noun-phrase
              (maybe-extend
                (list 'noun-phrase
                      noun-phrase
                      (parse-prepositional-phrase)))))
       (maybe-extend (parse-simple-noun-phrase)))

    '(define (parse-verb-phrase)
       (define (maybe-extend verb-phrase)
         (ramb verb-phrase
              (maybe-extend
                (list
                  'verb-phrase
                  verb-phrase
                  (parse-prepositional-phrase)))))
       (maybe-extend (parse-word verbs)))

    '(define (generate-sentence)
      (list 'sentence
            (parse-noun-phrase)
            (parse-verb-phrase)))

    (list 'generate-sentence)))




