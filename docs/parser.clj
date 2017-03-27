(module core.parser-combinator
    :exports [..]
    :imports {core        [Maybe Result concept parametric
                           parametric-fn parametric-concept
                           deftype-parametric]

              core.alloc  [Allocator Block]})

(deftype

  ;; A protocol means a collection of aliases for functions that satisfy its
  ;; requirements

  ;; A cursor to iterate over data structures for the default parser combiner
  ;; library.

  {CursorLike (protocol [target]
                 ;; Tries to get the next element from the underlying stream.
                 next  (-> target -> (Maybe CursorLike))

                 ;; Returns the current value of the stream
                 value (parametric-fn [t Anything] => target -> t))


   ;; Concept to describe a cursor over a data sturcture for the predicate
   ;; parser
   CursorError (parameteric-concept [cursor (must-be CursorLike)]
                    {:cursor     cursor
                     :message    const-char*})
   })



;; Helper to define related parametric types
(deftype-parametric
  {cursor [CursorLike]
   v      []}

  {
   ;; The input for a predicate is the new current cursor.
   PredicateOutput (concept
                      :cursor cursor
                      :value  v)

   ;; The result of a predicate is the standard (^Result error ok) type
   PredicateResult (^Result
                      (^CursorError cursor)
                      (^PredicateOutput cursor v))

   ;; A predicate function must transform a cursor position to either an error
   ;; or a value and a new cursor.
   CursorPredicate  (-> ^cursor (PredicateResult cursor v))

   })


(defn indexOfChar [s c]
  (loop [i 0
         len (C/strlen s)]
    (if (< i len)

      (if (= (nth i s)) i (recur (+ i 1)))

      -1)))

(defmacro tagged-union [tags]
  (let [create-tag-struct (fn [spec]
                            (let [name (head spec)]
                            (struct)
                            )
                            )]
  (map   tags)
  ))

(deftype
  Result (parametric [^error []
                      ^ok    []]

           (tagged-union {:error ^error
                          :ok    ^ok}))
  )


(with-macros

  {#Output (PredicateOutput cursor v)
   #Error  (CursorError cursor)
   #Result (Result #Error #Output)
   }

  (parametric
    [^cursor [^CursorLike]
     ^v      []]

    nothing ([cursor ^cursor
               ->     #Result]

             (match-tag
               (CursorLike/next cursor)
               {(:just [c ^cursor]) (#ResultT :error {:cursor  c
                                                     :message "Expected nothing, got something."})

                (:nothing []) (#ResultT :ok {:cursor cursor
                                            :value  })}))





     )







