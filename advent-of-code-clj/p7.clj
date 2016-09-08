(def input
  (let [rdr (clojure.java.io/reader "Interviews/7.txt")]
   (line-seq rdr)))

(def AND +)
(def NOT bit-not)
(def a [b])
(def b [AND d 2])
(def c [NOT 1])
(def d [3])
(def e [d])

(declare gv ev)
(defmacro gv
  "Symbol lookup. If symbol points to a collection,
   ev the collection and return. Otherwise, return 
   what it points to."
   [x]
   `(if (coll? (eval ~x)) (do (prn "macro says coll") (first (flatten (eval ~x)))) (do (prn "macro says not coll") (ev ~x))))

(defn ev
 "Given a single element, evaluate it and return.
  Given a collection, evalate the first symbol 
  and return if it is a 1-element collection.
  Otherwise apply symbol the rest of the evaluated
  collection."
  [x]
  (cond 
   (coll? x)
      (if (= 1 (count x))
          (do (prn "single element collection with:" x) (ev (nth x 0)))
          (do (prn "multi element collection with:" x) (apply (nth x 0)) (map ev (rest x))))
   (symbol? x) (do (prn "symbol: " x) (gv x))
   (number? x) (do (prn "number: " x) x)))

