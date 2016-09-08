; 1

(def floor (atom 0))
(def chars (drop-last (for [nxt (slurp "1.txt")] nxt)))

    (loop [count 0 index 0 rest-chars chars]
      (let [next-char (first rest-chars)
            delta (if (= \( next-char) 1 -1)
            next-count (+ count delta)]
        (if-not (zero? @floor)
          @floor
          (do 
            (when (= -1 next-count) (reset! floor index))
            (recur next-count (inc index) (rest rest-chars))))))


