; 5 

(def strings (line-seq (clojure.java.io/reader "5b.txt")))

(defn three-vowels? [string]
  (let [vs (reduce (fn [a n] (if (#{\a \e \i \o \u} n) (inc a) a)) 0 string)]
    (>= vs 3)))

(defn double-letter? [string]
  (let [dl (reduce (fn [[b l] n] (if (= n l) [true n] [(and b true) n])) [false (first string)] (rest string))]
    (first dl)))

(defn not-has? [string pair]
  (empty? (re-find (re-pattern pair) string)))

(defn pair-safe? [string]
  (and (not-has? string "ab") 
       (not-has? string "cd")
       (not-has? string "pq")
       (not-has? string "xy")))

(defn nice? [string]
  (and (three-vowels? string)
       (double-letter? string)
       (pair-safe? string)))

(reduce (fn [a n] (if (= true (nice? n)) (inc a) a)) 0 strings)

	(defn pair-pair-letters? [string]
	 (let [pair-inds (loop [ind 0 pairs {} char-set (partition 2 1 string)]
	   (if (empty? char-set) pairs
	    (let [pair (first char-set)
	          pairs (update pairs pair (fn [prev-ind] (conj prev-ind ind)))]
              (recur (inc ind) pairs (rest char-set)))))]
          (some true? (map (fn [[_ inds]] (and (< 1 (count inds)) (< 1 (- (apply max inds) (apply min inds))))) pair-inds))))

(defn sandwhich? [string]
  (let [parts (partition 3 1 string)
        s (map (fn [n] (= (first n) (last n))) parts)]
   (some true? s)))

(defn nice2? [string]
  (and (pair-pair-letters? string)
       (sandwhich? string)))

(reduce (fn [a n] (if (= true (nice2? n)) (inc a) a)) 0 strings)
 
