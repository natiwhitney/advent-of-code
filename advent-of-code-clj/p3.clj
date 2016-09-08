; 3

(def house-chars (drop-last (slurp "3.txt")))

(defn num-houses [house-chars]
 (loop [N 0 E 0 addresses #{"00"} rest-chars house-chars]
  (if (not (empty? rest-chars)) 
   (let [next-char (first rest-chars)
    	 N (case next-char
	    \^ (inc N)
	    \v (dec N)
	    N)
    	 E (case next-char
	    \> (inc E)
	    \< (dec E)
	    E)]
    (recur N E (conj addresses (str "N" N "E" E)) (rest rest-chars)))
    addresses)))

;; 2263
;; 2341

(defn combine [house-chars]
  (let [len (/ (count house-chars) 2)
        sc1 (num-houses (first (partition 2 house-chars)))
        sc2 (num-houses (second (partition len house-chars)))]
   (count (into sc1 sc2))))

