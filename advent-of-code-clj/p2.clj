; 2 
; this defines input
(load-file "2.clj")

(def lines (clojure.string/split input #"\n"))

(defn parse-line [line]
 (let [s-dims (clojure.string/split line #"x")]
  (map #(Integer/parseInt %) s-dims)))

(defn volume [d]
  (apply * d))

(defn perm [v]
  (cons (last v) (drop-last v)))

(defn perimeters [d]
  (map (fn [d1 d2] (* 2 (+ d1 d2))) d (perm d)))
 
(defn size-ribbons [d]
  (let [bow (volume d)
        ribbon (apply min (perimeters d))]
   (+ bow ribbon)))

(defn sum-ribbons [line]
  (let [d (parse-line line)]
    (size-ribbons d)))

(reduce + (map sum-ribbons lines))

