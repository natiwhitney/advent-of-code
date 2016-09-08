; 6
(def init-grid (make-array Boolean/TYPE 1000 1000))
(def init-grid-1 (make-array Short/TYPE 1000 1000))

(def instructions (line-seq (clojure.java.io/reader "6.txt")))
(def Winstructions (line-seq (clojure.java.io/reader "lights.txt")))

(defn re-pos [re s]
        (loop [m (re-matcher re s)
               res {}]
          (if (.find m)
            (recur m (assoc res (.start m) (.group m)))
            res)))

(defn parse-number-pairs [instruction]
  (let [pairs (re-seq #"\d+,\d+" instruction)
        x-start (Integer/parseInt (first (clojure.string/split (first pairs) #",")))
        y-start (Integer/parseInt (second (clojure.string/split (first pairs) #",")))
        x-end (Integer/parseInt (first (clojure.string/split (second pairs) #",")))
        y-end (Integer/parseInt (second (clojure.string/split (second pairs) #",")))]
  [x-start x-end y-start y-end]))

(defn update-xy [grid x y instruction]
  (cond
    (.startsWith instruction "turn on") (aset-boolean grid x y true)
    (.startsWith instruction "turn off") (aset-boolean grid x y false) 
    (.startsWith instruction "toggle") (let [current (aget grid x y)] (aset-boolean grid x y (not current)))))

(defn brighten-xy [grid x y instruction]
  (cond
    (.startsWith instruction "turn on") (aset-short grid x y (inc (aget grid x y)))
    (.startsWith instruction "turn off") (aset-short grid x y (dec (aget grid x y)))
    (.startsWith instruction "toggle") (aset-short grid x y (+ 2 (aget grid x y))))) 
   
(defn parse-instruction [instruction]
  (let [[x1 x2 y1 y2] (parse-number-pairs instruction)
        x-range (range x1 (+ x2 1))
        y-range (range y1 (+ y2 1))]
    (doseq [x x-range y y-range]
      (update-xy init-grid x y instruction))))

(def count-on (atom 0))

(defn count-grid-on []
  (doseq [x (range 0 1000) y (range 0 1000)]
    (when (aget init-grid x y) (swap! count-on inc))))




































