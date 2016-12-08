(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn md5 
  ([s] (md5 s ""))
  ([s n]
  (let [s (str s n)
        algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig))))

(def positions (atom (set (map str (range 0 8)))))

(defn valid-md5? [s]
  (= "00000" (subs s 0 5)))

(defn valid-pos? [s]
  (let [i (subs s 5 6)
        valid-i? (@positions i)
        _ (when valid-i? 
            (do
              (pprint (str "valid position:" i))
              (swap! positions disj i)))]
   (if valid-i? true false)))

(def indexes (atom (list)))

(defn get-num-indexed 
 ([s num]
  (get-num-indexed s num valid-pos?))
 ([s num filter-fn]
 (let [lazy-md5 (map (partial md5 s) (range))]
     (->> lazy-md5
          (filter valid-md5?)
          (filter filter-fn)
          (take num)))))

(defn valid-md5-to-pair [s]
  (let [c (subs s 6 7)
        i (subs s 5 6)]
  [i c]))
  
(defn index-pairs-to-string [pairs]
  (->> pairs
       (into (sorted-map))
       (vals)
       (apply str)))

(def res 
  (->> (get-num-indexed "ojvtpuvg" 8)
       (map valid-md5-to-pair)
       index-pairs-to-string))



