; 4 

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

	(defn mining [secret-key]
	 (loop [num 0]
	  (if (= "000000" (subs (md5 (str secret-key num)) 0 6))
	   num
	   (recur (inc num)))))

