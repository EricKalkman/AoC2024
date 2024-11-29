(ns aoc2024.parsecomb
  (:require [clojure.set :as set]))

;; The naming conventions are a bit all over the place. May change in the future

(defn error? [result] (contains? result :error))
(defn success? [result] (contains? result :rest))

(defn return-result [x]
  (fn [inp] {:result x :rest inp}))

(defn string->stringbuf [s] {:str s :pos 0})

(defn adv-buf [n buf] (update buf :pos (partial + n)))
(defn inc-buf [buf] (update buf :pos inc))

(defn buf-peek [{:keys [str pos]}] (get str pos))
(defn buf-take [n {:keys [str pos]}] (subs str pos (+ n pos)))

(defn buf-eof? [{:keys [str pos]}] (>= pos (count str)))
(defn buf-remaining [{:keys [str pos]}] (- (count str) pos))

(defn chr [c]
  (fn [{:keys [str pos] :as buf}]
    (cond
      (buf-eof? buf) {:error :eof :input buf}
      (= (get str pos) c) {:result c :rest (inc-buf buf)}
      :else {:error :wrong-char :expected c :input buf})))

(defn string [s]
  (fn [buf]
    (cond
      (< (buf-remaining buf) (count s)) {:error :eof :input buf}
      (= (buf-take (count s) buf) s) {:result s :rest (adv-buf (count s) buf)}
      :else {:error :wrong-string :expected s :input buf})))

(defn charset [st]
  (fn [buf]
    (cond
      (buf-eof? buf) {:error :eof :input buf}
      (st (buf-peek buf)) {:result (buf-peek buf) :rest (inc-buf buf)}
      :else {:error :wrong-char :expected st :input buf})))

(defn any-char [buf]
  (if (buf-eof? buf)
    {:error :eof :input buf}
    {:result (buf-peek buf) :rest (inc-buf buf)}))

(defn charset+
  [st]
  (fn [{:keys [str pos] :as buf}]
    (cond
      (buf-eof? buf) {:error :eof :input buf}
      (not (st (buf-peek buf))) {:error :wrong-char :expected st :input buf}
      :else
      (loop [idx (inc pos)]
        (if (or (>= idx (count str)) (not (st (get str idx))))
          {:result (buf-take (- idx pos) buf) :rest (adv-buf (- idx pos) buf)}
          (recur (inc idx)))))))

(def DIGITS #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(def UCASE (into #{} "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def LCASE (into #{} "abcdefghijklmnopqrstuvwxyz"))
(def LETTERS (set/union UCASE LCASE))
(def ALPHANUMERIC (set/union LETTERS DIGITS))
(def WHITESPACE #{\space \newline \return \tab})

(defn p-seq [& ps]
  (fn [inp]
    (loop [ps ps
           result []
           inp inp]
      (if (empty? ps)
        (condp = (count result)
          0 {:rest inp}
          1 {:result (first result) :rest inp} ; flatten result if it only has one element
          {:result result :rest inp})
        (let [res ((first ps) inp)]
          (if (error? res)
            res
            (recur (rest ps)
                   (if (:result res) (conj result (:result res)) result)
                   (:rest res))))))))

(defn skip [p]
  (fn [inp]
    (let [res (p inp)]
      (if (error? res)
        res
        (dissoc res :result)))))

(defn maybe
  "Does not contain a :result if the parser fails"
  [p]
  (fn [inp]
    (let [res (p inp)]
      (if (error? res)
        {:rest inp}
        res))))

(defn maybe-missing
  ":result is :missing if the parser fails"
  [p]
  (fn [inp]
    (let [res (p inp)]
      (if (error? res)
        {:result :missing :rest inp}
        res))))

(defn p-map [f p]
  (fn [inp]
    (let [res (p inp)]
      (if (error? res)
        res
        (update res :result f)))))

(defn p-repeat [n p]
  (fn [inp]
    (loop [inp inp
           n n
           res []]
      (if (zero? n)
        {:result res :rest inp}
      (let [r (p inp)]
        (if (error? r)
          r
          (recur (:rest r) (dec n) (conj res (:result r)))))))))

(defn some* [p]
  (fn [inp]
    (loop [inp inp
           res []]
      (let [r (p inp)]
        (if (error? r)
          {:result res :rest inp}
          (recur (:rest r) (if (:result r) (conj res (:result r)) res)))))))

(defn some+ [p]
  (->> (p-seq p
              (some* p))
       (p-map #(apply cons %))))

(defn inspect [inp]
  (print "Inspect: ")
  (prn inp)
  {:rest inp})

(defn p-or [& ps]
  (fn [inp]
    (reduce #(let [res (%2 inp)]
               (if (error? res)
                 %1
                 (reduced res)))
            {:error :no-successful-parsers :input inp}
            ps)))

(defn list-+ [elem sep &{:keys [vectorize?]}]
  (->>
    (p-seq elem
           (some* (p-seq (skip sep)
                         elem)))
    (p-map #(if vectorize?
              (into [(first %)] (second %))
              (apply cons %)))))

(defn list-* [elem sep]
  (p-or (list-+ elem sep)
        (return-result [])))

(defmacro rec
  "(rec name parser) allows parser to call itself recursively by the name name"
  {:clj-kondo/ignore [:unresolved-symbol]}
  [r p]
  `(letfn [(~r [inp#] (~p inp#))]
     ~r))

(def p-int
  "parses a (possibly negative) integer"
  (->> (p-seq (maybe (chr \-))
              (charset+ DIGITS))
       (p-map #(if (= \- (first %))
                  (- (Long/parseUnsignedLong (apply str (second %))))
                  (Long/parseUnsignedLong (apply str %))))))

(def p-bigint
  "parses a number into a bigint"
  (->> (p-seq (maybe (chr \-))
              (charset+ DIGITS))
       (p-map #(if (= \- (first %))
                  (- (bigint (apply str (second %))))
                  (bigint (apply str %))))))

(def ws "parse whitespace" (charset+ WHITESPACE))
(def spaces "parse repeated spaces" (charset+ #{\space}))
(def skip-ws "parser that skips whitespace" (skip ws))
(def skip-spaces "parser that skips spaces" (skip spaces))
(def nl "parses a newline character" (chr \newline))
(def skip-nl "parser that skips a newline" (skip nl))
(defn skip-string "parser that skips string s" [s] (skip (string s)))

(def word "parses alphabetic text. equivalent to (charset+ LETTERS)" (charset+ LETTERS))
(def skip-word "skips a word" (skip word))

(defn label
  "Given a parser p, combines each key in ks with the corresponding element of
  the successful result of p into a map. Useful for labeling parsers chained with
  e.g., p-seq or list-+
  
  ```clojure
  user=> (def p (label [:a :b :c :d] (list-+ word ws)))
  user=> (->> \"this is a test\" string->stringbuf p :result)
  {:a \"this\", :b \"is\", :c \"a\", :d \"test\"}
  ```"
  [ks p]
  (p-map #(zipmap ks %) p))
