(require '[clojure.core.match :refer [match]])

(def max-steps 100) ; set to nil or false to allow infinite loops

(def adder
  {:a [:jeqz 0 :d :b]
   :b [:dec 0 :c]
   :c [:inc 2 :a]
   :d [:jeqz 1 :g :e]
   :e [:dec 1 :f]
   :f [:inc 2 :d]
   :g :halt
   :start :a})

(def subtractor
  {:a [:jeqz 1 :e :b]
   :b [:jeqz 0 :b :c]
   :c [:dec 0 :d]
   :d [:dec 1 :a]
   :e [:jeqz 0 :h :f]
   :f [:dec 0 :g]
   :g [:inc 2 :e]
   :h :halt
   :start :a})

(defn hash-map->seq [m]
  (->> (keys m) (apply max) inc range (map #(m % 0))))

(defn seq->hash-map [v]
  (zipmap (range) v))

(defn run [machine memory]
  (loop [node (machine (machine :start))
         memory (seq->hash-map memory)
         steps 0]
    (if (and max-steps (> steps max-steps))
      :bottom
      (match node
             :halt (hash-map->seq memory)
             [:inc i next] (let [val (inc (memory i 0))]
                             (recur (machine next)
                                    (assoc memory i val)
                                    (inc steps)))
             [:dec i next] (let [val (max 0 (dec (memory i 0)))]
                             (recur (machine next)
                                    (assoc memory i val)
                                    (inc steps)))
             [:jeqz i if-zero if-not-zero] (let [next (if (zero? (memory i))
                                                        if-zero
                                                        if-not-zero)]
                                             (recur (machine next)
                                                    memory
                                                    (inc steps)))))))

(println (run adder [1 5 7])) ; (0 0 13)
(println (run subtractor [8 3])) ; (0 0 5)
(println (run subtractor [3 8])) ; :bottom
