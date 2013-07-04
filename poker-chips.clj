(use '[clojure.string :only (join)])

(defn create-initial-stacks
  [n]
  (list (repeat n 0) (repeat n 1)))

(defn done?
  [lists]
  (= 1 (count (distinct (nth lists 0)))))

(defn mix-and-divide
  [lists c]
    (let [l1 (nth lists 0)
          l2 (nth lists 1)
          divided (partition (count l1) (interleave l1 l2))]
    (if (not (done? divided))
          (recur divided (inc c))
          c)))

(defn calc-steps
  [stack-size]
  (mix-and-divide (create-initial-stacks stack-size) 1))

(defn format-line
  [stack-size steps]
  (str stack-size ": " steps))

(println 
 (join "\n" 
   (map #(format-line % (calc-steps %)) 
        (range 1 101))))
