(ns ai-retry.helpers.number-helpers)

(defn sum [numbers]
  (reduce + 0 numbers))

(defn average [numbers]
  (/ (sum numbers)
     (count numbers)))
