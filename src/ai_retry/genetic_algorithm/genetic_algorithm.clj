(ns ai-retry.genetic-algorithm.genetic-algorithm
  (:require [ai-retry.genetic-algorithm.population :as p]
            [ai-retry.genetic-algorithm.settings :as s]
            [helpers.general-helpers :as g])

  (:import [java.util Date]))

(defn date-stamp []
  (str (Date.)))

(defn print-population [pop]
  (doseq [g pop]
    (println g)))

; TODO: Write a version of judge-and-sort that doesnt automatically remove fitness information

(defn standard-display-f [max-gens fitness-f fit-err-comp {gen :current-gen, advanced-pop :advanced-pop}]
  (let [print-every (/ max-gens 200)
        sorted-pop #(p/judge-and-sort-population % fitness-f fit-err-comp)
        sample-fitness #(-> % first fitness-f)
        perc #(g/prec-round 3 (* 100 (/ (double %) %2)))]

    (when (zero? (rem gen print-every))
      (let [sp (sorted-pop advanced-pop)]
        (println (str (date-stamp) " - " (count advanced-pop) "\n\t"
                      (perc gen max-gens) "% - " gen "\n\t"
                      "Best: "(sample-fitness sp)))
        (print-population (take 5 sp))
        (println "\n")))))

(defn advance-generations
  "Advances the population max-generations many generations.
  Basically an iteration of p/advance-population.
  Display-f is called once per iteration and should accept a map of {:current-gen, :advanced-pop}
  and do any displaying necessary. Pass nil to not use a display function."
  [population settings display-f max-generations rand-gen]
  (let [problem-settings (:problem settings)
        fit-err-comp (:fit-err-comp problem-settings)
        checked-display-f (or display-f #(do %))]

    (loop [gen 0
           acc-pop population]

      (if (< gen max-generations)
        (let [advanced-pop (p/advance-population
                             acc-pop
                             settings
                             fit-err-comp
                             rand-gen)]

          (checked-display-f {:current-gen gen
                              :advanced-pop advanced-pop})

          (recur (inc gen) advanced-pop))

        acc-pop))))