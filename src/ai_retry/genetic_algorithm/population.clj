(ns ai-retry.genetic-algorithm.population
  (:require [ai-retry.genetic-algorithm.sequence :as gs]
            [helpers.general-helpers :as g]))

(def new-population
  [])


(defn add-gene-sequence [pop genes]
  (conj pop genes))

(defn add-gene-sequences [pop gene-sequenes]
  (reduce add-gene-sequence pop gene-sequenes))

(defn new-random-population [possible-gene-types sequence-length pop-size rand-gen]
  (add-gene-sequences new-population
    (for [_ (range pop-size)]
      (gs/new-random-gene-sequence possible-gene-types sequence-length rand-gen))))

(defn remove-from-vector [v i]
  (into (subvec v 0 i) (subvec v (inc i))))

(defn remove-at-index
  "Returns a pair of [elem-at-i rest-elems]"
  [v i]
  (let [r (remove-from-vector v i)]
    [(v i) r]))

(defn random-genes [pop n-to-pick rand-gen]
  "Randomly picks n-to-pick many genes from the population.
  Returns a pair of [picked-genes remaining-genes]"
  (let [r-elem #(g/random-int 0 (count %) rand-gen)]
    (loop [rest-pop pop
           picked []]
      (if (>= (count picked) n-to-pick)
        [picked rest-pop]
        (let [[p r] (remove-at-index rest-pop (r-elem rest-pop))]
          (recur r (conj picked p)))))))

(defn seperate-elite-genes
  "Sorts the population according to the com(parator), seperates the top elite-perc percentage, and returns a pair of [elite-genes other-genes]."
  [pop elite-perc com]
  (let [sorted (vec (sort com pop))
        n-to-keep (int (* (count pop) elite-perc))]
    [(subvec sorted 0 n-to-keep) (subvec sorted n-to-keep)]))

(defn get-potential-parents
  "Returns the elite parents along with some randomly chosen lesser genes."
  [pop elite-perc lesser-perc]
  (let [[elites rest-genes] (seperate-elite-genes pop elite-perc)]))



#_
(defn seperate-bottom-perc
  "Returns a population where only the first perc-to-keep (decimal) percentage of the population is kept (rounded down). The sort order is defined by the comparator com."
  [pop perc-to-keep com]
  (let [sorted (vec (sort com pop))
        n-to-keep (int (* (count pop) perc-to-keep))]
    (subvec sorted 0 n-to-keep)))

(defn mutate-population [pop mutate-chance possible-gene-types rand-gen]
  (mapv #(-> %
             (gs/swap-mutate rand-gen)
             (gs/replacement-mutate possible-gene-types rand-gen))
        pop))


(defn advance-population [pop mutate-chance perc-to-keep possible-gene-types])



