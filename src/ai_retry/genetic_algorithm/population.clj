(ns ai-retry.genetic-algorithm.population
  (:require [ai-retry.genetic-algorithm.sequence :as gs]
            [helpers.general-helpers :as g])

  (:import [ai_retry.genetic_algorithm.settings Settings]))

(def parent-one-breed-chance 0.5)

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

(defn mutate-population [pop mutate-chance possible-gene-types rand-gen]
  (let [mut? #(g/random-perc mutate-chance rand-gen)]
    (mapv #(let [swap-mut (if (mut?)
                            (gs/swap-mutate % rand-gen)
                            %)
                 replace-mut (if (mut?)
                               (gs/replacement-mutate swap-mut possible-gene-types rand-gen)
                               swap-mut)]
             replace-mut)
          pop)))

; TODO: Try to think of a better way. Shuffling isn't that efficient.
(defn multiple-random-from-coll
  "Randomly picks n-to-pick many elements from the collection.
 Returns a pair of [picked-elems remaining-elems].
 Note: remaining-elems will be shuffled."
  [pop n-to-pick rand-gen]
  (let [shuffled (g/shuffle pop rand-gen)
        start-i (- (count pop) n-to-pick)]
    [(subvec shuffled start-i) (subvec shuffled 0 start-i)]))

(defn seperate-elite-genes
  "Expects the population is sorted already. The fittest gene should be first.
  Seperates the top elite-perc percentage, and returns a pair of [elite-genes other-genes]."
  [sorted-pop elite-perc]
  (let [n-to-keep (int (* (count sorted-pop) elite-perc))]
    [(subvec sorted-pop 0 n-to-keep) (subvec sorted-pop n-to-keep)]))

(defn get-potential-parents
  "Returns the elite parents along with some randomly chosen lesser genes."
  [sorted-pop elite-perc lesser-perc rand-gen]
  (let [[elites rest-genes] (seperate-elite-genes sorted-pop elite-perc)
        rest-to-pick (* (count rest-genes) lesser-perc)]
    (into elites
          (multiple-random-from-coll rest-genes rest-to-pick rand-gen))))

(defn get-parents [sorted-pop elite-perc lesser-perc rand-gen]
  (let [parents (get-potential-parents sorted-pop elite-perc lesser-perc rand-gen)]
    (multiple-random-from-coll parents 2 rand-gen)))

(defn repopulate-population [sorted-pop expected-size elite-perc lesser-perc rand-gen]
  (let [n-needed (- expected-size (count sorted-pop))
        parents #(get-parents sorted-pop elite-perc lesser-perc rand-gen)]
    (into sorted-pop
          (mapv (fn [_]
                  (let [[p1 p2] (parents)]
                    (gs/breed p1 p2 parent-one-breed-chance rand-gen)))
                (range n-needed)))))

(defn advance-population [sorted-pop ^Settings settings fitness-comparator rand-gen]
  (let [{{ec :elite-chance lc :lesser-chance mc :mut-chance} :standard,
         {pg :possible-gene-types} :problem} settings
        sorted-pop (sort pop fitness-comparator)

        mutated-pop (mutate-population sorted-pop mc pg rand-gen)]))


; TODO: DO SOME TESTING!!!!!!!!

