(ns ai-retry.genetic-algorithm.population
  (:require [ai-retry.genetic-algorithm.sequence :as gs]
            [helpers.general-helpers :as g]))

(def parent-one-breed-chance 0.5)

(def genes-accessor first)
(def fitness-accessor second)
(def std-fitness-comparator #(> (fitness-accessor %) (fitness-accessor %2)))
(def std-error-comparator #(< (fitness-accessor %) (fitness-accessor %2)))

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
  "Expects the population is sorted already. The fittest individual should be first.
  Seperates the top elite-perc percentage, and returns a pair of [elite-individuals other-individuals]."
  [sorted-pop elite-perc]
  (let [n-to-keep (int (* (count sorted-pop) elite-perc))]
    [(subvec sorted-pop 0 n-to-keep) (subvec sorted-pop n-to-keep)]))

(defn get-potential-parents
  "Returns the elite parents along with some randomly chosen lesser genes."
  [sorted-pop elite-perc lesser-perc rand-gen]
  (let [[elites rest-genes] (seperate-elite-genes sorted-pop elite-perc)
        rest-to-pick (* (count rest-genes) lesser-perc)]
    (into elites
          ; TODO: Waste throwing away second?
          (first (multiple-random-from-coll rest-genes rest-to-pick rand-gen)))))

(defn get-parents [sorted-pop elite-perc lesser-perc rand-gen]
  (let [parents (get-potential-parents sorted-pop elite-perc lesser-perc rand-gen)]
    (first (multiple-random-from-coll parents 2 rand-gen))))

(defn repopulate-population [sorted-pop expected-size elite-perc lesser-perc rand-gen]
  (let [n-needed (- expected-size (count sorted-pop))
        parents #(get-parents sorted-pop elite-perc lesser-perc rand-gen)]
    (into sorted-pop
          (mapv (fn [_]
                  (let [[p1 p2] (parents)]
                    (gs/breed p1 p2 parent-one-breed-chance rand-gen)))
                (range n-needed)))))

(defn get-children [sorted-pop n-children elite-perc lesser-perc rand-gen]
  (let [parents #(get-parents sorted-pop elite-perc lesser-perc rand-gen)]
    (mapv (fn [_]
            (let [[p1 p2] (parents)]
              (gs/breed p1 p2 parent-one-breed-chance rand-gen)))
          (range n-children))))

(defn combine-populations [& pops]
  (reduce into [] pops))

(defn remove-scores [judged-pop]
  ; TODO: Eager or lazy? Sorting *should* force everything anyways.
  (mapv genes-accessor judged-pop))

(defn judge-population
  "Runs each individual through the fitness function to evaluate their fitness.
  Returns a lazy list of pairs of [individual fitness-score]"
  [pop fitness-f]
  ;  Pass in flag?
  (map #(vector % (fitness-f %))
       pop))

(defn judge-and-sort-population [pop fitness-f com]
  (remove-scores
    (sort com
      (judge-population pop fitness-f))))

(defn remove-weak-individuals [sorted-pop keep-perc]
  (let [keep-n (int (* (count sorted-pop) keep-perc))]
    (subvec sorted-pop 0 keep-n)))

(defn n-children-per-generation [pop-size keep-perc]
  (- pop-size (* keep-perc pop-size)))

(defn advance-population [pop settings fitness-comparator rand-gen]
  (let [{{ep :elite-perc lp :lesser-perc mc :mut-chance ps :pop-size kp :keep-perc} :standard,
         {pg :possible-gene-types ff :fitness-f} :problem} settings
        sorted-pop (judge-and-sort-population pop ff fitness-comparator)
        n-children (n-children-per-generation ps kp)
        children (get-children sorted-pop n-children ep lp rand-gen)

        thinned-pop (remove-weak-individuals sorted-pop kp)
        mutated-children (mutate-population children mc pg rand-gen)]

    (combine-populations thinned-pop mutated-children)))


; TODO: DO SOME TESTING!!!!!!!!

