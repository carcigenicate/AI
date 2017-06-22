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

; TODO: Change to shuffle + subvec the last n genes?

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
(defn random-genes2
  "Randomly picks n-to-pick many genes from the population.
 Returns a pair of [picked-genes remaining-genes].
 Note: remaining-genes will be shuffled."
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
          (random-genes rest-genes rest-to-pick rand-gen))))

(defn get-parents [sorted-pop elite-perc lesser-perc rand-gen]
  (let [parents (get-potential-parents sorted-pop elite-perc lesser-perc rand-gen)]))


(defn repopulate-population [sorted-pop expected-size rand-gen]
  (let [n-needed (- expected-size (count sorted-pop))]
    (into sorted-pop
          (mapv (fn [_])
                  ;TODO: Get parent pool, pick 2, breed
                (range n-needed)))))

(defn advance-population [sorted-pop mutate-chance perc-to-keep possible-gene-types]
  (let []))



