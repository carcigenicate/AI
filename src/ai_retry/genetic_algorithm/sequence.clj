(ns ai-retry.genetic-algorithm.sequence
  (:require [helpers.general-helpers :as g]))

(defn new-sequence-from-coll [coll]
  (vec coll))

(defn new-default-gene-sequence [default-gene n-genes]
  (new-sequence-from-coll (repeat n-genes default-gene)))

(defn new-random-gene-sequence [possible-gene-types sequence-length rand-gen]
  (new-sequence-from-coll
    (for [_ (range sequence-length)]
      (g/random-from-collection possible-gene-types rand-gen))))

; Mutations

(defn swap-genes [genes i j]
  ; Transients?
  (let [t (get genes i)]
    (-> genes
      (assoc i (get genes j))
      (assoc j t))))

(defn rand-gene-index [genes rand-gen]
  (g/random-int 0 (count genes) rand-gen))

(defn swap-mutate [genes rand-gen]
  ; Allow i and j to be the same?
  (let [n-genes (count genes)
        i (rand-gene-index genes rand-gen)
        j (rand-gene-index genes rand-gen)]
    (swap-genes genes i j)))

(defn replacement-mutate [genes possible-gene-types rand-gen]
  (let [gene-type (g/random-from-collection possible-gene-types rand-gen)
        i (rand-gene-index genes rand-gen)]
    (assoc genes i gene-type)))

(defn genes-compatible? [genes1 genes2]
  (= (count genes1) (count genes2)))

(defn check-gene-compatibility [genes1 genes2]
  (when-not (genes-compatible? genes1 genes2)
    (throw (RuntimeException. (str "Genes " genes1 " and " genes2 " must be the same length!")))))

(defn breed [genes1 genes2 first-parent-chance rand-gen]
  (check-gene-compatibility genes1 genes2)

  (reduce (fn [acc [g1 g2]]
            (if (g/random-perc first-parent-chance rand-gen)
              g1
              g2))
          []
          (map vector genes1 genes2)))

