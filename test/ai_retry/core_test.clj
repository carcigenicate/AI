(ns ai-retry.core-test
  (:require [clojure.test :refer :all]
            [ai-retry.main :refer :all]
            [ai-retry.genetic-algorithm.settings :as s]
            [ai-retry.genetic-algorithm.population :as p]
            [ai-retry.genetic-algorithm.genetic-algorithm :as ga]

            [helpers.general-helpers :as g]
            [ai-retry.helpers.number-helpers :as nh]))

(def test-rand-gen (g/new-rand-gen 99))

(def gene-types (into #{} (range 12 1000)))

(defn factors-of [n]
  (let [factor-of? #(zero? (rem %2 %))
        sqrt #(Math/sqrt ^double %)]
    (filter #(factor-of? % n) (range 2 n))
    #_
    (reduce (fn [acc m]
              (if (factor-of? m n)
                (reduced false)
                acc))
            true
            (range 2 (inc (sqrt n))))))

(defn fitness-f [genes]
  (let [r #(mapv (fn [_] (g/random-int 0 1000 test-rand-gen))
                 (range (count genes)))]
    (nh/sum (mapv #(Math/abs ^double (- % %2)) genes (r)))))

(def problem-settings
  (s/->Problem-Settings fitness-f
                        gene-types
                        p/std-error-comparator))

; TODO: Fix magics
(def standard-settings
  (s/->Standard-Settings
    0.4 0.4 ; Elite-perc / lesser-perc
    0.5 ; Keep perc
    0.5 ; Mutation chance
    500)) ; Pop size

(def settings (s/->Settings standard-settings problem-settings))

(def test-population (p/new-random-population
                       gene-types
                       10
                       (-> settings :standard :pop-size)
                       test-rand-gen))

(defn test-scenario [pop]
  (ga/advance-generations pop settings 1e6 test-rand-gen))
