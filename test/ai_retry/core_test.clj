(ns ai-retry.core-test
  (:require [clojure.test :refer :all]
            [ai-retry.main :refer :all]
            [ai-retry.genetic-algorithm.settings :as s]
            [ai-retry.genetic-algorithm.population :as p]
            [helpers.general-helpers :as g]))

(def test-rand-gen (g/new-rand-gen 99))

(def problems-settings
  (s/->Problem-Settings #(reduce + 0 %)
                        (into #{} (range 10))))

(def settings (s/default-settings problems-settings))

(def test-population [[1 2 3] [4 5 6] [7 8 9]
                      [1 9 5] [2 8 4] [9 6 3]
                      [3 2 1] [4 5 6] [9 8 7]])

(defn test-scenario [pop]
  (p/advance-population pop settings p/std-fitness-comparator test-rand-gen))
