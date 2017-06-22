(ns ai-retry.genetic-algorithm.genetic-algorithm
  (:require [ai-retry.genetic-algorithm.population :as p]
            [ai-retry.genetic-algorithm.settings :as s]))

(defrecord Genetic-Algorithm [population fitness-f settings])

(defn new-ga-obj
  ([fitness-f settings]
   (->Genetic-Algorithm p/new-population fitness-f settings))
  ([fitness-f]
   (new-ga-obj fitness-f s/default-standard-settings)))

(defn advance-generation [ga])
