(ns ai-retry.genetic-algorithm.settings)

(defrecord Settings [elite-perc rand-pick-perc mut-chance])

(def default-settings
  (->Settings 0.4 0.1 0.01))
