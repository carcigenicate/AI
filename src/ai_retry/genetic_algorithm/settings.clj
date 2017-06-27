(ns ai-retry.genetic-algorithm.settings)

(defrecord Standard-Settings [elite-perc lesser-perc keep-perc mut-chance pop-size])

(def default-standard-settings
  (->Standard-Settings
    0.4
    0.2
    0.3
    0.01
    100))

(defrecord Problem-Settings [fitness-f possible-gene-types fit-err-comp])

(defrecord Settings [standard problem])

(defn default-settings [problem-settings]
  (->Settings default-standard-settings
              problem-settings))

