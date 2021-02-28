(defn eval-msg [{:keys [inspect] :as msg}]
  (if inspect
    (println msg)
    msg))
