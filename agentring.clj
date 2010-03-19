(defn relay [x i use-queue]
  (when (:next x)
    (send (:next x) relay i use-queue))
  (when (and use-queue (:report-queue x))
    (.put (:report-queue x) i))
  x)

(defn run [m n use-queue]
  (let [q (new java.util.concurrent.SynchronousQueue)
        hd (reduce (fn [next _] (agent {:next next}))
                   (agent {:report-queue q})
                   (range (dec m)))]
    (doseq [i (range n)]
      (send hd relay i use-queue))
    (when use-queue
      (doseq [i (range n)]
        (println (.take q))))))
