(defn loves [x y]
  (str x " loves " y))
(defn test-rebind []
  (println (loves "ricky" "lucy"))
  (let [loves-orig loves]
    (binding [loves (fn [& args]
                    (println "Logging str")
                    (apply loves-orig args))]
      (println (loves "fred" "ethel")))))
