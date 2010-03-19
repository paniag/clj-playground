(import '(java.util.concurrent Executors))

(def *v* 0)

(defn incv [n] (set! *v* (+ *v* n)))

(defn test-vars [nthreads niters]
  (let [pool (Executors/newFixedThreadPool nthreads)
        tasks (map (fn [t]
                     #(binding [*v* 0]
                        (dotimes [n niters]
                          (incv t))
                        *v*))
                   (range nthreads))
        ]
    (let [ret (.invokeAll pool tasks)]
      (.shutdown pool)
      (map #(.get %) ret))))

(defn test-recur-vars [nthreads depth]
  (println *v*)
  (if (= 0 depth)
    *v*
    (conj
     (let [pool  (Executors/newFixedThreadPool nthreads)
           tasks (map (fn [_]
                        #(binding [*v* *v*]
                           (println *v*)
                           (incv 1)
                           (test-recur-vars nthreads (dec depth))))
                      (range nthreads))
           ]
       (let [ret (.invokeAll pool tasks)]
         (.shutdown pool)
         (map #(.get %) ret)))
     *v*)))

(defn t4 [v n d]
  (if (> 2 d)
    v
    [
     v
     (let [pool (Executors/newFixedThreadPool n)
           tasks (map (fn [_]
                        #(t4 (inc v) n (dec d)))
                      (range n))
           ]
       (let [ret (.invokeAll pool tasks)]
         (.shutdown pool)
         (map #(.get %) ret)))
     ]))

(defn t3 [d]
  "apparently thread contexts do not nest, but just derive from global namespace"
  (println *v*)
  (when (< 0 d)
    (binding [*v* *v*]
    (let [pool (Executors/newFixedThreadPool 1)
          task (map (fn [_]
                      #(do ;binding [*v* *v*]
                      (incv 1)
                      (t3 (dec d))))
                    [1])
          ]
      (let [ret (.invokeAll pool task)]
        (.shutdown pool)
        (map #(.get %) ret))))))

(defn t2 [d]
  "works"
  (println *v*)
  (when (< 0 d)
    (binding [*v* *v*]
      (incv 1)
      (t2 (dec d)))))

(defn test-loop-vars [depth]
  (loop [depth depth]
    (if (< 0 depth)
      (binding [*v* *v*]
        (incv 1)
        (println *v*)
        (recur (dec depth)))
      (println *v*))))
