(ns tube.core)
(use '[clojure.java.io :only [reader]])
(use 'clojure.set)
(use '[clojure.string :as str :only (lower-case replace)
       :rename {replace str-replace}] )

(def alphabet #{\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z})
(assert (= (count alphabet) 26))

(def stations
  (line-seq (reader "stations.txt")))

(def filtered-stations
    (map #(set(str-replace % #"[\W\d]" "" ))  (map str/lower-case stations)))

(def station-map
  (into (sorted-map) (zipmap stations filtered-stations)))

(defn sort-stations [req-letters stations]
    (let [results stations]
      (into (sorted-map-by (fn [key1 key2]
                             (compare [(count (difference req-letters (get results key1))) key1]
                                      [(count (difference req-letters (get results key2))) key2])))
            results)))

(def data-struct
    {:stations [] :current-letters [] :remaining-stations station-map :req-letters alphabet } )

(defn get-next-station [data]
  (let [station (first (sort-stations (data :req-letters) (data :remaining-stations)))]
;  (println (str (first station)) (second station))
  {
   :remaining-stations (dissoc (data :remaining-stations) (first station))
   :req-letters (difference (data :req-letters) (second station))
   :stations (conj (data :stations) (first station)) 
   :current-letters (union (data :current-letters) (second station))
  }
  ))
  
(defn main []
  (loop [data data-struct]   
  ;(println data)
  (if (zero? (count (difference alphabet (data :current-letters))))
    (println "done" (data :stations))
    (recur (get-next-station data)))))



