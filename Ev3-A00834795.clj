(println "pls work bro") ; IT WORKS WUUUU

(require '[clojure.java.io :as io])
;(require '[clojure.string :as str])

(defn leer-archivo [path]
  (with-open [reader (io/reader path)]
    (doall (map read-string (line-seq reader)))))

(defn procesar-crucero [crucero-id]
  (let [crucero-path (str "C:/Users/VaneJ/OneDrive/Datos adjuntos de correo electrónico/Escritorio/Evidencia 3/Crucero" crucero-id ".txt")
        carriles-path (str "C:/Users/VaneJ/OneDrive/Datos adjuntos de correo electrónico/Escritorio/Evidencia 3/Carriles" crucero-id ".txt")
        crucero-info (leer-archivo crucero-path)
        carriles-info (leer-archivo carriles-path)]
    {:id crucero-id :crucero crucero-info :carriles carriles-info}))

(defn procesar-cruceros [num-cruceros]
  (pmap procesar-crucero (range 1 (inc num-cruceros))))

(def cruceros (procesar-cruceros 2))

(println cruceros)

