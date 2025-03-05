(ns evidencia3-simulation.core
   (:require [clojure.java.io :as io]
             [clojure.string :as str]))

 (defn leer-archivo [path]
   (try
     (with-open [reader (io/reader path)]
       (let [contenido (slurp reader)
             datos (read-string contenido)]
         datos))
     (catch Exception e
       (println "Error al leer el archivo:" path)
       (throw e))))

 (defn procesar-crucero [crucero-id]
   (let [crucero-path (str "C:/Users/VaneJ/OneDrive/Datos adjuntos de correo electrónico/Escritorio/Evidencia 3/Crucero" crucero-id ".txt")]
     {:id crucero-id :crucero (leer-archivo crucero-path)}))

 (defn procesar-cruceros [num-cruceros]
   (pmap procesar-crucero (range 1 (inc num-cruceros))))

 (defn contar-vehiculos [crucero]
   (reduce + (pmap (fn [semaforo]
                     (let [carriles (nth semaforo 5)]
                       (reduce + (pmap (fn [carril]
                                         (count (rest carril))) carriles))))
                   (rest (:crucero crucero)))))

 (defn tiempo-espera-por-semaforo [semaforo]
   (let [luces (rest (nth semaforo 3))
         tiempo-espera (reduce + (map (fn [[color tiempo]]
                                        (if (= color 'verde) 0 tiempo))
                                      luces))
         vehiculos-por-carril (map #(count (rest %)) (nth semaforo 5))]
     (* tiempo-espera (reduce + vehiculos-por-carril))))

 (defn tiempo-espera-por-crucero [crucero]
   (reduce + (pmap tiempo-espera-por-semaforo (rest (:crucero crucero)))))

 (defn tiempo-promedio-espera [cruceros]
   (let [total-espera (reduce + (pmap tiempo-espera-por-crucero cruceros))
         total-vehiculos (reduce + (pmap contar-vehiculos cruceros))]
     (if (zero? total-vehiculos)
       0
       (/ (double total-espera) total-vehiculos))))

 (defn clasificar-cruceros [cruceros]
   (let [cruceros-info (pmap (fn [crucero]
                               {:nombre (:id crucero)
                                :cantidad-semaforos (count (rest (:crucero crucero)))
                                :cantidad-vehiculos (contar-vehiculos crucero)
                                :tiempo-promedio-espera (tiempo-promedio-espera [crucero])})
                             cruceros)]
     (sort-by (juxt :nombre :cantidad-semaforos :cantidad-vehiculos) cruceros-info)))

 (defn imprimir-cruceros [cruceros-clasificados]
   (println "Listas de Cruceros")
   (doseq [crucero cruceros-clasificados]
     (println (str "Crucero: " (:nombre crucero)))
     (println (str "Cantidad de semáforos: " (:cantidad-semaforos crucero)))
     (println (str "Cantidad de vehículos: " (:cantidad-vehiculos crucero)))
     (println (str "Tiempo promedio de espera: " (format "%.2f" (:tiempo-promedio-espera crucero)) " segundos"))
     (println)))

(defn calcular-tiempo-verde [luces]
  (reduce + (map (fn [[color tiempo]]
                   (if (= color 'verde) tiempo 0))
                 luces)))

(defn calcular-tiempo-despeje [vehiculos tiempo-verde]
  (let [vehiculos-en-verde (filter #(<= % tiempo-verde) vehiculos)]
    (count vehiculos-en-verde)))

(defn calcular-tiempo-muerto [semaforo]
  (let [luces (rest (nth semaforo 3))
        tiempo-verde (calcular-tiempo-verde luces)
        vehiculos (mapcat rest (nth semaforo 5))
        tiempo-despeje (calcular-tiempo-despeje vehiculos tiempo-verde)]
    (max 0 (- tiempo-verde tiempo-despeje))))

(defn calcular-ciclo-luces [luces]
  (reduce + (map second luces)))

(defn filtrar-sentidos [sentidos]
  (let [sentidos-mapa {'D "Derecha" 'I "Izquierda" 'R "Recto"}]
    (->> sentidos
         (filter #(not= 0 (second %)))
         (map (fn [[sentido _]] (get sentidos-mapa sentido)))
         (filter identity)
         (clojure.string/join ", "))))

(defn formatear-vehiculos-por-carril [carriles-vehiculos]
  (->> carriles-vehiculos
       (map (fn [[carril & vehiculos]] (str (name carril) ": " (count vehiculos))))
       (clojure.string/join ", ")))

(defn contar-vehiculos-por-carril [carriles-vehiculos]
  (reduce + (map #(count (rest %)) carriles-vehiculos)))

(defn calcular-tiempo-muerto-acumulado [cruceros]
  (let [semaforos (mapcat (fn [crucero]
                            (map (fn [semaforo]
                                   (let [tiempo-muerto (calcular-tiempo-muerto semaforo)]
                                     {:crucero-id (:id crucero)
                                      :semaforo-id (first semaforo)
                                      :tiempo-muerto tiempo-muerto}))
                                 (rest (:crucero crucero))))
                          cruceros)]
    (reduce (fn [acc semaforo]
              (update acc {:crucero-id (:crucero-id semaforo)
                           :semaforo-id (:semaforo-id semaforo)}
                      (fn [v] (+ (or v 0) (:tiempo-muerto semaforo)))))
            {}
            semaforos)))

(defn top-cruceros-espera [cruceros porcentaje]
  (let [cruceros-con-espera (map (fn [crucero]
                                   {:nombre (:id crucero)
                                    :tiempo-espera (tiempo-promedio-espera [crucero])})
                                 cruceros)
        top-cruceros (take (Math/ceil (* porcentaje (count cruceros-con-espera)))
                           (sort-by :tiempo-espera > cruceros-con-espera))]
    top-cruceros))

(defn top-cruceros-menor-espera [cruceros porcentaje]
  (let [cruceros-con-espera (map (fn [crucero]
                                   {:nombre (:id crucero)
                                    :tiempo-espera (tiempo-promedio-espera [crucero])})
                                 cruceros)
        top-cruceros (take (Math/ceil (* porcentaje (count cruceros-con-espera)))
                           (sort-by :tiempo-espera cruceros-con-espera))]
    top-cruceros))

(defn imprimir-top-cruceros [cruceros]
  (println "Top 10% de cruceros con mayor tiempo de espera:")
  (doseq [crucero cruceros]
    (println (str "Crucero: " (:nombre crucero) ", Tiempo de espera promedio: " (:tiempo-espera crucero))))
  (println))

(defn imprimir-top-cruceros-menor-espera [cruceros]
  (println "Top 10% de cruceros con menor tiempo de espera:")
  (doseq [crucero cruceros]
    (println (str "Crucero: " (:nombre crucero) ", Tiempo de espera promedio: " (:tiempo-espera crucero))))
  (println))

(defn estadisticas-crucero [crucero]
  (let [id (:id crucero)
        semaforos (rest (:crucero crucero))]
    (println (str "Estadisticas del Crucero " id ":"))
    (println (str "Cantidad de semaforos: " (count semaforos)))
    (doseq [semaforo semaforos]
      (let [semaforo-id (first semaforo)
            direccion (second semaforo)
            carriles (nth semaforo 2)
            luces (nth semaforo 3)
            sentidos (nth semaforo 4)
            carriles-vehiculos (nth semaforo 5)]
        (try
          (let [luces (rest luces) ; Corregir aquí si es necesario quitar la etiqueta "luces"
                ciclo-luces (calcular-ciclo-luces luces)
                tiempo-muerto (calcular-tiempo-muerto semaforo)
                total-vehiculos (contar-vehiculos-por-carril carriles-vehiculos)
                sentidos-filtrados (filtrar-sentidos (rest sentidos))
                vehiculos-formateados (formatear-vehiculos-por-carril carriles-vehiculos)]
            (println (str "Semaforo ID: " semaforo-id))
            (println (str "Direccion: " direccion))
            (println (str "Carriles: " carriles))
            (println (str "Ciclo de tiempo de las luces: " ciclo-luces))
            (println (str "Tiempo muerto en verde: " tiempo-muerto))
            (println (str "Sentidos: " sentidos-filtrados))
            (println (str "Vehiculos por carril: " vehiculos-formateados))
            (println (str "Total de vehiculos: " total-vehiculos))
            (println))
          (catch Exception e
            (println "Error procesando semaforo:" semaforo)
            (println (.getMessage e))
            (throw e)))))))

(defn top-semaforos-tiempo-muerto [cruceros porcentaje]
  (let [tiempos-muertos (calcular-tiempo-muerto-acumulado cruceros)
        sorted-semaforos (sort-by val > tiempos-muertos)
        num-top (max 1 (int (Math/ceil (* porcentaje (count sorted-semaforos)))))
        top-semaforos (take num-top sorted-semaforos)]
    top-semaforos))

(defn imprimir-top-semaforos [top-semaforos]
  (println "Top 10% de semforos con mayor tiempo muerto en verde:")
  (doseq [semaforo top-semaforos]
    (println (str "Crucero ID: " (:crucero-id (first semaforo))
                  ", Semaforo ID: " (:semaforo-id (first semaforo))
                  ", Tiempo muerto acumulado: " (second semaforo) " segundos"))))

(defn -main []
  (let [num-cruceros 2 ;; Cambia esto según la cantidad de cruceros que deseas procesar
        cruceros-procesados (procesar-cruceros num-cruceros)
        cruceros-clasificados (clasificar-cruceros cruceros-procesados)]
    (imprimir-cruceros cruceros-clasificados)
    (loop []
      (println "Ingrese los numeros de los cruceros que desea observar (separados por comas):")
      (let [input (read-line)
            crucero-ids (map #(Integer/parseInt %) (clojure.string/split input #","))
            cruceros-seleccionados (filter #(some #{(:id %)} crucero-ids) cruceros-procesados)]
        (doseq [crucero cruceros-seleccionados]
          (estadisticas-crucero crucero))
        (println "¿Quieres seguir observando cruceros? (si/no)")
        (let [respuesta (read-line)]
          (when (= respuesta "si")
            (recur)))))
    (println "Tiempo promedio de espera en la totalidad de los vehiculos de la simulacion (en segundos):")
    (println (format "%.2f" (tiempo-promedio-espera cruceros-procesados)))
    (println)
    (let [top-cruceros (top-cruceros-espera cruceros-procesados 0.1)
          top-cruceros-menor (top-cruceros-menor-espera cruceros-procesados 0.1)
          top-semaforos (top-semaforos-tiempo-muerto cruceros-procesados 0.1)]
      (imprimir-top-cruceros top-cruceros)
      (imprimir-top-cruceros-menor-espera top-cruceros-menor)
      (imprimir-top-semaforos top-semaforos))))
