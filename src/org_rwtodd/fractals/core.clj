(ns org-rwtodd.fractals.core
  (:import (javax.swing JFrame JLabel JMenuBar)
           (java.awt Color)
           (java.awt.image BufferedImage))
  (:require [org-rwtodd.fractals.colors :as colors]
            [org-rwtodd.fractals.algo :as algo])
  (:gen-class))

(def starter-spec
  "Default starting point for fractal exploration"
  {
   :in-fractal "(algo/->Mandelbrot 32 4.0)"
   :in-scheme
 "(colors/combine-schemes 0
                         (colors/gradient-scheme 255 0xff0000 0xddff00 0x0000ff))"
   :center [-0.5 0.0]
   :size [1.0 1.0]
   :image-size [300 300]
   })

(defn- evaluate-state
  "fill out the application state by evaluating the inputs."
  [st]
  (let [[sx sy] (:image-size st)
        existing-img (:image st)]
    (assoc st
           :fractal (eval (read-string (:in-fractal st)))
           :scheme  (colors/vectorize-scheme (eval (read-string (:in-scheme st))))
           :image   (if (and existing-img
                             (= (.getWidth existing-img) sx)
                             (= (.getHeight existing-img) sy))
                      existing-img
                      (BufferedImage. sx sy BufferedImage/TYPE_INT_RGB)))))

(def app-state (atom (evaluate-state starter-spec)))

(defn generate-img
  []
  (let [st @app-state
        [cx cy]    (:center st)
        [szx szy]  (:size   st)]
    (algo/fill-image (:image st) (:fractal st) (:scheme st)
                     {:xmin (- cx szx) :xmax (+ cx szx)
                      :ymin (- cy szy) :ymax (+ cy szy)})))


(defn recenter!
  "Adjust the global state for a new center `x` and `y`, with optional
  rescaling by `scale`"
  ([x y]
   (swap! app-state assoc :center [x y]))
  ([x y scale]
   (swap! app-state
          (fn [s]
            (assoc s
                   :center [x y],
                   :size (let [[a b] (:size s)]
                           [(* scale a) (* scale b)]))))))
  
(defn change-inputs!
  "Update the global state's existing keys with any provided."
  [& kvs]
  (swap! app-state
         (fn [s]
           (evaluate-state (apply assoc s kvs)))))
    
(defn generate-frame
  "Start the main frame--must be called from swing thread"
  []
  ())

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
