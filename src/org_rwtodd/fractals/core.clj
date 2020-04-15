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
   :fractal "(algo/->Mandelbrot 32 4.0)"
   :colors
 "(colors/vectorize-scheme
 (colors/combine-schemes 0
                         (colors/gradient-scheme 255 0xff0000 0xddff00 0x0000ff)))"
   :center [-0.5 0.0]
   :size [1.0 1.0]
   :image-size [300 300]
   })

(defn- evaluate-state
  "fill out the application state by evaluating the inputs."
  [st]
  (let [[sx sy] (:image-size st)]
    (assoc st
           :algorithm (eval (read-string (:fractal st)))
           :scheme    (eval (read-string (:colors st)))
           :image     (BufferedImage. sx sy BufferedImage/TYPE_INT_RGB))))

(def app-state (atom (evaluate-state starter-spec)))

(defn generate-img
  []
  (let [st @app-state
        [cx cy]    (:center st)
        [szx szy]  (:size   st)]
    (algo/fill-image (:image st) (:algorithm st) (:scheme st)
                     {:xmin (- cx szx) :xmax (+ cx szx)
                      :ymin (- cy szy) :ymax (+ cy szy)})))
        
(defn generate-frame
  "Start the main frame--must be called from swing thread"
  []
  ())

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
