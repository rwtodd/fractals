(ns org-rwtodd.fractals.core
  (:import (javax.swing JFrame JLabel JMenuBar)
           (java.awt Color)
           (java.awt.image BufferedImage))
  (:gen-class))

(def starter-spec
  "Default starting point for fractal exploration"
  {
   :fractal "mandelbrot"
   :colorscheme "sunfire"
   :depth 500
   :ul [0.0 0.0]
   :scale [0.01 0.01]
   })

(defn generate-frame
  "Start the main frame--must be called from swing thread"
  []
  ())

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
