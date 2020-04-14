(ns org-rwtodd.fractals.algo
  (:import [java.awt.image BufferedImage])
  (:require [org-rwtodd.fractals.colors :as colors])
  (:gen-class))

(definterface Algorithm
  (^int fidelity [])
  (^int point [^double x ^double y]))

(deftype Mandelbrot [depth esc]
  Algorithm
  (fidelity [_] depth)
  (point [_ x y]
    (loop [ans (dec depth)
           cx x
           cy y]
      (let [xsq (* cx cx)
            ysq (* cy cy)]
        (if (or (zero? ans) (< esc (+ xsq ysq)))
          ans
          (let [tmp (* cx cy)]
            (recur (dec ans)
                   (+ x (- xsq ysq))
                   (+ y tmp tmp))))))))

(defn fill-image
  [^BufferedImage img ^Algorithm alg scheme coords]
  (let [xmax (.getWidth img)
        ymax (.getHeight img)
        algx-min (nth coords 0)
        x-scale (/ (- (nth coords 1) algx-min) xmax)
        algy-min (nth coords 2)
        y-scale (/ (- (nth coords 3) algy-min) ymax)
        color-scale (/ (colors/depth scheme) (.fidelity alg))]
    (doseq [y (range ymax),  x (range xmax)]
      (let [algx  (+ algx-min (* x x-scale))
            algy  (+ algy-min (* y y-scale))
            ptval (.point alg algx algy)
            scaled (int (*  ptval color-scale))]
        (.setRGB img x y (colors/get-color scheme scaled))))
    img))

(comment  
(let [img (BufferedImage. 800 800 BufferedImage/TYPE_INT_RGB)
      alg (Mandelbrot. 256 4.0)
      cs   (colors/vectorize-scheme
            (colors/combine-schemes java.awt.Color/BLACK
                                    (colors/gradient-scheme 255 java.awt.Color/RED
                                                            java.awt.Color/YELLOW
                                                            java.awt.Color/BLUE)))]
  (fill-image img alg cs [-1.395 -1.39 -0.015 -0.005]))
)
