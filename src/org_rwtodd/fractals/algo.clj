(ns org-rwtodd.fractals.algo
  (:import [java.awt.image BufferedImage])
  (:require [org-rwtodd.fractals.colors :as colors])
  (:gen-class))

(definterface Algorithm
  (^int fidelity [])
  (^int point [^double x ^double y]))

;; ~~~~ START faster math for this part of the file ~~~
(set! *unchecked-math* true)

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

(deftype JuliaSquared [depth esc addX addY]
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
                   (+ addX (- xsq ysq))
                   (+ addY tmp tmp))))))))

(deftype JuliaExp [depth esc addX addY]
  Algorithm
  (fidelity [_] depth)
  (point [_ x y]
    (loop [ans (dec depth)
           cx x
           cy y]
      (if (or (zero? ans) (< esc (+ (* cx cx) (* cy cy))))
        ans
        (let [tmp (Math/exp cx)]
          (recur (dec ans)
                 (+ addX (* tmp (Math/cos cy)))
                 (+ addY (* tmp (Math/sin cy)))))))))

(deftype JuliaZExp [depth esc addX addY]
  Algorithm
  (fidelity [_] depth)
  (point [_ x y]
    (loop [ans (dec depth)
           cx x
           cy y]
      (if (or (zero? ans) (< esc (+ (* cx cx) (* cy cy))))
        ans
        (let [tmp (Math/exp cx)
              expr (* tmp (Math/cos cy))
              expi (* tmp (Math/sin cy))]
          (recur (dec ans)
                 (+ addX (- (* cx expr) (* cy expi)))
                 (+ addY (* cx expi) (* cy expr))))))))

(defn- split-into-ranges
  "splits `n` into `rsize`-sized ranges covering 0 to `n`"
  [n rsize]
  (let [rs (range 0 n rsize)]
    (map range rs (concat (rest rs) [n]))))

(defn fill-image
  "Fill a `BufferedImage` `img` with a picture of a fractal `alg` in
  the color scheme `scheme` for the coordinates given in the
  map."
  [^BufferedImage img ^Algorithm alg scheme
   {^double algx-min :xmin ^double algx-max :xmax
    ^double algy-min :ymin ^double algy-max :ymax}]
  (let [xmax (.getWidth img)
        ymax (.getHeight img)
        x-scale (/ (- algx-max algx-min) xmax)
        y-scale (/ (- algy-max algy-min) ymax)
        color-scale (/ (colors/depth scheme) (.fidelity alg))]
    (dorun
     (pmap (fn [yrange]
             (doseq [y yrange,  x (range xmax)]
               (let [algx  (+ algx-min (* x x-scale))
                     algy  (+ algy-min (* y y-scale))
                     ptval (.point alg algx algy)
                     scaled (int (*  ptval color-scale))]
                 (.setRGB img x y (colors/get-color scheme scaled)))))
           (split-into-ranges ymax 50)))
    img))

;; ~~~~ END faster math for this part of the file ~~~
(set! *unchecked-math* false)

