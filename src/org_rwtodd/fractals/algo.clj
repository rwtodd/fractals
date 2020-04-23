(ns org-rwtodd.fractals.algo
  (:import [java.awt.image BufferedImage])
  (:require [org-rwtodd.fractals.colors :as colors])
  (:gen-class))

(definterface Algorithm
  (^int fidelity [])
  (^int point [^double x ^double y]))

;; ~~~~ START faster math for this part of the file ~~~
(set! *unchecked-math* true)

;; TODO: try more formulas from http://www.lifesmith.com/formulas.html

;; Formula:  F(Z) = Z^2 + Z_0
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

;; Formula:  F(Z) = Z^2 -Z + Z_0
(deftype Z2-Z+Z0 [depth esc]
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
                   (+ x (- xsq ysq cx))
                   (+ y tmp tmp (- cy)))))))))

;; Formula: F(Z) = Z^3 + Z_0
(deftype Z3+Z0 [depth esc]
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
          (recur (dec ans)
                 (+ x (- (* xsq cx) (* 3 cx ysq)))
                 (+ y (- (* 3 cy xsq) (* ysq cy)))))))))

;; Formula: F(Z) = Z^4 + Z_0
(deftype Z4+Z0 [depth esc]
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
          (recur (dec ans)
                 (+ x (+ (* xsq xsq) (* -6 xsq ysq) (* ysq ysq)))
                 (+ y (* 4 (- (* cy xsq cx) (* cx ysq cy))))))))))


;; Formula: F(Z) = Z^5 + Z_0
(deftype Z5+Z0 [depth esc]
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
          (let [cx4 (* xsq xsq),  cy4 (* ysq ysq)]
          (recur (dec ans)
                 (+ x (* cx4 cx) (* -10 xsq cx ysq) (* 5 cx cy4))
                 (+ y (* 5 cy cx4) (* -10 xsq ysq cy) (* cy4 cy)))))))))

;; Formula:  F(Z) = Z^2 + C  ;; C = (addX, addY)
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

;; Formula:  F(Z) = exp(Z) + C  ;; C = (addX, addY)
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

;; Formula: F(Z) = Z*exp(Z) + C ;; C = (addX, addY)
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

