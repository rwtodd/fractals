(ns org-rwtodd.fractals.colors
  (:refer-clojure :exclude [concat])
  (:import (java.awt Color))
  (:gen-class))

(def ega-colors
  "The EGA 16-Color Palette"
  [0x0 0xaa 0xaa00 0xaaaa 0xaa0000 0xaa00aa 0xaa5500 0xaaaaaa
   0x555555 0x5555ff 0x55ff55 0x55ffff 0xff5555 0xff55ff 0xffff55 0xffffff])

(defn- to-rgb
  "Convert Colors to RGB values, or leave it alone if it's an integer already."
  [c]
  (if (integer? c)
    c
    (.getRGB c)))

(defn- to-color
  "Convert integers to Color, or leave it alone if it's a Color already."
  [c]
  (if (integer? c)
    (Color. c)
    c))

(defn scheme-to-rgb
  "Convert any sequence of colors into an optimized vector-based scheme via enumeration"
  [cs]
  (into [] (map to-rgb) cs))

(defn- single-gradient-scheme
  "Create a color sequence by gradually transitioning
  from colors `a` to `b`, in `n` steps. No fancy models,
  just interpolate the R,G,B values."
  [n a b]
  (let [r0      (.getRed a)
        g0      (.getGreen a)
        b0      (.getBlue a)
        rdiff   (- (.getRed b) r0)
        gdiff   (- (.getGreen b) g0)
        bdiff   (- (.getBlue b) b0)
        divisor (dec n)]
    (map (fn [x] (let [amt (/ x divisor)]
                   (Color. (int (+ r0 (* amt rdiff)))
                           (int (+ g0 (* amt gdiff)))
                           (int (+ b0 (* amt bdiff))))))
         (range n))))

(defn- gray-scheme
  "Create a color sequence which is `max` levels of gray
  (from 0 to max-1)."
  [max]
  (single-gradient-scheme max Color/BLACK Color/WHITE))

(defn- monochrome-scheme
  "Create a color sequence which is `max` levels which
  proceed from black to the given color `c`"
  [max c]
  (single-gradient-scheme max Color/BLACK c))

(defn- to-scheme
  "Leaves sequences alone, and converts single integers-or-colors
  into single-entry sequences"
  [s]
  (cond
    (seq? s)            s
    (integer? s)        [(Color. s)]
    (instance? Color s) [s]
    :else  (throw (IllegalArgumentException. (str "Can't convert " s " to a color sequence!")))))

(defn concat
  "combines a series of color sequences by concatenating their
  ranges"
  [& schemes] (mapcat to-scheme schemes))

(defn gradient
  "Define a color sequence that interpolates across many colors. If only one color is
  given, then transition from BLACK to the color. If no colors are given, a gray sequence
  is created."
  ([n] (gray-scheme n))
  ([n color] (monochrome-scheme n (to-color color)))
  ([n color-a & colors]
   (let [per      (int (/ n (count colors)))
         first-seq (single-gradient-scheme (- n (* (dec (count colors)) per))
                                           (to-color color-a)
                                           (to-color (first colors)))
         grads    (->> colors
                       (map to-color)
                       (partition 2 1)
                       (map (fn [[c1 c2]] (single-gradient-scheme (inc per) c1 c2)))
                       rest)]
     (apply clojure.core/concat first-seq grads))))
