(ns org-rwtodd.fractals.colors
  (:import (java.awt Color))
  (:gen-class))

(defprotocol ColorScheme
  (depth [cs])
  (get-color [cs x]))

;; VectorColorSchemes should always have rgb values, *not* Colors
(deftype VectorColorScheme [v]
  ColorScheme
  (depth [_] (count v))
  (get-color [_ x] (nth v x)))

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

(defn vectorize-scheme
  "Convert any `ColorScheme` into an optimized vector-based scheme via enumeration"
  [cs]
  (if (instance? VectorColorScheme cs)
    cs
    (->VectorColorScheme
     (into []
           (map (comp to-rgb (partial get-color cs)))
           (range (depth cs))))))

(deftype FnColorScheme [max f]
  ColorScheme
  (depth [cs] max)
  (get-color [cs x] (f x)))

(defn- single-gradient-scheme
  "Create a `ColorScheme` by gradually transitioning
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
    (FnColorScheme.
     n
     (fn [x] (let [amt (/ x divisor)]
               (Color. (int (+ r0 (* amt rdiff)))
                       (int (+ g0 (* amt gdiff)))
                       (int (+ b0 (* amt bdiff)))))))))

(defn gray-scheme
  "Create a `ColorScheme` which is `max` levels of gray
  (from 0 to max-1)."
  [max]
  (single-gradient-scheme max Color/BLACK Color/WHITE))

(defn- monochrome-scheme
  "Create a `ColorScheme` which is `max` levels which
  proceed from black to the given color `c`"
  [max c]
  (single-gradient-scheme max Color/BLACK c))

(defn- to-scheme
  "Leaves `ColorScheme`s alone, and converts single integers-or-colors
  into single-entry `ColorScheme`s."
  [s]
  (cond
    (satisfies? ColorScheme s) s
    (integer? s) (->VectorColorScheme [s])
    (instance? Color s) (->VectorColorScheme [(.getRGB s)])
    :else  (throw (IllegalArgumentException. "Can't convert to a colorscheme!"))))

(defn combine-schemes
  "combines a series of `ColorScheme` objects by concatenating their
  ranges"
  ([a] (to-scheme a))
  ([a b]
   (let [a     (to-scheme a)
         b     (to-scheme b)
         mxa   (depth a)
         total (+ mxa (depth b))]
     (->FnColorScheme
      total
      (fn [x] (if (< x mxa)
                (get-color a x)
                (get-color b (- x mxa)))))))
  ([a b & more]
   (reduce combine-schemes (combine-schemes a b) more)))

(defn- skip-first-scheme
  "Create a derived `ColorScheme` that skips the first entry in the given scheme. This should
  be mainly useful in constructing multi-part gradients, since you don't want the first element
  of the next gradient to match the last element of the current one."
  [cs]
  (->FnColorScheme
   (dec (depth cs)) 
   (fn [x] (get-color cs (inc x)))))
    
(defn gradient-scheme
  "Define a `ColorScheme` that interpolates across many colors. If only one color is
  given, then transition from BLACK to the color."
  ([n color]
   (monochrome-scheme n color))
  
  ([n color-a & colors]
   (let [grads    (partition 2 1 (map to-color (cons color-a colors)))
         gradcnt  (count grads)
         wrappers (cons identity (repeat skip-first-scheme))
         per      (int (/ n gradcnt))
         sizes    (cons (- n (* (dec gradcnt) per))
                        (repeat (inc per)))]
     (apply combine-schemes
            (map (fn [[color-1 color-2] size wrapper]
                   (wrapper (single-gradient-scheme size color-1 color-2)))
                 grads
                 sizes
                 wrappers)))))
