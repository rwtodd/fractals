(ns org-rwtodd.fractals.colors
  (:import (java.awt Color))
  (:gen-class))

(defprotocol ColorScheme
  (depth [cs])
  (getColor [cs x]))

(deftype VectorColorScheme [v]
  ColorScheme
  (depth [_] (count v))
  (getColor [_ x] (nth v x)))

(defn vectorize-scheme
  "Convert any `ColorScheme` into a `VectorColorScheme` via enumeration"
  [cs]
  (if (instance? VectorColorScheme cs)
    cs
    (VectorColorScheme. (into [] (map (partial getColor cs)) (range (depth cs))))))

(deftype FnColorScheme [max f]
  ColorScheme
  (depth [cs] max)
  (getColor [cs x] (f x)))

(defn single-gradient-scheme
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

(defn monochrome-scheme
  "Create a `ColorScheme` which is `max` levels which
  proceed from black to the given color `c`"
  [max c]
  (single-gradient-scheme max Color/BLACK c))

(defn combine-schemes
  "combines a series of `ColorScheme` objects by concatenating their
  ranges"
  ([a] a)
  ([a b]
   (let [mxa   (.depth a)
         total (+ mxa (.depth b))]
     (FnColorScheme.
      total
      (fn [x] (if (< x mxa)
                (.getColor a x)
                (.getColor b (- x mxa)))))))
  ([a b & more]
   (reduce combine-schemes (combine-schemes a b) more)))

(defn skip-first-scheme
  "Create a derived `ColorScheme` that skips the first entry in the given scheme. This should
  be mainly useful in constructing multi-part gradients, since you don't want the first element
  of the next gradient to match the last element of the current one."
  [cs]
  (FnColorScheme.
   (dec (.depth cs)) 
   (fn [x] (.getColor cs (inc x)))))
    
(defn gradient-scheme
  "Define a `ColorScheme` that interpolates across many colors. If only one color is
  given, then transition from BLACK to the color."
  ([n color]
   (monochrome-scheme n color))
  
  ([n color-a & colors]
   (let [grads    (partition 2 1 (cons color-a colors))
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
