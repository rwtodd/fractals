(ns org-rwtodd.fractals.core
  (:import (javax.swing JFrame JLabel JMenuBar ImageIcon
                        SwingUtilities JMenu JMenuItem
                        JRadioButtonMenuItem ButtonGroup
                        JFileChooser JDialog JPanel JButton
                        Box BoxLayout BorderFactory
                        JTextArea JScrollPane)
           (java.awt Color BorderLayout Dimension)
           (java.awt.image BufferedImage)
           (java.awt.event MouseAdapter ActionListener WindowEvent))
  (:require [org-rwtodd.fractals.colors :as colors]
            [org-rwtodd.fractals.algo :as algo]
            [clojure.edn :as edn])
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

(defn- state-string
  "Write the important state from `st` to an edn string, returning it."
  [st]
  (pr-str (select-keys st #{:in-fractal :in-scheme :center :size :image-size})))

(defn- evaluate-state
  "fill out the application state by evaluating the inputs."
  [st]
  (let [[sx sy] (:image-size st)
        existing-img (:image st)
        curns (find-ns 'org-rwtodd.fractals.core)]
    (assoc st
           :fractal (binding [*ns* curns]
                      (eval (read-string (:in-fractal st))))
           :scheme  (colors/vectorize-scheme
                     (binding [*ns* curns]
                       (eval (read-string (:in-scheme st)))))
           :image   (if (and existing-img
                             (= (.getWidth existing-img) sx)
                             (= (.getHeight existing-img) sy))
                      existing-img
                      (BufferedImage. sx sy BufferedImage/TYPE_INT_RGB)))))

(def app-state (atom (evaluate-state starter-spec)))

(defn generate-image
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

(defn recenter-on-image!
  [x y]
  (let [st @app-state
        [cx cy] (:center st)
        [szx szy] (:size st)
        [imwid imht] (:image-size st)
        newx (- cx (- szx (* (/ (double x) imwid) 2 szx)))
        newy (- cy (- szy (* (/ (double y) imht) 2 szy)))]
    (recenter! newx newy (:click-scale st))))

(defn change-inputs!
  "Update the global state's existing keys with any provided."
  [& kvs]
  (let [st (swap! app-state
                  (fn [s]
                    (evaluate-state (apply assoc s kvs))))]
    (if-let [frm (:swing-frame st)]
      (do
        (generate-image)
        (.setIcon (:swing-label st) (ImageIcon. (:image st)))
        (doto frm .pack .repaint)))))

(defn- algorithm-dialog
  [st]
  (let [dlg (JDialog. (:swing-frame st) "Algorithm Settings" true)
        inputs (Box. BoxLayout/PAGE_AXIS)
        frac-txt (JTextArea. (:in-fractal st))
        scheme-txt (JTextArea. (:in-scheme st))
        btns (Box. BoxLayout/LINE_AXIS)
        okbtn (JButton. "Ok")
        cbtn  (JButton. "Cancel")
        handlers (reify ActionListener
                   (actionPerformed [_ ae]
                     (case (.getActionCommand ae)
                       "Ok" (change-inputs! :in-fractal (.getText frac-txt)
                                            :in-scheme (.getText scheme-txt))
                       "Cancel" nil
                       (println (.getActionCommand ae)))
                     (.dispatchEvent dlg (WindowEvent. dlg WindowEvent/WINDOW_CLOSING))))]
    (.. dlg getContentPane (setLayout (BorderLayout.)))
    ;; setup the input boxes
    (doto inputs
      (.setBorder (BorderFactory/createEmptyBorder 5 5 5 5))
      (.add (JLabel. "Algorithm:"))
      (.add (JScrollPane. frac-txt))
      (.add (Box/createRigidArea (Dimension. 0 5)))
      (.add (JLabel. "Color Scheme:"))
      (.add (JScrollPane. scheme-txt)))

    ;; setup the OK, Cancel buttons
    (.addActionListener okbtn handlers)
    (.addActionListener cbtn handlers)
    (doto btns
      (.setBorder (BorderFactory/createEmptyBorder 5 5 5 5))
      (.add (Box/createHorizontalGlue))
      (.add okbtn)
      (.add (Box/createRigidArea (Dimension. 5 0)))
      (.add cbtn))

    (doto dlg
      (.add btns BorderLayout/SOUTH)
      (.add inputs)
      .pack
      .show)))

(defn- create-settings-menu
  "Create the settings options in a `Settings` menu, and return it."
  []
  (let [mm (JMenu. "Settings")
        alg (JMenuItem. "Algorithm")
        por (JMenuItem. "Viewport")
        poract (fn [] (println "port!"))
        listener  (reify ActionListener
                    (actionPerformed [_ ae]
                      (case (.getActionCommand ae)
                        "Algorithm" (algorithm-dialog @app-state)
                        "Viewport"  (poract)
                        nil)))]
    (.addActionListener alg listener)
    (.addActionListener por listener)
    (doto mm (.add alg) (.add por))))

(defn- create-file-menu
  "Create the file options in a `File` menu, and return it."
  []
  (let [mm (JMenu. "File")
        save (JMenuItem. "Save As")
        load (JMenuItem. "Load")
        saveact (fn []
                  (let [st @app-state
                        jfc (JFileChooser.)]
                    (when (= JFileChooser/APPROVE_OPTION
                           (.showSaveDialog jfc (:swing-frame st)))
                      (spit (.getSelectedFile jfc) (state-string st)))))
        loadact  (fn []
                   (let [frm (:swing-frame @app-state)
                         jfc (JFileChooser.)]
                     (when (= JFileChooser/APPROVE_OPTION
                            (.showOpenDialog jfc frm))
                         (let [nst (swap! app-state
                                          merge
                                          (evaluate-state
                                           (edn/read-string (slurp (.getSelectedFile jfc)))))]
                           (generate-image)
                           (.setIcon (:swing-label nst) (ImageIcon. (:image nst)))
                         (doto frm .pack .repaint)))))
        listener  (reify ActionListener
                    (actionPerformed [_ ae]
                      (case (.getActionCommand ae)
                        "Save As" (saveact)
                        "Load"    (loadact)
                        nil)))]
    (.addActionListener save listener)
    (.addActionListener load listener)
    (doto mm (.add save) (.add load))))
    
(defn- create-scale-menu
  "Create the zooming options in a `Scale` menu, and return it."
  []
  (let [mm (JMenu. "Scale")
        zi (JRadioButtonMenuItem. "Zoom In")
        zo (JRadioButtonMenuItem. "Zoom Out")
        nz (JRadioButtonMenuItem. "No Zoom" true)
        bgrp (ButtonGroup.)
        zooms (reify ActionListener
                (actionPerformed [_ ae]
                  (swap! app-state assoc
                         :click-scale
                         (case (.getActionCommand ae)
                           "Zoom In" 0.5
                           "Zoom Out" 2.0
                           "No Zoom" 1.0
                           1.0))))]
    (.addActionListener zi zooms)
    (.addActionListener zo zooms)
    (.addActionListener nz zooms)
    (doto bgrp (.add zi) (.add nz) (.add zo))
    (doto mm (.add zi) (.add nz) (.add zo))))

(defn- add-menus
  "Put the menu items on the frame"
  [frm]
  (let [m (JMenuBar.)
        fmenu  (create-file-menu)
        stmenu (create-settings-menu)
        scmenu (create-scale-menu)]
    (doto m (.add fmenu) (.add stmenu) (.add scmenu))
    (.setJMenuBar frm m)))

(defn- click-handler
  "handle clicks on the main frame"
  [x y]
  (recenter-on-image! x y)
  (generate-image))

(defn generate-frame
  "Start the main frame--must be called from swing thread"
  []
  (generate-image)
  (let [frm (JFrame. "Fractals")
        pane (.getContentPane frm)
        icon (ImageIcon. (:image @app-state))
        lbl (JLabel. icon)]
    (swap! app-state assoc
           :swing-frame frm
           :swing-label lbl
           :click-scale 1.0)
    (add-menus frm)
    (.addMouseListener lbl
                       (proxy [MouseAdapter] []
                         (mousePressed [event]
                           (click-handler (.getX event) (.getY event))
                           (.repaint lbl))))
    (.setLayout pane (BorderLayout.))
    (.add pane lbl)
    (doto frm .pack (.setVisible true))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (SwingUtilities/invokeLater generate-frame))

