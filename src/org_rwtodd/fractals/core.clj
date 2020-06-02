(ns org-rwtodd.fractals.core
  (:import (javax.imageio ImageIO)
           (javax.swing JFrame JLabel JMenuBar ImageIcon
                        SwingUtilities JMenu JMenuItem
                        JRadioButtonMenuItem ButtonGroup
                        JFileChooser JDialog JPanel JButton
                        Box BoxLayout BorderFactory
                        JTextArea JScrollPane JTextField
                        WindowConstants JSeparator
                        JList ListCellRenderer)
           (javax.swing.filechooser FileNameExtensionFilter)
           (java.awt Color BorderLayout Dimension)
           (java.awt.image BufferedImage)
           (java.awt.event MouseAdapter ActionListener WindowEvent)
           (java.util.prefs Preferences))
  (:require [org-rwtodd.fractals.colors :as colors]
            [org-rwtodd.fractals.algo :as algo]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))


;; Preferences to remember where we last loaded or saved fractals
(def last-save-loc (let [node (.node (Preferences/userRoot) "org_rwtodd/fractals")]
                     (atom { :prefs node :value (.get node "LastDir" nil) })))

(defn- update-save-loc!
  "Update the value of the save location from a provided JFileChooser, and save
  it to the preferences node if needed."
  [^JFileChooser jfc]
  (let [^java.io.File selected (.getSelectedFile jfc)
        loc (str (.getParentFile selected))]
    (swap! last-save-loc
           (fn [orig]
             (when-not (= (:value orig) loc)
               (.put (:prefs orig) "LastDir" loc))
             (assoc orig :value loc)))))

(defn- configure-file-chooser
  "Set up a file chooser for an `extension` and use the `@last-save-loc` if available"
  [extension]
  (let [jfc (JFileChooser.)
        filt (FileNameExtensionFilter. (str extension " Files") (into-array String [extension]))]
    (.setFileFilter jfc filt)
    (when-let [dir (:value @last-save-loc)]
      (.setCurrentDirectory jfc (io/file dir)))
    jfc))
      
(defn- labelled-widget
  "Creates a lbl followed by the given widget in parent.
  Returns the widget instance."
  [parent lbl widget]
  (let [l    (JLabel. lbl)
        lbox (Box. BoxLayout/LINE_AXIS)]
    (doto lbox (.add l) (.add (Box/createHorizontalGlue)))
    (doto parent
      (.add lbox)
      (.add widget))
    widget))

(defn- ensure-extension
  "Make sure the file f ends in .ext, otherwise add it.  Returns
  the new file"
  [f ext]
  (if (str/ends-with? (str f) ext)
    f
    (io/file (str f ext))))        

(def starter-spec
  "Default starting point for fractal exploration"
  {
   :in-fractal "(algo/->Alg0001 32 4.0)"
   :in-scheme
 "(colors/combine-schemes 0
                         (colors/gradient-scheme 255 0xff0000 0xddff00 0x0000ff))"
   :center [-0.5 0.0]
   :size [1.366 0.768]
   :image-size [683 384]
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
        curns (find-ns 'org-rwtodd.fractals.core)
        eval-frac (or (:fractal st)
                      (binding [*ns* curns]
                        (eval (read-string (:in-fractal st)))))
        eval-cols (or (:scheme st)
                      (colors/vectorize-scheme
                       (binding [*ns* curns]
                         (eval (read-string (:in-scheme st))))))]
    (assoc st
           :fractal eval-frac
           :scheme  eval-cols
           :image   (if (and existing-img
                             (= (.getWidth existing-img) sx)
                             (= (.getHeight existing-img) sy))
                      existing-img
                      (BufferedImage. sx sy BufferedImage/TYPE_INT_RGB)))))

(def app-state (atom nil))

(defn reset-state!
  "Reset the application state to a known-good starter state."
  []
  (reset! app-state (evaluate-state starter-spec)))

(defn generate-image
  "Generate the image specified by the given state, into the given state's `:image`."
  [st]
  (let [[cx cy]    (:center st)
        [szx szy]  (:size   st)]
    (algo/fill-image (:image st) (:fractal st) (:scheme st)
                     {:xmin (- cx szx) :xmax (+ cx szx)
                      :ymin (- cy szy) :ymax (+ cy szy)})))

(defn recenter!
  "Adjust the global state for a new center `x` and `y`, with optional
  rescaling by `scale`.  Returns the new application state."
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
  (let [st            @app-state
        [cx cy]       (:center st)
        [szx szy]     (:size st)
        [imwid imht]  (:image-size st)
        newx          (- cx (- szx (* (/ (double x) imwid) 2 szx)))
        newy          (- cy (- szy (* (/ (double y) imht) 2 szy)))
        st            (recenter! newx newy (:click-scale st))]
    (generate-image st)))

(defn change-inputs!
  "Update the global state's existing keys with any provided."
  [& kvs]
  (let [st (swap! app-state
                  (fn [s]
                    (evaluate-state (apply assoc s kvs))))]
    (if-let [frm (:swing-frame st)]
      (do
        (generate-image st)
        (.setIcon (:swing-label st) (ImageIcon. (:image st)))
        (doto frm .pack .repaint)))))

(def algorithm-list
  (delay
    (->>
     (into []
           (comp  (filter (fn [[k v]] (str/starts-with? (str k) "->")))
                  (map (fn [[k v]] (assoc (select-keys (meta v) [:doc :arglists]) :name (str "algo/" k)))))
           (ns-publics 'org-rwtodd.fractals.algo))
     (sort-by :name)
     (to-array))))

(defn- algorithm-list-dialog
  [st]
  (let [dlg (JDialog. (:swing-frame st) "Algorithm List" false)
        renderer (let [lbl (doto (JLabel.)
                             (.setOpaque true)
                             (.setBorder (BorderFactory/createLineBorder Color/BLACK)))]
                   (reify ListCellRenderer
                     (getListCellRendererComponent [_ lst v _ _ focused?]
                       (doto lbl
                         (.setText (format "<html><h3>%s</h3><p>%s<br>%s</p></html>" (:name v) (:doc v) (:arglists v)))
                         (.setBackground  (if focused? (.getSelectionBackground lst) (.getBackground lst)))
                         (.setForeground  (if focused? (.getSelectionForeground lst) (.getForeground lst)))))))
        ^"[Ljava.lang.Object;" algs  @algorithm-list
        jlst (doto (JList. algs) (.setCellRenderer renderer))
        jsp  (JScrollPane. jlst)]
    (doto dlg
      (.setContentPane jsp)
      .pack
      .show)))

(defn- algorithm-dialog
  [st]
  (let [dlg (JDialog. (:swing-frame st) "Algorithm Settings" false)
        inputs (Box. BoxLayout/PAGE_AXIS)
        frac-txt (JTextArea. (:in-fractal st))
        scheme-txt (JTextArea. (:in-scheme st))
        btns (Box. BoxLayout/LINE_AXIS)
        lbtn (JButton. "Alg List")
        abtn (JButton. "Apply")
        qbtn (JButton. "Quit")
        doapply (fn []
                  (let [astate @app-state
                        afrac (.getText frac-txt)
                        aschm (.getText scheme-txt)
                        changed (concat
                                 (if-not (= afrac
                                            (:in-fractal astate))
                                   [:in-fractal afrac :fractal nil])
                                 (if-not (= aschm
                                            (:in-scheme astate))
                                   [:in-scheme aschm :scheme nil]))]
                        (when (seq changed) (apply change-inputs! changed))))            
        handlers (reify ActionListener
                   (actionPerformed [_ ae]
                     (case (.getActionCommand ae)
                       "Alg List"  (algorithm-list-dialog st)
                       "Apply"     (doapply)
                       "Quit"      (.dispatchEvent dlg (WindowEvent. dlg WindowEvent/WINDOW_CLOSING))
                       (println (.getActionCommand ae)))))]
    (.. dlg getContentPane (setLayout (BorderLayout.)))
    ;; setup the input boxes
    (.setBorder inputs (BorderFactory/createEmptyBorder 5 5 5 5))
    (labelled-widget inputs "Algorithm:" (JScrollPane. frac-txt))
    (.add inputs (Box/createRigidArea (Dimension. 0 5)))
    (labelled-widget inputs "Color Scheme:" (JScrollPane. scheme-txt))

    ;; setup the Apply Quit buttons
    (.addActionListener lbtn handlers)
    (.addActionListener abtn handlers)
    (.addActionListener qbtn handlers)
    (doto btns
      (.setBorder (BorderFactory/createEmptyBorder 5 5 5 5))
      (.add (Box/createHorizontalGlue))
      (.add lbtn)
      (.add  (Box/createRigidArea (Dimension. 5 0)))
      (.add abtn)
      (.add  (Box/createRigidArea (Dimension. 5 0)))
      (.add qbtn))

    (doto dlg
      (.add btns BorderLayout/SOUTH)
      (.add inputs)
      .pack
      .show)))

(defn- viewport-dialog
  [st]
  (let [dlg (JDialog. (:swing-frame st) "Viewport Settings" true)
        inputs (Box. BoxLayout/PAGE_AXIS)
        center-txt  (JTextField. (pr-str (:center st)))  
        size-txt    (JTextField. (pr-str (:size st)))
        imgsize-txt (JTextField. (pr-str (:image-size st)))
        btns (Box. BoxLayout/LINE_AXIS)
        abtn (JButton. "Apply")
        qbtn (JButton. "Quit")
        handlers (reify ActionListener
                   (actionPerformed [_ ae]
                     (case (.getActionCommand ae)
                       "Apply" (change-inputs! :center     (read-string (.getText center-txt))
                                               :size       (read-string (.getText size-txt))
                                               :image-size (read-string (.getText imgsize-txt)))
                       "Quit"  (.dispatchEvent dlg (WindowEvent. dlg WindowEvent/WINDOW_CLOSING))
                       (println (.getActionCommand ae)))))]
    (.. dlg getContentPane (setLayout (BorderLayout.)))
    ;; setup the input boxes
    (.setBorder inputs (BorderFactory/createEmptyBorder 5 5 5 5))
    (labelled-widget inputs "Center Point:" center-txt)
    (.add inputs (Box/createRigidArea (Dimension. 0 5)))
    (labelled-widget inputs "Span Size:" size-txt)
    (.add inputs (Box/createRigidArea (Dimension. 0 5)))
    (labelled-widget inputs "Image Size:" imgsize-txt)

    ;; setup the Apply Quit buttons
    (.addActionListener abtn handlers)
    (.addActionListener qbtn handlers)
    (doto btns
      (.setBorder (BorderFactory/createEmptyBorder 5 5 5 5))
      (.add (Box/createHorizontalGlue))
      (.add abtn)
      (.add (Box/createRigidArea (Dimension. 5 0)))
      (.add qbtn))

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
                        "Viewport"  (viewport-dialog @app-state)
                        nil)))]
    (.addActionListener alg listener)
    (.addActionListener por listener)
    (doto mm (.add alg) (.add por))))

(defn save-image
  "Save the fractal image given in application atate `st` to a
  PNG file named `fn` at size `sz` given as `[x y]`."
  [st sz fn]
  (let [img (if (= sz (:image-size st))
              (:image st)
              (generate-image (assoc st
                                     :image
                                     (BufferedImage. (first sz) (second sz) BufferedImage/TYPE_INT_RGB))))]
    (spit (str fn ".fract") (state-string st))
    (ImageIO/write img "png" (io/file fn))))

(defn- export-dialog
  "Create a dialog box to let you export a picture of the fractal."
  [st]
  (let [dlg (JDialog. (:swing-frame st) "Image Export" true)
        inputs (Box. BoxLayout/PAGE_AXIS)
        sz-txt (labelled-widget inputs
                                "Export Size:"
                                (JTextField. (pr-str (:image-size st))))
        btns (Box. BoxLayout/LINE_AXIS)
        onebtn (JButton. "1x-Size")
        twobtn (JButton. "2x-Size")
        fourbtn (JButton. "4x-Size")
        ebtn (JButton. "Export")
        qbtn (JButton. "Quit")
        handlers (reify ActionListener
                   (actionPerformed [_ ae]
                     (case (.getActionCommand ae)
                       "1x-Size" (.setText sz-txt (pr-str (:image-size st)))
                       "2x-Size" (.setText sz-txt (pr-str (into [] (map #(* 2 %) (:image-size st)))))
                       "4x-Size" (.setText sz-txt (pr-str (into [] (map #(* 4 %) (:image-size st)))))
                       "Export"  (let [jfc (configure-file-chooser "png")]
                                   (.dispatchEvent dlg (WindowEvent. dlg WindowEvent/WINDOW_CLOSING))
                                   (when (= JFileChooser/APPROVE_OPTION
                                            (.showSaveDialog jfc (:swing-frame st)))
                                     (save-image st (read-string (.getText sz-txt)) (ensure-extension (.getSelectedFile jfc) ".png"))
                                     (update-save-loc! jfc)))
                       "Quit"   (.dispatchEvent dlg (WindowEvent. dlg WindowEvent/WINDOW_CLOSING))
                       (println (.getActionCommand ae)))))]
    (.. dlg getContentPane (setLayout (BorderLayout.)))
    (.setBorder inputs (BorderFactory/createEmptyBorder 5 5 5 5))

    ;; setup the buttons
    (doto btns
      (.setBorder (BorderFactory/createEmptyBorder 5 5 5 5))
      (.add (Box/createHorizontalGlue)))
    (doseq [b [onebtn twobtn fourbtn ebtn qbtn]]
      (.addActionListener b handlers)
      (.add btns (Box/createRigidArea (Dimension. 5 0)))
      (.add btns b))

    (doto dlg
      (.add btns BorderLayout/SOUTH)
      (.add inputs)
      .pack
      .show)))

(defn- create-file-menu
  "Create the file options in a `File` menu, on the given frame `frm` and return it."
  [frm]
  (let [mm (JMenu. "File")
        save (JMenuItem. "Save As")
        load (JMenuItem. "Load")
        quit   (JMenuItem. "Exit")
        loadact  (fn []
                   (let [frm (:swing-frame @app-state)
                         jfc (configure-file-chooser "fract")]
                     (when (= JFileChooser/APPROVE_OPTION
                            (.showOpenDialog jfc frm))
                         (let [nst (swap! app-state
                                          merge
                                          (evaluate-state
                                           (edn/read-string (slurp (.getSelectedFile jfc)))))]
                           (update-save-loc! jfc)
                           (generate-image nst)
                           (.setIcon (:swing-label nst) (ImageIcon. (:image nst)))
                           (doto frm .pack .repaint)))))
        listener  (reify ActionListener
                    (actionPerformed [_ ae]
                      (case (.getActionCommand ae)
                        "Save As" (export-dialog @app-state)
                        "Load"    (loadact)
                        "Exit"    (.dispatchEvent frm (WindowEvent. frm WindowEvent/WINDOW_CLOSING))                        
                        nil)))]
    (.addActionListener save listener)
    (.addActionListener load listener)
    (.addActionListener quit  listener)
    (doto mm
      (.add save)
      (.add load)
      (.add (JSeparator.))
      (.add quit))))
    
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
        fmenu  (create-file-menu frm)
        stmenu (create-settings-menu)
        scmenu (create-scale-menu)]
    (doto m (.add fmenu) (.add stmenu) (.add scmenu))
    (.setJMenuBar frm m)))

(defn- click-handler
  "handle clicks on the main frame"
  [x y]
  (recenter-on-image! x y))

(defn generate-frame
  "Start the main frame--must be called from swing thread"
  [in-repl?]
  (generate-image @app-state)
  (let [frm (JFrame. "Fractals")
        pane (.getContentPane frm)
        icon (ImageIcon. (:image @app-state))
        lbl (JLabel. icon)]
    (when-not in-repl? (.setDefaultCloseOperation frm WindowConstants/EXIT_ON_CLOSE))
    (.setIconImage frm (ImageIO/read (io/resource "org_rwtodd/fractals/icon.png")))
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

(defn main-in-repl
  "start up the program, but don't let it quit when the window is closed."
  []
  (reset-state!)
  (SwingUtilities/invokeLater #(generate-frame true)))
  
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (reset-state!)
  (SwingUtilities/invokeLater #(generate-frame false)))

