(ns ^:figwheel-hooks covid.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [goog.string :as gstring]
   [goog.string.format]))



(def cases [3509 3739 159
            7630 8133 204
            13489 14965 382
            10539 11784 366
            11437 10791 287
            12518 12843 215
            5742 6451 94
            4343 4374 52
            4501 2810 31
            1441 453 9
            36 9 1])


(def death [1 0 0
            0 2 0
            2 4 0
            2 4 0
            6 15 0
            31 77 0
            81 212 1
            279 518 1
            687 688 0
            331 163 0
            9 1 0])


(defn combine [i]
  (map #(reduce + %)
       (partition 3 i)))


(def deathrates
  (map (fn [c d] (/ d c))
       (combine cases) (combine death)))


(defn prob-or
  ([x] x)
  ([x & t] (- 1.0 (* (- 1.0 x)
                     (- 1.0 (apply prob-or t))))))

(defn prob-pow [x n]
  (- 1.0 (Math/pow (- 1.0 x) n)))


(defn prob [dr num]
  (apply prob-or (map prob-pow dr num)))


(def persons (r/atom [0 0 0 0 0 0 0 0 0 0 0]))

(def probabilities
  (partition 2
  [0 "You live forever"
   0.5 "Winning a coing toss"
   1 "You die one day"
   (/ 1.0 6) "Rolling a six in one try"
   (prob-pow (/ 1.0 6) 3) "Rolling a six in three tries"
   (/ 1.0 36) "Two six with two dices"
   ;; (/ 6.0 36) "Rolling doubles with two dices"
   0.003  "Guessing someones birthday on the first try"
   0.34 "Drawing an ace in 5 cards"
   0.8 "Guessing a card in four tries"
   0.0001 "Guessing a pin on the frist try"
   0.438 "A pair in texas hold'em"
   0.235 "Two pairs in texas hold'em"
   0.0483 "Three of a kind in texas hold'em"
   0.0260 "Full house in texas hold'em"
   0.00168067 "Four of a kind in texas hold'em"
   0.00027851 "Straight flush in texas hold'em"
   ]))

probabilities


(defn box [param value]
  [:input {:type "text" :value value
           :style {:width "40pt"}
           :on-change (fn [e]
                        (let [new-value (js/parseInt (.. e -target -value))
                              valid? (and (>= new-value 0)
                                          (<= new-value) 1000)]
                          (if valid?
                            (swap! persons
                                   (fn [data] (assoc data param new-value)))
                            (swap! persons (fn [data] (assoc data param 0))))))}])

(defn button [param]
  [:input {:type "button" :value "+"
          :on-click #(swap! persons
                            (fn [data]
                              (assoc data param (inc (nth data param)))))}])


(defn create-slider [n]
  ( let [age-low  (* n 10)
         age-high (+ age-low 9)
         count (nth @persons n)]
   [:tr
    [:td (str age-low " to " age-high)]
    [:td (gstring/format "%.3f" (* 100 (nth deathrates n))) " %"]
    [:td [box n count] [button n]]]))

(defn get-comment [p]
  (let [more-likely (filter #(<= (first %) p) probabilities)
        entry (last (sort #(compare (first %1) (first %2))  more-likely))]
    (second entry)))


(defn covid []
  (let [prip (prob deathrates @persons)
        comment (get-comment prip)]
    [:div
     [:table
      (for [n (range 0 11)]
        [create-slider n])]
     [:p (gstring/format "%.2f" (* 100 prip)) " %"]
     [:p "It is more likely that someone of the people on the list will die then" [:br]
      comment]]))

(defn ^:export run []
  (rdom/render [covid] (js/document.getElementById "app-covid")))


(run)

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

