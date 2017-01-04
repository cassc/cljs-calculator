(ns calculator.core
  (:require
   [clojure.string :as s]
   [reagent.core :as reagent :refer [atom]]
   [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defonce input-state (atom []))
(defonce result-state (atom nil))
(defonce ans-state (atom nil))

(defn reset-calc! []
  (reset! input-state [])
  (reset! result-state nil)
  (reset! ans-state nil))

(defonce error-state (atom false))
(defn calculate! []
  (try
    (let [form (apply str @input-state)
          r (js/eval form)]
      (reset! input-state [r])
      (reset! result-state r)
      (reset! ans-state r))
    (catch js/Error e
      (reset! error-state true))))

(def cal-keys
  {:ac      "AC"
   :ce      "CE"
   :percent "%"
   :divide  "/"
   :times   "*"
   :minus   "-"
   :plus    "+"
   :dot     "."
   :ans     "ANS"
   :equal   "="})

(defn key-funcs [k]
  (fn []
    (reset! result-state nil)
    (reset! error-state false)
    
    (cond
      (number? k)
      (swap! input-state conj k)

      (= :ac k)
      (reset-calc!)

      (= :ce k)
      (swap! input-state pop)
      

      (= :ans k)
      (when @ans-state
        (swap! input-state conj @ans-state))
      
      (= :equal k)
      (calculate!)

      :else
      (swap! input-state conj (cal-keys k)))))

(defn make-row []
  (fn [keys]
    [:div.row
     (for [k keys]
       ^{:key (str k)}
       [:a {:href "javascript:;"} [:div.key {:on-click (key-funcs k)} (cal-keys k k)]])]))

(defn make-input [in result]
  (or result (apply str in)))

(defn input-display []
  (fn []
    (let [in @input-state
          result @result-state]
      [:input.cal-input {:type :text :disabled true :value (make-input in result)}])))

(defn state-display []
  [:div.row.ans-display
   [:span (if @error-state
            [:span.error "Invalid expression"]
            (str "ANS=" @ans-state))]])

(defn my-app []
  (fn []
    [:div.cal-holder
     [:h3 "Online Calulator"]
     [input-display]
     [state-display]
     [make-row [:ac :ce :percent :divide]]
     [make-row [7 8 9 :times]]
     [make-row [4 5 6 :minus]]
     [make-row [1 2 3 :plus]]
     [make-row [:dot 0 :ans :equal]]]))

(defn main []
  (reagent/render [#'my-app] (.getElementById js/document "app")))

(main)

