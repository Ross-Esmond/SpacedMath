(ns spacedmath.app
  (:require
    [spacedmath.problems :as pr]
    [re-frame.core :as re-frame]
    [reagent.dom :as rdom]
    [reagent.core :as reagent]))



(defn main-panel []
  (reagent/create-class
    {:reagent-render (fn []
      [:div [:h1 "$" + (pr/latex [::pr/exp ::pr/x]) + "$"]])
     :component-did-mount (fn [] (. js/MathJax typeset))
    })
  )

(defn init []
  (let [root-el (.getElementById js/document "app")]
    (rdom/render [main-panel] root-el)))
