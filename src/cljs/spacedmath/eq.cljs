(ns spacedmath.eq
  (:require
    [reagent.dom :as rdom]
    [reagent.core :as reagent])
  (:require-macros
    [utils :as ut]))

(defn main-panel []
  (reagent/create-class
    {:reagent-render
      (fn [] "what?")}))

(defn init []
  (let [root-el (.getElementById js/document "app")]
    (rdom/render [main-panel] root-el)))
