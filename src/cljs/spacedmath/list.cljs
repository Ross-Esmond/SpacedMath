(ns spacedmath.list)

(def math-list
  [
    [:equal \y [:power 2 40]]
    [:equal \y [:exp 5]]
    [:equal \y [:add [:mult 5.2 \x] 2.3]]
    [:equal \y [:add [:mult [:div 7 4] [:power \x 2]] [:mult -3 \x] 12]]
    [:equal \y [:add [:add [:mult 2 [:power \t 3]] [:mult -3 [:power \t 2]] [:mult -4 \t]]]]
    [:equal \y [:add [:add [:mult 1.4 [:power \t 5]] [:mult -2.5 [:power \t 2]] 6.7]]]
    [:equal \y [:add [:add [:power \x 2] [:mult -2 [:power \x 3]]]]]
    [:equal \y [:mult 2 [:power \t [:mult -1 [:div 3 4]]]]]
    [:equal \y [:mult \c [:power \y -6]]]])
