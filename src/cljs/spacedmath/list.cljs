(ns spacedmath.list)

(def math-list
  [
    [:power 2 40]
    [:exp 5]
    [:add [:mult 5.2 \x] 2.3]
    [:add [:mult [:div 7 4] [:power \x 2]] [:mult -3 \x] 12]
    [:add [:add [:mult 2 [:power \t 3]] [:mult -3 [:power \t 2]] [:mult -4 \t]]]
    [:add [:add [:mult 1.4 [:power \t 5]] [:mult -2.5 [:power \t 2]] 6.7]]
    [:add [:add [:power \x 2] [:mult -2 [:power \x 3]]]]
    [:mult 2 [:power \t [:mult -1 [:div 3 4]]]]])
