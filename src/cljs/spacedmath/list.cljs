(ns spacedmath.list)

(def math-list
  [
    [:equal [:fn \f \x] [:power 2 40]]
    [:equal [:fn \f \x] [:exp 5]]
    [:equal [:fn \f \x] [:add [:mult 5.2 \x] 2.3]]
    [:equal [:fn \g \x] [:add [:mult [:div 7 4] [:power \x 2]] [:mult -3 \x] 12]]
    [:equal [:fn \f \t] [:add [:add [:mult 2 [:power \t 3]] [:mult -3 [:power \t 2]] [:mult -4 \t]]]]
    [:equal [:fn \f \t] [:add [:add [:mult 1.4 [:power \t 5]] [:mult -2.5 [:power \t 2]] 6.7]]]
    [:equal [:fn \g \x] [:add [:add [:power \x 2] [:mult -2 [:power \x 3]]]]]
    [:equal [:fn \g \t] [:mult 2 [:power \t [:mult -1 [:div 3 4]]]]]
    [:equal [:fn \f \y] [:mult \c [:power \y -6]]]
    [:equal [:fn \F \r] [:div 5 [:power \r 3]]]
    [:equal \y [:add [:power \x [:div 5 3]] [:mult -1 [:power \x [:div 2 3]]]]]
    [:equal [:fn \h \t] [:add [:root \t 4] [:mult -4 [:exp \t]]]]])
