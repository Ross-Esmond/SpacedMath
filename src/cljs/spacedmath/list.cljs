(ns spacedmath.list)

(def math-list
  [
    [:power 2 40]
    [:exp 5]
    [:add [:mult 5.2 :x] 2.3]
    [:add [:mult [:div 7 4] [:power :x 2]] [:mult -3 :x] 12]])
