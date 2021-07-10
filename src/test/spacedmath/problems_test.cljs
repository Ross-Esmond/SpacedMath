(ns spacedmath.problems-test
  (:require
    [cljs.test :refer-macros [deftest is testing run-tests]]
    [spacedmath.problems :as p :refer [convert]]))

(deftest convert-numbers
  (is (= (convert [:mult -1 [:exp \x]]) [::p/mult -1 [::p/exp \x]])))

(deftest latex
  (is (= (p/latex (convert [:mult -1 [:sin \x]]) "-\\sin\\left(x\\right)"))
      "negative latex printing failed")
  (is (= (p/latex (convert [:mult 3 \x])) "3x") "printing with a scaler multiple failed")
  (is (= (p/latex (convert :pi)) "\\pi") "printing pi failed")
  (is (= (p/latex (convert [:mult 5 2])) "5\\left(2\\right)") "printing two scaler multiples failed")
  (is (= (p/latex 5) "5"))
  (is (= (p/latex (convert [:div 5 4])) "\\frac{5}{4}"))
  (is (= (p/latex (convert \y)) "y"))
  (is (= (p/latex (convert [:mult 5 2 3])) "5\\left(2\\right)\\left(3\\right)")))

(deftest distrinput
  (is (= (p/distrinput [::p/sin ::p/input] \x) [::p/sin \x]))
  (is (= (p/distrinput [::p/sin [::p/cos ::p/input]] \x) [::p/sin [::p/cos \x]])))
        
(deftest find-derives
  (is (= (p/find-derives [::p/add [::p/derive [::p/sin \x]] [::p/derive [::p/cos \x]]]) [[::p/sin \x] [::p/cos \x]])))

(deftest prime-dive
  (is (= (p/prime-dive [::p/exp \x]) {:text [] :skills #{} :answer [::p/exp \x]}))
  (is (= (p/prime-dive [::p/derive [::p/exp \x] \x]) {:text ["Use the identity to find"] :skills #{::p/exp} :answer [::p/exp \x]}))
  (is (= (p/prime-dive [::p/derive [::p/exp \t] \t]) {:text ["Use the identity to find"] :skills #{::p/exp} :answer [::p/exp \t]}))
  (is (= (:skills (p/prime-dive [::p/derive [::p/exp 5] \x])) #{::p/const}))
  (is (= (:answer (p/prime-dive [::p/derive 5 \x]) 0)))
  (is (= (:answer (p/prime-dive [::p/derive \x \x])) 1))
  (is (= (:skills (p/prime-dive [::p/derive [::p/mult 5 \x] \x])) #{::p/scaler}))
  (is (= (:skills (p/prime-dive [::p/derive [::p/mult [::p/div 7 4] \x] \x])) #{::p/scaler}))
  (is (= (:skills (p/prime-dive [::p/derive [::p/add \x \x 5] \x])) #{::p/add ::p/const})))

(deftest variance
  (is (= (p/variance 5) #{}))
  (is (= (p/variance \x) #{\x}))
  (is (= (p/variance \t) #{\t}))
  (is (= (p/variance [::p/add 5 \x]) #{\x})))

(deftest simplify
  (is (= (p/simplify 5) 5))
  (is (= (p/simplify [::p/add 5 0]) 5))
  (is (= (p/simplify [::p/add 0 0]) 0))
  (is (= (p/simplify [::p/mult 5 1]) 5))
  (is (= (p/simplify [::p/mult 1 1]) 1))
  (is (= (p/simplify [::p/mult 5 1 5]) [::p/mult 5 5]))
  (is (= (p/simplify [::p/div 5 1]) 5))
  (is (= (p/simplify [::p/power 5 1]) 5))
  (is (= (p/simplify [::p/power [::p/mult 5 \x] 1]) [::p/mult 5 \x]))
  (is (= (p/simplify [::p/power [::p/mult 1 \x] 1]) \x))
  (is (= (p/simplify [::p/mult [::p/mult 5 5] 5]) [::p/mult 5 5 5]))
  (is (= (p/simplify [::p/add 2 [::p/add 3 4] 5]) [::p/add 2 3 4 5]))
  (is (= (p/simplify [::p/mult 5 -3]) [::p/mult -5 3])))

(deftest insta-add
  (is (= (p/insta-add 2 3) 5))
  (is (= (p/insta-add 2 -3) -1))
  (is (= (p/insta-add [::p/div 4 3] [::p/div 1 3]) [::p/div 5 3]))
  (is (= (p/insta-add [::p/div 4 3] 1) [::p/div 7 3]))
  (is (= (p/insta-add [::p/div 4 3] -1) [::p/div 1 3]))
  (is (= (p/insta-add [::p/div 1 2] [::p/div 1 2]) 1))
  (is (= (p/insta-add [::p/mult -1 [::p/div 1 3]] [::p/div 2 3]) [::p/div 1 3])))

(deftest crunch-numbers
  (is (= (p/crunch-numbers [::p/mult -1 [::p/div 1 3]]) [::p/div -1 3])))
