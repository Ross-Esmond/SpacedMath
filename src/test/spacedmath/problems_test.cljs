(ns spacedmath.problems-test
  (:require
    [cljs.test :refer-macros [deftest is testing run-tests]]
    [spacedmath.problems :as p :refer [convert]]))

(deftest convert-numbers
  (is (= (convert [:mult -1 [:exp :x]]) [::p/mult -1 [::p/exp ::p/x]])))

(deftest latex
  (is (= (p/latex (convert [:mult -1 [:sin :x]])) "-\\sin\\left(x\\right)")
      "negative latex printing failed")
  (is (= (p/latex (convert [:mult 3 :x])) "3x") "printing with a scaler multiple failed")
  (is (= (p/latex (convert :pi)) "\\pi") "printing pi failed")
  (is (= (p/latex (convert [:mult 5 2])) "5\\left(2\\right)") "printing two scaler multiples failed")
  (is (= (p/latex 5) "5"))
  (is (= (p/latex (convert [:div 5 4])) "\\frac{5}{4}"))
  (is (= (p/latex (convert :y)) "y")))

(deftest distrinput
  (is (= (p/distrinput [::p/sin ::p/input] ::p/x) [::p/sin ::p/x]))
  (is (= (p/distrinput [::p/sin [::p/cos ::p/input]] ::p/x) [::p/sin [::p/cos ::p/x]])))
        
(deftest find-derives
  (is (= (p/find-derives [::p/add [::p/derive [::p/sin ::p/x]] [::p/derive [::p/cos ::p/x]]]) [[::p/sin ::p/x] [::p/cos ::p/x]])))

(deftest prime
  (is (= (p/prime [::p/sin ::p/x]) [::p/cos ::p/x]))
  (is (= (p/prime [::p/add [::p/sin ::p/x] [::p/sin ::p/x]]) [::p/add [::p/cos ::p/x] [::p/cos ::p/x]]))
  (is (= (p/prime [::p/power ::p/x 5]) [::p/mult 5 [::p/power ::p/x 4]]))
  (is (= (p/prime [::p/power 3 5]) 0)))

(deftest prime-dive
  (is (= (p/prime-dive [::p/exp ::p/x]) {:text [] :skills #{} :answer [::p/exp ::p/x]}))
  (is (= (p/prime-dive [::p/derive [::p/exp ::p/x] ::p/x]) {:text ["Use the identity to find"] :skills #{::p/exp} :answer [::p/exp ::p/x]}))
  (is (= (:skills (p/prime-dive [::p/derive [::p/exp 5] ::p/x])) #{::p/const}))
  (is (= (:answer (p/prime-dive [::p/derive 5 ::p/x]) 0)))
  (is (= (:answer (p/prime-dive [::p/derive ::p/x ::p/x])) 1))
  (is (= (:skills (p/prime-dive [::p/derive [::p/mult 5 ::p/x] ::p/x])) #{::p/scaler}))
  (is (= (:skills (p/prime-dive [::p/derive [::p/mult [::p/div 7 4] ::p/x] ::p/x])) #{::p/scaler}))
  (is (= (:skills (p/prime-dive [::p/derive [::p/add ::p/x ::p/x 5] ::p/x])) #{::p/add ::p/const})))

(deftest variance
  (is (= (p/variance 5) #{}))
  (is (= (p/variance ::p/x) #{::p/x}))
  (is (= (p/variance (p/convert :t)) #{::p/t}))
  (is (= (p/variance [::p/add 5 ::p/x]) #{::p/x})))

(deftest simplify
  (is (= (p/simplify 5) 5))
  (is (= (p/simplify [::p/add 5 0]) 5))
  (is (= (p/simplify [::p/add 0 0]) 0))
  (is (= (p/simplify [::p/mult 5 1]) 5))
  (is (= (p/simplify [::p/mult 1 1]) 1))
  (is (= (p/simplify [::p/mult 5 1 5]) [::p/mult 5 5]))
  (is (= (p/simplify [::p/div 5 1]) 5))
  (is (= (p/simplify [::p/power 5 1]) 5))
  (is (= (p/simplify [::p/power [::p/mult 5 ::p/x] 1]) [::p/mult 5 ::p/x]))
  (is (= (p/simplify [::p/power [::p/mult 1 ::p/x] 1]) ::p/x)))
