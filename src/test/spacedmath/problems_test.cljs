(ns spacedmath.problems-test
  (:require
    [cljs.test :refer-macros [deftest is testing run-tests]]
    [spacedmath.problems :as p :refer [convert]]))

(deftest convert-numbers
  (is (= (convert [:mult -1 [:exp :x]]) [::p/mult -1 [::p/exp ::p/x]])))

(deftest latex
  (is (= (p/latex (convert [:mult -1 [:sin :x]])) "-\\sin(x)") "negative latex printing failed")
  (is (= (p/latex (convert [:mult 3 :x])) "3x") "printing with a scaler multiple failed")
  (is (= (p/latex (convert :pi)) "\\pi") "printing pi failed"))

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
  (is (= (p/prime-dive [::p/derive [::p/exp ::p/x]]) {:text ["Use the identity to find"] :skills #{::p/exp} :answer [::p/exp ::p/x]})))
