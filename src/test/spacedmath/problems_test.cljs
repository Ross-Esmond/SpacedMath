(ns spacedmath.problems-test
  (:require
    [cljs.test :refer-macros [deftest is testing run-tests]]
    [spacedmath.problems :as p :refer [convert]]))

(deftest convert-numbers
  (is (= (convert [:mult -1 [:exp :x]]) [::p/mult -1 [::p/exp ::p/x]])))

(deftest latex
  (is (= (p/latex (convert [:mult -1 [:sin :x]])) "-\\sin(x)") "negative latex printing failed"))

(deftest distrinput
  (is (= (p/distrinput [::p/sin ::p/input] ::p/x) [::p/sin ::p/x]))
  (is (= (p/distrinput [::p/sin [::p/cos ::p/input]] ::p/x) [::p/sin [::p/cos ::p/x]])))
        
(deftest find-derives
  (is (= (p/find-derives [::p/add [::p/derive [::p/sin ::p/x]] [::p/derive [::p/cos ::p/x]]]) [[::p/sin ::p/x] [::p/cos ::p/x]])))

(deftest prime-full
  (is (= (p/prime-full [::p/sin ::p/x]) {:skills #{::p/sin}}))
  (is (= (p/prime-full [::p/add [::p/sin ::p/x] [::p/exp ::p/x]]) {:skills #{::p/sin ::p/exp ::p/add}})))

(deftest prime
  (is (= (p/prime [::p/sin ::p/x]) [::p/cos ::p/x]))
  (is (= (p/prime [::p/add [::p/sin ::p/x] [::p/sin ::p/x]]) [::p/add [::p/cos ::p/x] [::p/cos ::p/x]])))

