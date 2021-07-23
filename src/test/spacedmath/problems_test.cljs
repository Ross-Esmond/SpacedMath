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
  (is (= (p/latex (convert :pi)) "\\pi ") "printing pi failed")
  (is (= (p/latex (convert [:mult 5 2])) "5\\left(2\\right)") "printing two scaler multiples failed")
  (is (= (p/latex 5) "5"))
  (is (= (p/latex (convert [:div 5 4])) "\\frac{5}{4}"))
  (is (= (p/latex (convert \y)) "y"))
  (is (= (p/latex (convert [:mult 5 2 3])) "5\\left(2\\right)\\left(3\\right)"))
  (is (= (p/latex (convert [:fn \f \x])) "f(x)"))
  (is (= (p/latex (convert [:derive [:fn \f \x] \x])) "f'(x)")))

(deftest distrinput
  (is (= (p/distrinput [::p/sin \x] \t) [::p/sin \t]))
  (is (= (p/distrinput [::p/sin [::p/cos \x]] \t) [::p/sin [::p/cos \t]])))
        
(deftest prime-dive
  (is (= (p/prime-dive [::p/exp \x]) {:text [] :skills #{} :answer [::p/exp \x]}))
  (is (=
       (p/prime-dive [::p/derive [::p/exp \x] \x])
       {:text ["Use the identity to find$$\\frac{d}{dx}\\left(e^{x}\\right) = e^{x}$$"] :skills #{::p/exp} :answer [::p/exp \x]}))
  (is (=
       (p/prime-dive [::p/derive [::p/exp \t] \t])
       {:text ["Use the identity to find$$\\frac{d}{dt}\\left(e^{t}\\right) = e^{t}$$"] :skills #{::p/exp} :answer [::p/exp \t]}))
  (is (= (:skills (p/prime-dive [::p/derive [::p/exp 5] \x])) #{::p/const}))
  (is (= (:answer (p/prime-dive [::p/derive 5 \x]) 0)))
  (is (= (:answer (p/prime-dive [::p/derive \x \x])) 1))
  (is (= (:skills (p/prime-dive [::p/derive [::p/mult 5 \x] \x])) #{::p/scaler}))
  (is (= (:skills (p/prime-dive [::p/derive [::p/mult [::p/div 7 4] \x] \x])) #{::p/scaler}))
  (is (= (:skills (p/prime-dive [::p/derive [::p/add \x \x 5] \x])) #{::p/add ::p/const}))
  (is (= (:answer (p/prime-dive [::p/derive [::p/mult \c \x] \x])) \c))
  (is (= (:answer (p/prime-dive [::p/derive [::p/div 5 [::p/power \x 4] \x] \x])) [::p/mult -5 4 [::p/power \x -5]]))
  (is (= (:answer (p/prime-dive [::p/derive [::p/exp [::p/add \x 1]] \x]) [::p/exp [::p/add \x 1]]))))

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

(deftest math-match
  (is (= (p/math-match \t \x) {\x \t}))
  (is (= (p/math-match ::p/sin ::p/sin) {}))
  (is (= (p/math-match [::p/sin \u] \x) {\x [::p/sin \u]}))
  (is (= (p/math-match [::p/sin \u] [::p/sin \x]) {\x \u}))
  (is (= (p/math-match [::p/tan \u] [::p/sin \x]) nil))
  (is (= (p/math-match [::p/tan [::p/sin \u]] [::p/tan [::p/sin \x]]) {\x \u}))
  (is (= (p/math-match [::p/mult [::p/sin \u] [::p/tan \v]] [::p/mult [::p/sin \x] [::p/tan \x]]) nil))
  (is (= (p/math-match [::p/sin \u] [\f \x]) {\f ::p/sin \x \u}))
  (is (= (p/math-match [::p/derive [::p/sin \v] \u] [::p/derive [::p/sin \x] \x]) nil))
  (is (= (p/math-match [::p/derive [::p/sin \u] \u] [::p/derive [::p/sin \x] \x]) {\x \u}))
  (is (= (p/math-match [::p/derive [::p/mult \a \u] \u] [::p/derive [::p/mult \c \x] \x]) {\c \a \x \u}))
  (is (= (p/math-match [::p/derive [::p/mult [::p/sin \d] \u] \u] [::p/derive [::p/mult \c \x] \x]) {\c [::p/sin \d] \x \u}))
  (is (= (p/math-match [::p/derive [::p/mult [::p/sin \u] \u] \u] [::p/derive [::p/mult \c \x] \x]) nil))
  (is (= (p/math-match [::p/sin [::p/tan \u]] [\f [\f \x]]) nil))
  (is (= (p/math-match [::p/mult [::p/sin \u] [::p/tan \u]] [::p/mult [\g \x] [\f \x]]) {\g ::p/sin \f ::p/tan \x \u}))
  (is (= (p/math-match [::p/mult \u \v] [::p/mult \x \x]) nil))
  (is (= (p/math-match [::p/mult \u \u] [::p/mult \x \y]) nil)))

(deftest prime-pattern
  (doseq [rule @p/rules]
    (is (= (p/prime-pattern (:match rule)) {:text (:text rule) :skills (:skills rule) :answer (:result rule)}))))
