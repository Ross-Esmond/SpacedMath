(ns spacedmath.problems-test
  (:require
    [cljs.test :refer-macros [deftest is testing run-tests]]
    [spacedmath.problems :as p :refer [convert]]
    [clojure.string :refer [includes?]]))

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
  (is (= (p/latex (convert [:derive [:fn \f \x] \x])) "f'(x)"))
  (is (= (p/latex (convert [:power [:power \x 2] 2])) "\\left(x^{2}\\right)^{2}"))
  (is (= (p/latex (convert [:add 5 3 2])) "5 + 3 + 2"))
  (is (= (p/latex (convert [:add [:subtract 5 3] 2])) "5 - 3 + 2")))

(deftest distrinput
  (is (= (p/distrinput [::p/sin \x] \t) [::p/sin \t]))
  (is (= (p/distrinput [::p/sin [::p/cos \x]] \t) [::p/sin [::p/cos \t]])))

(deftest prime-pattern
  (is (= (p/prime-pattern [::p/exp \x]) {:text [] :skills #{} :answer [::p/exp \x]}))
  (is (=
       (p/prime-pattern [::p/derive [::p/exp \x] \x])
       {:text ["Use the identity to find$$\\frac{d}{dx}\\left(e^{x}\\right) = e^{x}$$"] :skills #{::p/exp} :answer [::p/exp \x]}))
  (is (=
       (p/prime-pattern [::p/derive [::p/exp \t] \t])
       {:text ["Use the identity to find$$\\frac{d}{dt}\\left(e^{t}\\right) = e^{t}$$"] :skills #{::p/exp} :answer [::p/exp \t]}))
  (is (= (:skills (p/prime-pattern [::p/derive [::p/exp 5] \x])) #{::p/const}))
  (is (= (:skills (p/prime-pattern [::p/derive [::p/mult 5 \x] \x])) #{::p/scaler}))
  (is (= (:skills (p/prime-pattern [::p/derive [::p/mult [::p/div 7 4] \x] \x])) #{::p/scaler}))
  (is (= (:skills (p/prime-pattern [::p/derive [::p/add \x \x 5] \x])) #{::p/add ::p/const})))

(def answers
  [[[::p/derive 5 \x] 0]
   [[::p/derive \x \x] 1]
   [[::p/derive [::p/add \x 5] \x] 1]
   [[::p/derive [::p/mult \c \x] \x] \c]
   [[::p/derive [::p/mult [::p/sin \x] [::p/exp \x]] \x]
    [::p/add [::p/mult [::p/cos \x] [::p/exp \x]] [::p/mult [::p/sin \x] [::p/exp \x]]]]
   [[::p/derive [::p/div 5 [::p/power \x 4]] \x] [::p/mult -5 4 [::p/power \x -5]]]
   [[::p/derive [::p/div 1 \x] \x] [::p/mult -1 [::p/power \x -2]]]
   [[::p/derive [::p/add [::p/mult [::p/div 7 4] [::p/power \x 2]] [::p/mult -3 \x] 12] \x]
    [::p/add [::p/mult [::p/div 7 4] 2 \x] -3]]
   [[::p/derive [::p/div [::p/exp \y] [::p/sin \y]] \y]
    [::p/div [::p/add [::p/mult [::p/sin \y] [::p/exp \y]] [::p/mult -1 [::p/exp \y] [::p/cos \y]]] [::p/power [::p/sin \y] 2]]]])

(deftest prime-pattern-answers
  (doseq [[problem answer] answers]
    (is (= (:answer (p/prime-pattern problem)) answer))))

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
  (is (= (p/simplify [::p/power [::p/power \x 5] -2]) [::p/power \x -10]))
  (is (= (p/simplify [::p/power [::p/root \x 5] -2]) [::p/power \x [::p/div -2 5]]))
  (is (= (p/simplify [::p/mult [::p/mult 5 5] 5]) [::p/mult 5 5 5]))
  (is (= (p/simplify [::p/add \x [::p/add \y \z] \w]) [::p/add \x \y \z \w]))
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
  (is (= (p/math-match [::p/derive [::p/sin \u] \u] [::p/derive [\f \x] \x]) {[\f \x] [::p/sin \u] \x \u}))
  (is (= (p/math-match [::p/derive [::p/sin \v] \u] [::p/derive [::p/sin \x] \x]) nil))
  (is (= (p/math-match [::p/derive [::p/sin \u] \u] [::p/derive [::p/sin \x] \x]) {\x \u}))
  (is (= (p/math-match [::p/derive [::p/mult \a \u] \u] [::p/derive [::p/mult \c \x] \x]) {\c \a \x \u}))
  (is (= (p/math-match [::p/derive [::p/mult [::p/sin \d] \u] \u] [::p/derive [::p/mult \c \x] \x]) {\c [::p/sin \d] \x \u}))
  (is (= (p/math-match [::p/derive [::p/mult [::p/sin \u] \u] \u] [::p/derive [::p/mult \c \x] \x]) nil))
  (is (= (p/math-match [::p/derive [::p/add \x 5] \x] [::p/derive [::p/add [\f \u] [\g \u]] \u])
         {[\f \u] \x, [\g \u] 5, \u \x}))
  (is (= (p/math-match
           [::p/derive [::p/mult [::p/sin \u] [::p/tan \u]] \u]
           [::p/derive [::p/mult [\g \x] [\f \x]] \x])
         {[\g \x] [::p/sin \u] [\f \x] [::p/tan \u] \x \u}))
  (is (= (p/math-match [::p/mult \u \v] [::p/mult \x \x]) nil))
  (is (= (p/math-match [::p/mult \u \u] [::p/mult \x \y]) nil))
  (is (= (p/math-match [::p/derive [::p/exp 5] \u] [::p/derive \c \x]) {\c [::p/exp 5] \x \u}))
  (is (= (p/math-match
           [::p/derive [::p/div 5 [::p/power \x 4]] \x]
           [::p/derive [::p/div \c [\f \x]] \x])
         {\c 5 [\f \x] [::p/power \x 4] \x \x}))
  (is (= (p/math-match [::p/derive [::p/mult \x \x] \x] [::p/derive [::p/mult \c [\f \x]] \x]) nil)))

(deftest pretty-print
  (is (= (p/pretty-print [::p/add \x -3]) [::p/subtract \x 3]))
  (is (= (p/pretty-print [::equal \y [::p/add \x -3]]) [::equal \y [::p/subtract \x 3]]))
  (is (= (p/pretty-print [::p/add \x [::p/mult -3 \x]]) [::p/subtract \x [::p/mult 3 \x]]))
  (is (= (p/pretty-print [::p/add [::p/mult -1 \x] [::p/mult -3 \x]]) [::p/subtract [::p/mult -1 \x] [::p/mult 3 \x]])))



(deftest prime-pattern-auto
  (doseq [rule @p/rules]
    (if (not (includes? (str (:result rule)) "derive"))
      (is (= (p/prime-pattern (:match rule)) {:text [((:text rule) (:match rule) (:result rule) {\c \c})]
                                              :skills (:skills rule)
                                              :answer (:result rule)})))))
