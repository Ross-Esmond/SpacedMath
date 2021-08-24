(ns spacedmath.problems-test
  (:require
    [cljs.test :refer-macros [deftest is testing run-tests]]
    [spacedmath.problems :as p :refer [convert]]
    [spacedmath.list :refer [detailed-math-list]]
    [clojure.string :refer [includes?]]))

(deftest convert-numbers
  (is (= (convert [:mult -1 [:exp \x]]) [::p/mult -1 [::p/exp \x]])))

(deftest latex
  (is (= (p/latex (convert [:mult -1 [:sin \x]])) "-\\sin\\left(x\\right)")
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
  (is (= (p/latex (convert [:add [:subtract 5 3] 2])) "5 - 3 + 2"))
  (is (= (p/latex (convert :sin)) "\\sin\\left(Nothing\\right)"))
  (is (= (p/latex (convert :root)) "\\sqrt[Nothing]{Nothing}"))
  (is (= (p/latex (convert :exp)) "e^{Nothing}")))

(deftest prime-pattern
  (is (= (p/prime-pattern [::p/exp \x]) {:text [] :skills #{} :answer [::p/exp \x]}))
  (is (=
       (p/prime-pattern [::p/derive [::p/exp \x] \x])
       {:text ["Use the identity to find$$\\frac{d}{dx}\\left(e^{x}\\right) = e^{x}$$"] :skills #{::p/exp} :answer [::p/exp \x]}))
  (is (=
       (p/prime-pattern [::p/derive [::p/exp \t] \t])
       {:text ["Use the identity to find$$\\frac{d}{dt}\\left(e^{t}\\right) = e^{t}$$"] :skills #{::p/exp} :answer [::p/exp \t]}))
  (is (= (:skills (p/prime-pattern [::p/derive [::p/exp 5] \x])) #{::p/const}))
  (is (= (:skills (p/prime-pattern [::p/derive [::p/exp \x] \x])) #{::p/exp}))
  (is (= (:skills (p/prime-pattern [::p/derive [::p/sin \x] \x])) #{::p/sin}))
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
   [[::p/derive [::p/div 5 [::p/power \x 4]] \x] [::p/mult -20 [::p/power \x -5]]]
   [[::p/derive [::p/div 1 \x] \x] [::p/mult -1 [::p/power \x -2]]]
   [[::p/derive [::p/add [::p/mult [::p/div 7 4] [::p/power \x 2]] [::p/mult -3 \x] 12] \x]
    [::p/add [::p/mult [::p/div 7 2] \x] -3]]
   [[::p/derive [::p/div [::p/exp \y] [::p/sin \y]] \y]
    [::p/div [::p/add [::p/mult [::p/sin \y] [::p/exp \y]] [::p/mult -1 [::p/exp \y] [::p/cos \y]]] [::p/power [::p/sin \y] 2]]]])

(deftest prime-pattern-answers
  (doseq [[problem answer] answers]
    (is (= (:answer (p/prime-pattern problem)) answer))))

(deftest math-list-verification
  (doseq [item detailed-math-list]
    (let [{answer :answer skills :skills} (p/prime-pattern [::p/derive (p/convert (nth (first item) 2)) \x])]
      (is (= answer (p/convert (nth item 2))))
      (is (= skills (p/convert (nth item 1)))))))

(deftest variance
  (is (= (p/variance 5) #{}))
  (is (= (p/variance \x) #{\x}))
  (is (= (p/variance \t) #{\t}))
  (is (= (p/variance [::p/add 5 \x]) #{\x})))

(deftest rule-simplify
  (is (= (p/rule-simplify 5) 5))
  (is (= (p/rule-simplify [::p/add 5 0]) 5))
  (is (= (p/rule-simplify [::p/add 1 \x]) [::p/add \x 1]))
  (is (= (p/rule-simplify [::p/add 0 0 0]) 0))
  (is (= (p/rule-simplify [::p/mult 5 1]) 5))
  (is (= (p/rule-simplify [::p/mult \x 1 \y]) [::p/mult \x \y]))
  (is (= (p/rule-simplify [::p/mult \x 2 4 \y]) [::p/mult 8 \x \y]))
  (is (= (p/rule-simplify [::p/add \x 2 4 \y]) [::p/add \x \y 6]))
  (is (= (p/rule-simplify [::p/div 5 1]) 5))
  (is (= (p/rule-simplify [::p/power 5 1]) 5))
  (is (= (p/rule-simplify [::p/power 5 2]) 25))
  (is (= (p/rule-simplify [::p/power 2 40]) [::p/power 2 40]))
  (is (= (p/rule-simplify [::p/power [::p/mult 5 \x] 1]) [::p/mult 5 \x]))
  (is (= (p/rule-simplify [::p/power [::p/mult 1 \x] 1]) \x))
  (is (= (p/rule-simplify [::p/power [::p/power \x 5] -2]) [::p/power \x -10]))
  (is (= (p/rule-simplify [::p/power [::p/root \x 5] -2]) [::p/power \x [::p/div -2 5]]))
  (is (= (p/rule-simplify [::p/power [::p/root \x 1] -2]) [::p/power \x -2]))
  (is (= (p/rule-simplify [::p/div 4 6]) [::p/div 2 3])))
  ;(is (= (p/rule-simplify [::p/mult [::p/mult \x \y] \z]) [::p/mult \x \y \z])))
  ;(is (= (p/rule-simplify [::p/add \x [::p/add \y \z] \w]) [::p/add \x \y \z \w])))

(deftest math-unify
  (is (= (p/math-unify \t \x) [{\x \t}]))
  (is (= (p/math-unify ::p/sin ::p/sin) [{}]))
  (is (= (p/math-unify [::p/sin \u] \x) [{\x [::p/sin \u]}]))
  (is (= (p/math-unify [::p/sin \u] [::p/sin \x]) [{\x \u}]))
  (is (= (p/math-unify [::p/tan \u] [::p/sin \x]) []))
  (is (= (p/math-unify [::p/tan [::p/sin \u]] [::p/tan [::p/sin \x]]) [{\x \u}]))
  (is (= (p/math-unify [::p/mult [::p/sin \u] [::p/tan \v]] [::p/mult [::p/sin \x] [::p/tan \x]]) []))
  (is (= (p/math-unify [::p/derive [::p/sin \u] \u] [::p/derive [\f \x] \x]) [{[\f \x] [::p/sin \u] \x \u}]))
  (is (= (p/math-unify [::p/derive [::p/sin \v] \u] [::p/derive [::p/sin \x] \x]) []))
  (is (= (p/math-unify [::p/derive [::p/sin \u] \u] [::p/derive [::p/sin \x] \x]) [{\x \u}]))
  (is (= (p/math-unify [::p/derive [::p/mult \a \u] \u] [::p/derive [::p/mult \c \x] \x]) [{\c \a \x \u}]))
  (is (= (p/math-unify [::p/derive [::p/mult [::p/sin \d] \u] \u] [::p/derive [::p/mult \c \x] \x]) [{\c [::p/sin \d] \x \u}]))
  (is (= (p/math-unify [::p/derive [::p/mult [::p/sin \u] \u] \u] [::p/derive [::p/mult \c \x] \x]) []))
  (is (= (first (p/math-unify [::p/derive [::p/add \x 5] \x] [::p/derive [::p/add [\f \u] [\g \u]] \u]))
         {[\f \u] \x, [\g \u] 5, \u \x}))
  (is (= (first
           (p/math-unify
             [::p/derive [::p/mult [::p/sin \u] [::p/tan \u]] \u]
             [::p/derive [::p/mult [\g \x] [\f \x]] \x]))
         {[\g \x] [::p/sin \u] [\f \x] [::p/tan \u] \x \u}))
  (is (= (p/math-unify [::p/mult \u \v] [::p/mult \x \x]) []))
  (is (= (p/math-unify [::p/mult \u \u] [::p/mult \x \y]) []))
  (is (= (p/math-unify [::p/derive [::p/exp 5] \u] [::p/derive \c \x]) [{\c [::p/exp 5] \x \u}]))
  (is (= (p/math-unify
           [::p/derive [::p/div 5 [::p/power \x 4]] \x]
           [::p/derive [::p/div \c [\f \x]] \x])
         [{\c 5 [\f \x] [::p/power \x 4] \x \x}]))
  (is (= (p/math-unify [::p/derive [::p/mult \x \x] \x] [::p/derive [::p/mult \c [\f \x]] \x]) []))
  (is (= (p/math-unify [] []) [{}]))
  (is (= (p/math-unify [::p/div 1 1] [::p/div 1 0]) []))
  (is (= (p/math-unify [::p/div 1 0] [::p/div 1 0]) [{}]))
  (is (= (p/math-unify [::p/div 0 1] [::p/div 1 0]) []))
  (is (= (p/math-unify [::p/add 1 1] [::p/add 1]) []))
  (is (= (p/math-unify [::p/add 1 0] [::p/add 1 0]) [{}]))
  (is (= (p/math-unify [::p/add 0 1] [::p/add 1 0]) [{}]))
  (is (= (p/math-unify 1 \a) [{\a 1}]))
  (is (= (p/math-unify [::p/div 2 1] [::p/div \a 1]) [{\a 2}]))
  (is (= (p/math-unify [::p/div 2 2] [::p/div \a \a]) [{\a 2}]))
  (is (= (p/math-unify [::p/div 2 1] [::p/div \a \a]) []))
  (is (= (p/math-unify [::p/div ::p/pi ::p/pi] [::p/div \a \a]) [{\a ::p/pi}]))
  (is (= (p/math-unify [::p/div 2 3] [\f \u]) [{[\f \u] [::p/div 2 3]}]))
  (is (= (p/math-unify [::p/div \x \y] [::p/div [\f \u] [\g \u]]) [{[\f \u] \x [\g \u] \y}]))
  (is (= (p/math-unify [::p/derive [::p/div \x \x] \x] [::p/derive [::p/div \v \v] \v]) [{\v \x}]))
  (is (= (p/math-unify [::p/derive [::p/div \y \y] \x] [::p/derive [::p/div \v \v] \v]) []))
  (is (= (p/math-unify [::p/add 0 \x] [::p/add \u 0]) [{\u \x}])))

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

(deftest parser
  (is (= (p/parse-mafs "(50*50)") [::p/mult 50 50]))
  (is (= (p/parse-mafs "f(x)") [::p/fn \f \x]))
  (is (= (p/parse-mafs "f(x)") [::p/fn \f \x]))
  (is (= (p/parse-mafs "f(x)=((x^2)+(2*x))")
         [::p/equal [::p/fn \f \x] [::p/add [::p/power \x 2] [::p/mult 2 \x]]]))
  (is (= (p/parse-mafs "f(x)=((x^2)+(2*x*y))")
         [::p/equal [::p/fn \f \x] [::p/add [::p/power \x 2] [::p/mult 2 \x \y]]]))
  (is (= (p/parse-mafs "(x+2+x+y)")
         [::p/add \x 2 \x \y]))
  (is (= (p/parse-mafs "2^40") [::p/power 2 40]))
  (is (= (p/parse-mafs "1.4^40") [::p/power 1.4 40]))
  (is (= (p/parse-mafs "exp(40)") [::p/exp 40]))
  (is (= (p/parse-mafs "sin(x)") [::p/sin \x]))
  (is (= (p/parse-mafs "f(x)=tan(2*x)") [::p/equal [::p/fn \f \x] [::p/tan [::p/mult 2 \x]]]))
  (is (= (p/parse-mafs "pi") ::p/pi))
  (is (= (p/parse-mafs "pi*R") [::p/mult ::p/pi \R]))
  (is (= (p/parse-mafs "root(x,2)") [::p/root \x 2]))
  (is (= (p/parse-mafs "y=x^2") [::p/equal \y [::p/power \x 2]]))
  (is (= (p/parse-mafs "x*y*z") [::p/mult \x \y \z]))
  (is (= (p/parse-mafs "(x*y*z)") [::p/mult \x \y \z]))
  (is (= (p/parse-mafs "x+y+z") [::p/add \x \y \z]))
  (is (= (p/parse-mafs "(x+y+z)") [::p/add \x \y \z]))
  (is (= (p/parse-mafs "x*(y*z)") [::p/mult \x [::p/mult \y \z]]))
  (is (= (p/parse-mafs "x+(y+z)") [::p/add \x [::p/add \y \z]]))
  (is (= (p/parse-mafs "x+(exp(x))") [::p/add \x [::p/exp \x]])))

(deftest latex-score
  (is (= (p/latex-score "") 0))
  (is (= (p/latex-score \x) 1))
  (is (= (p/latex-score [::p/add \x \y]) 5))
  (is (= (p/latex-score [::p/add \x \y \z]) 9))
  (is (= (p/latex-score [::p/mult \x \y \z]) 3))
  (is (= (p/latex-score [::p/power \x \a]) 2))
  (is (= (p/latex-score [::p/root \x \a]) 3))
  (is (= (p/latex-score [::p/div \x \a]) 3))
  (is (= (p/latex-score [::p/exp \a]) 2))
  (is (= (p/latex-score [::p/parens [::p/add \a \b]]) 7))
  (is (= (p/latex-score [::p/sin \x]) 6))
  (is (= (p/latex-score 5) 1))
  (is (= (p/latex-score 50) 2))
  (is (= (p/latex-score 500) 3))
  (is (= (p/latex-score ::p/pi) 1)))

(deftest compute-numeric
  (is (= (p/compute-numeric [::p/mult 2 2]) 4))
  (is (= (p/compute-numeric [::p/add 2 2]) 4))
  (is (= (p/compute-numeric [::p/add [::p/div 1 2] 2]) [::p/div 5 2]))
  (is (= (p/compute-numeric [::p/mult [::p/div 2 3] 2]) [::p/div 4 3]))
  (is (= (p/compute-numeric [::p/power [::p/div 2 3] 2]) [::p/div 4 9]))
  (is (= (p/compute-numeric [::p/power 4 [::p/div 1 2]]) 2))
  (is (= (p/compute-numeric [::p/power [::p/div 4 9] [::p/div 1 2]]) [::p/div 2 3]))
  (is (= (p/compute-numeric [::p/div [::p/div 3 2] [::p/div 5 7]]) [::p/div 21 10])))

(deftest regressions
  (is (vector? (p/rule-simplify (p/parse-mafs "y=")))))
