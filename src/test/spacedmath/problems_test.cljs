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

(deftest parser
  (is (= (p/parse-mafs "(50*50)") [::p/mult 50 50]))
  (is (= (p/parse-mafs "f(x)") [::p/fn \f \x]))
  (is (= (p/parse-mafs "f(x)") [::p/fn \f \x]))
  (is (= (p/parse-mafs "f(x)=((x^2)+(2*x))")
         [::p/equal [::p/fn \f \x] [::p/add [::p/power \x 2] [::p/mult 2 \x]]]))
  (is (= (p/parse-mafs "f(x)=((x^2)+(2*x*y))")
         [::p/equal [::p/fn \f \x] [::p/add [::p/power \x 2] [::p/mult 2 \x \y]]]))
  (is (= (p/parse-mafs "(x+2+x+y)")
         [::p/add \x \x \y 2]))
  (is (= (p/parse-mafs "2^40") [::p/power 2 40]))
  (is (= (p/parse-mafs "1.4^40") [::p/power 1.4 40]))
  (is (= (p/parse-mafs "exp(40)") [::p/exp 40]))
  (is (= (p/parse-mafs "sin(x)") [::p/sin \x]))
  (is (= (p/parse-mafs "f(x)=tan(2*x)") [::p/equal [::p/fn \f \x] [::p/tan [::p/mult 2 \x]]]))
  (is (= (p/parse-mafs "pi") ::p/pi))
  (is (= (p/parse-mafs "pi*R") [::p/mult ::p/pi \R]))
  (is (= (p/parse-mafs "root(x,2)") [::p/root \x 2]))
  (is (= (p/parse-mafs "y=x^2") [::p/equal \y [::p/power \x 2]])))

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
  (is (= (p/latex-score [::p/sin \x]) 6)))
