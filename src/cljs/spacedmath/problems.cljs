(ns spacedmath.problems
    (:require 
          [goog.string :as gstring]
          [goog.string.format]
          [clojure.string :as string]))

(defmulti latex (fn [thing] (cond (vector? thing) (first thing) (number? thing) ::number :else thing)))
(defmethod latex ::exec [func] (str "\\" (name (first func)) "(" (latex (last func)) ")"))
(defmethod latex ::symbol [sym] (name sym))
(defmethod latex ::add [func] (string/join "+" (map latex (rest func))))
(defmethod latex ::exp [func] (str "e^{" (latex (last func)) "}"))
(defmethod latex ::number [number] (str number))
(defmethod latex ::mult [func]
  (if (= -1 (nth func 1))
    (str "-" (latex (nth func 2)))
    (str "(" (latex (nth func 1)) ")(" (latex (nth func 2)) ")")))
(defmethod latex :default [_] "Nothing")

(def skills (atom #{ ::add ::power ::chain}))

(derive ::add ::commutative)
(derive ::trig ::unary)
(derive ::trig ::exec)
(derive ::sin ::trig)
(derive ::cos ::trig)
(derive ::tan ::trig)
(derive ::sec ::trig)
(derive ::csc ::trig)
(derive ::cot ::trig)
(derive ::x ::symbol)
(derive ::exp ::unary)
(derive ::power ::unary)
(derive ::power ::numbered)

(def complementary {::sin ::cos
                    ::tan ::cot
                    ::sec ::csc})

(def identities {::sin [::cos ::input]
                 ::cos [::mult -1 [::sin ::input]]
                 ::tan [::power [[::sec ::input] 2]]
                 ::cot [::mult -1 [::power [[::csc ::input] 2]]]
                 ::sec [::mult [[::sec ::input] [::tan ::input]]]
                 ::csc [::mult -1 [::mult [[::csc ::input] [::cot ::input]]]]
                 ::exp [::exp ::input]})

(swap! skills (fn [sk] (into sk (keys identities))))
(println skills)

(doseq [n (keys identities)] (derive n ::ident))

(assert (= (latex [::mult -1 [::sin ::x]]) "-\\sin(x)") "negative latex printing failed")

(defn distrinput [target input] (into [] (map (fn [item] (if (= item ::input) input (if (vector? item) (distrinput item input) item))) target)))

(assert (= (distrinput [::sin ::input] ::x) [::sin ::x]))
(assert (= (distrinput [::sin [::cos ::input]] ::x) [::sin [::cos ::x]]))

(defn math-fn [math] (if (vector? math) (first math) math))

(defmulti prime math-fn)
(defmethod prime ::ident [func] (distrinput ((first func) identities) ::x))
(defmethod prime ::add [func] [::add (prime (nth func 1)) (prime (nth func 2))])
(defmethod prime ::x [func] 1)
(defmethod prime :default [func] (println (str "Could not find derivative for " func)))

(assert (= (prime [::sin ::x]) [::cos ::x]))
(println (prime [::sin ::x]))
(assert (= (prime [::add [::sin ::x] [::sin ::x]]) [::add [::cos ::x] [::cos ::x]]))
(println (latex (prime [::add [::sin ::x] [::sin ::x]])))

(def derivation
  {:satisfies #{}
   :facilitates skills
   :generate
   (fn [problem]
     (do (println problem)
      {:problem (str "Find \\(\\frac{dy}{dx}\\) for \\[y = " (latex problem) "\\]")
       :solution (str "\\[\\frac{dy}{dx} = " (latex (prime problem)) "\\]")}))})

(declare mathequals)

(defn mathexact [a b]
  (if (and (vector? a) (vector? b))
    (and (= (count a) (count b)) (every? true? (map mathequals a b)))
    (= a b)))

(defn mathequals [a b]
  (if (and (vector? a) (vector? b))
    (if (isa? (first a) ::commutative)
      (or (mathexact a b) (mathexact [(first a) (nth a 2) (nth a 1)] b))
      (mathexact a b))
    (mathexact a b)))

