(ns practice.core)
(require '[clojure.string :as str])

(defmulti latex (fn [thing] (cond (vector? thing) (first thing) (number? thing) ::number :else thing)))
(defmethod latex ::exec [func] (format "\\%s(%s)" (name (first func)) (latex (last func))))
(defmethod latex ::symbol [sym] (name sym))
(defmethod latex ::add [func] (str/join "+" (map latex (rest func))))
(defmethod latex ::exp [func] (format "e^{%s}" (latex (last func))))
(defmethod latex ::number [number] (str number))
(defmethod latex ::mult [func]
  (if (= -1 (nth func 1))
    (format "-%s" (latex (nth func 2)))
    (format "(%s)(%s)" (latex (nth func 1)) (latex (nth func 2)))))

(def skills #{ ::add ::sin ::cos ::tan ::sec ::csc ::cot ::exp})

(derive ::add ::commutative)
(derive ::trig ::unary)
(derive ::trig ::exec)
(derive ::trig ::ident)
(derive ::sin ::trig)
(derive ::cos ::trig)
(derive ::tan ::trig)
(derive ::sec ::trig)
(derive ::csc ::trig)
(derive ::cot ::trig)
(derive ::x ::symbol)
(derive ::exp ::unary)

(def complementary {::sin ::cos
                    ::tan ::cot
                    ::sec ::csc})

(def identities {::sin [::cos ::input]
                 ::cos [::mult -1 [::sin ::input]]
                 ::tan [::power [[::sec ::input] 2]]
                 ::cot [::mult -1 [::power [[::csc ::input] 2]]]
                 ::sec [::mult [[::sec ::input] [::tan ::input]]]
                 ::csc [::mult -1 [::mult [[::csc ::input] [::cot ::input]]]]})

(assert (= (latex [::mult -1 [::sin ::x]]) "-\\sin(x)") "negative latex printing failed")

(defn distrinput [target input] (into [] (map (fn [item] (if (= item ::input) input (if (vector? item) (distrinput item input) item))) target)))

(assert (= (distrinput [::sin ::input] ::x) [::sin ::x]))
(assert (= (distrinput [::sin [::cos ::input]] ::x) [::sin [::cos ::x]]))

(defmulti prime (fn [math] (if (vector? math) (first math) math)))
(defmethod prime ::ident [func] (distrinput ((first func) identities) ::x))
(defmethod prime ::add [func] [::add (prime (nth func 1)) (prime (nth func 2))])
(defmethod prime ::x [func] 1)

(assert (= (prime [::sin ::x]) [::cos ::x]))
(println (prime [::sin ::x]))
(assert (= (prime [::add [::sin ::x] [::sin ::x]]) [::add [::cos ::x] [::cos ::x]]))
(println (latex (prime [::add [::sin ::x] [::sin ::x]])))

(declare additionProblem)

(def addition
  {::satisfies #{::add}
   ::facilitates skills
   ::generate
   (fn [include]
     (let [problem (additionProblem include)]
      {::problem (format "Find \\(\\frac{dy}{dx}\\) for \\[y = %s\\]" (latex problem))
       ::solution (format "\\[\\frac{dy}{dx} = %s\\]" (latex (prime problem)))}))})

(defn additionProblem [include]
  (let
    [items (reduce (fn [stack item] (if (isa? item ::unary) (conj stack item) stack)) [] include)]
    [::add [(first items) ::x] (if (= (count items) 2) [(last items) ::x] ::x)]))
(println ((::generate addition) #{::sin ::cos}))

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

(assert (mathequals (additionProblem #{ ::exp ::sin }) [::add [::exp ::x] [::sin ::x]]) "addition with exp failed")
(assert (= (additionProblem #{ ::sin }) [::add [::sin ::x] ::x]) "addition with sin failed")
(assert (= (additionProblem #{ ::tan }) [::add [::tan ::x] ::x]) "addition with tan failed")
(assert (= (additionProblem #{ ::exp }) [::add [::exp ::x] ::x]) "addition with exp failed")
