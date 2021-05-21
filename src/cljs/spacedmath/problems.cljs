(ns spacedmath.problems
    (:require 
          [goog.string :as gstring]
          [goog.string.format]
          [clojure.string :as string]))

(defn convert [target]
  (cond
    (vector? target) (vec (map convert target))
    (number? target) target
    :else (keyword "spacedmath.problems" (name target))))

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

(def skills (atom #{::add ::power ::chain}))

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

(defn distrinput [target input] (into [] (map (fn [item] (if (= item ::input) input (if (vector? item) (distrinput item input) item))) target)))

(defn math-fn [math] (if (vector? math) (first math) math))


(defmulti prime math-fn)
(defmethod prime ::ident [func] (distrinput ((first func) identities) ::x))
(defmethod prime ::add [func] [::add (prime (nth func 1)) (prime (nth func 2))])
(defmethod prime ::mult [func] [::add [::mult (nth func 1) (prime (nth func 2))] [::mult (prime (nth func 1)) (nth func 2)]])
(defmethod prime ::x [func] 1)
(defmethod prime :default [func] (println (str "Could not find derivative for " func)))



(defmulti prime-step math-fn)
(defmethod prime-step ::ident [func]
  {:text "Use the identity to find"
   :math (distrinput ((first func) identities) ::x)
   :skills #{(first func)}})
(defmethod prime-step ::add [func]
  {:text "Distribute over addition"
   :math [::add [::derive (nth func 1)] [::derive (nth func 2)]]
   :skills #{::add}})
(defmethod prime-step ::mult [func]
  {:text "Apply the Product Rule"
   :math [::add [::mult (nth func 1) [::derive (nth func 2)]] [::mult [::derive (nth func 1)] (nth func 2)]]
   :skills #{::product}})
   

(println (prime-step [::sin ::x]))

(defn find-derives [target]
  (if-not (vector? target)
    []
    (cond
      (= (first target) ::derive) [(nth target 1)]
      (< (count target) 3) (find-derives (last target))
      :else
      (reduce
        (fn [result item]
          (if
            (vector? item)
            (into result (if (= (first item) ::derive) [(nth item 1)] (find-derives item)))
            result))
        []
        (rest target)))))
        

(defn prime-full [func]
  (loop [items [func]
         skills #{}]
    (let [item (first items)
          step (prime-step item)
          items-new (into (rest items) (find-derives (:math step)))
          skills-new (into skills (:skills step))]
      (if
        (empty? items-new) {:skills skills-new}
        (recur items-new skills-new)))))


(def derivation
  {:satisfies #{}
   :facilitates skills
   :generate
   (fn [problem]
     (do (println problem)
      {:problem (str "Find \\(\\frac{dy}{dx}\\) for \\[y = " (latex problem) "\\]")
       :solution (str "\\[\\frac{dy}{dx} = " (latex (prime problem)) "\\]")
       :skills (:skills (prime-full problem))}))})

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
