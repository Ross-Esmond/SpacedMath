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

(defn variance
  [func variable]
  (if
    (= func variable) :identity
    (if (and (vector? func) (some #(not (= (variance % variable) :none)) (rest func)))
      :function
      :none)))

(defn numeric? [target] (number? target))

(defn math-fn [math]
  (cond
    (vector? math) (first math)
    (numeric? math) ::numeric
    :else math))

(defn parens [& strings] (str "\\left(" (string/join strings) "\\right)"))

(defmulti latex math-fn)
(defmethod latex ::exec [func] (str "\\" (name (first func)) (parens (latex (last func)))))
(defmethod latex ::symbol [sym] (name sym))
(defmethod latex ::named [sym] (str "\\" (name sym)))
(defmethod latex ::add [func] (string/join "+" (map latex (rest func))))
(defmethod latex ::exp [func] (str "e^{" (latex (last func)) "}"))
(defmethod latex ::numeric [number] (str number))
(defmethod latex ::mult [func]
  (cond
    (= -1 (nth func 1)) (str "-" (latex (nth func 2)))
    (number? (nth func 1)) (str (latex (nth func 1)) (if
                                                       (number? (nth func 2))
                                                       (parens (latex (nth func 2)))
                                                       (latex (nth func 2))))
    :else (str (latex (nth func 1)) (latex (nth func 2)))))
(defmethod latex ::power [func] (str (latex (nth func 1)) "^{" (latex (nth func 2)) "}"))
(defmethod latex ::derive [func] (str "\\frac{d}{dx}" (parens (latex (nth func 1)))))
(defmethod latex ::equal [func] (str (latex (nth func 1)) " = " (latex (nth func 2))))
(defmethod latex ::div [func]
  (str "\\frac{" (latex (nth func 1)) "}{" (latex (nth func 2)) "}"))
(defmethod latex :default [_] "Nothing")


(def skills (atom #{::add ::power ::chain ::const}))

(derive ::add ::commutative)
(derive ::mult ::commutative)
(derive ::trig ::unary)
(derive ::trig ::exec)
(derive ::sin ::trig)
(derive ::cos ::trig)
(derive ::tan ::trig)
(derive ::sec ::trig)
(derive ::csc ::trig)
(derive ::cot ::trig)
(derive ::x ::symbol)
(derive ::y ::symbol)
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

(def non-operand
  {::add [:any 0]
   ::mult [:any 1]
   ::div [2, 1]
   ::power [2, 1]})

(defn simplify [func]
  (cond
    (vector? func)
    (let [[operator & operands] func
           root (into [operator] (map simplify operands))]
      (if (not (contains? non-operand operator)) root
        (let [[target value] (operator non-operand)]
          (if (= target :any)
            (let [[operator & operands] root
                  filtered (into [operator] (filter #(not (= % value)) operands))]
              (cond
                (= (count filtered) 1) value
                (= (count filtered) 2) (last filtered)
                :else filtered))
            (if (= (nth root target) value)
              (nth root 1)
              root)))))
    :else func))

(def greek #{::pi})

(swap! skills (fn [sk] (into sk (keys identities))))

(doseq [n (keys identities)] (derive n ::ident))

(doseq [n greek] (derive n ::named))

(defn distrinput [target input]
  (into [] (map
             (fn [item]
               (if (= item ::input) input (if (vector? item) (distrinput item input) item)))
             target)))



(defmulti prime math-fn)
(defmethod prime ::ident [func] (distrinput ((first func) identities) ::x))
(defmethod prime ::add [func] [::add (prime (nth func 1)) (prime (nth func 2))])
(defmethod prime ::mult [func]
  [::add [::mult (nth func 1) (prime (nth func 2))] [::mult (prime (nth func 1)) (nth func 2)]])
(defmethod prime ::x [func] 1)
(defmethod prime ::power [func] (cond
                                  (isa? (nth func 1) ::symbol)
                                  [::mult (nth func 2) [::power ::x (- (nth func 2) 1)]]
                                  (numeric? (nth func 1))
                                  0))
(defmethod prime :default [func] (println (str "Could not find derivative for " func)))


(defn math-fn-type [math]
  (cond
    (= (variance math ::x) :none) ::numeric
    (vector? math) (first math)
    :else math))

(defmulti prime-step math-fn-type)
(defmethod prime-step ::numeric [func]
  {:text (str "The expression $" (latex func) "$ is just a number, so its derivative is 0.")
   :math 0
   :skills #{::const}})
(defmethod prime-step ::symbol [func]
  {:text (str "The derivative of $" (latex func) "$ is just 1.\n")
   :math 1
   :skills #{}})
(defmethod prime-step ::ident [func]
  {:text "Use the identity to find"
   :math (distrinput ((first func) identities) ::x)
   :skills #{(first func)}})
(defmethod prime-step ::add [func]
  (let [math (into [::add] (map (fn [_] [::derive _]) (rest func)))]
    {:text (str
             "Distribute over addition.\n$$" (latex [::equal [::derive func] math]) "$$")
     :math math
     :skills #{::add}}))
(defmethod prime-step ::mult [func]
  (cond
    (= (variance (nth func 1) ::x) :none)
    (let [math [::mult (nth func 1) [::derive (nth func 2)]]]
      {:text (str
               "Since $" (latex (nth func 1)) "$ is just a number, then"
               "$$" (latex [::equal [::derive func] math]) "$$")
       :math math
       :skills #{::scaler}})
    :else
    (let [math [::add
                [::mult (nth func 1) [::derive (nth func 2)]]
                [::mult [::derive (nth func 1)] (nth func 2)]]]
      {:text (str "Apply the Product Rule.$$" (latex [::equal [::derive func] math]) "$$")
       :math math
       :skills #{::product}})))
(defmethod prime-step ::power [func]
  (let [math [::mult (nth func 2) [::power (nth func 1) (- (nth func 2) 1)]]]
    {:text (str "Apply the Power Rule.\n$$" (latex [::equal [::derive func] math]) "$$")
     :math math
     :skills #{::power}}))
          
   

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
        

(defn prime-dive [func]
  (if (not (vector? func))
    {:text []
     :skills #{}
     :answer func}
    (if (= ::derive (nth func 0))
      (let [step (prime-step (nth func 1))
            dive (prime-dive (:math step))]
        {:text (concat [(:text step)] (:text dive))
         :skills (into (:skills step) (:skills dive))
         :answer (simplify (:answer dive))})  
      (reduce (fn [stack op]
                (let [result (prime-dive op)]
                  {:text (concat (:text stack) (:text result))
                   :skills (into (:skills stack) (:skills result))
                   :answer (conj (:answer stack) (:answer result))}))
              {:text [] :skills #{} :answer [(first func)]}
              (rest func)))))

          

(defn basic-derivation [func]
  (if (= (nth func 0) ::equal)
    (let [derived (prime-dive [::derive (nth func 2)])]
      {:problem (str "Differentiate the function\n$$" (latex func) "$$")
       :steps (:text derived)
       :skills (:skills derived)
       :answer (str "$$" (latex [::equal [::derive (nth func 1)] (:answer derived)]) "$$")})
    nil))


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

