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
(defmethod latex ::named [sym] (str "\\" (name sym)))
(defmethod latex ::add [func] (string/join "+" (map latex (rest func))))
(defmethod latex ::exp [func] (str "e^{" (latex (last func)) "}"))
(defmethod latex ::number [number] (str number))
(defmethod latex ::mult [func]
  (cond
    (= -1 (nth func 1)) (str "-" (latex (nth func 2)))
    (number? (nth func 1)) (str (latex (nth func 1)) (latex (nth func 2)))
    :else (str "(" (latex (nth func 1)) ")(" (latex (nth func 2)) ")")))
(defmethod latex ::power [func] (str (latex (nth func 1)) "^{" (latex (nth func 2)) "}"))
(defmethod latex ::derive [func] (str "\\frac{d}{dx}" (latex (nth func 1))))
(defmethod latex ::equal [func] (str (latex (nth func 1)) " = " (latex (nth func 2))))
(defmethod latex :default [_] "Nothing")

(def skills (atom #{::add ::power ::chain ::const}))

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

(def greek #{::pi})

(swap! skills (fn [sk] (into sk (keys identities))))

(doseq [n (keys identities)] (derive n ::ident))

(doseq [n greek] (derive n ::named))

(defn distrinput [target input] (into [] (map (fn [item] (if (= item ::input) input (if (vector? item) (distrinput item input) item))) target)))

(defn math-fn [math] (if (vector? math) (first math) math))


(defn numeric? [target] (number? target))


(defmulti prime math-fn)
(defmethod prime ::ident [func] (distrinput ((first func) identities) ::x))
(defmethod prime ::add [func] [::add (prime (nth func 1)) (prime (nth func 2))])
(defmethod prime ::mult [func] [::add [::mult (nth func 1) (prime (nth func 2))] [::mult (prime (nth func 1)) (nth func 2)]])
(defmethod prime ::x [func] 1)
(defmethod prime ::power [func] (cond
                                  (isa? (nth func 1) ::symbol) [::mult (nth func 2) [::power ::x (- (nth func 2) 1)]]
                                  (numeric? (nth func 1)) 0))
(defmethod prime :default [func] (println (str "Could not find derivative for " func)))


(defmulti prime-step math-fn)
(defmethod prime-step ::ident [func]
  (cond
    (isa? (nth func 1) ::symbol)
    {:text "Use the identity to find"
     :math (distrinput ((first func) identities) ::x)
     :skills #{(first func)}}
    (numeric? (nth func 1))
    {:text (str "The expression $" (latex func) "$ is just a number, so its derivative is 0.")
     :math 0
     :skills #{::const}}))
(defmethod prime-step ::add [func]
  (let [math [::add [::derive (nth func 1)] [::derive (nth func 2)]]]
    {:text (str
             "Distribute over addition.\n$$\\frac{d}{dx}(" (latex func) ") = " (latex math) "$$")
     :math math
     :skills #{::add}}))
(defmethod prime-step ::mult [func]
  (let [math [::add [::mult (nth func 1) [::derive (nth func 2)]] [::mult [::derive (nth func 1)] (nth func 2)]]]
    {:text (str "Apply the Product Rule.\n$$\\frac{d}{dx}" (latex func) " = " (latex math))
     :math math
     :skills #{::product}}))
(defmethod prime-step ::power [func]
  (cond
    (isa? (nth func 1) ::symbol)
    (let [math [::mult (nth func 2) [::power (nth func 1) (- (nth func 2) 1)]]]
      {:text (str "Apply the Power Rule.\n$$\\frac{d}{dx}" (latex func) " = " (latex math) "$$")
       :math math
       :skills #{::power}})
    (numeric? (nth func 1))
    {:text (str "The expression $" (latex func) "$ is just a number, so its derivative is 0.")
     :math 0
     :skills #{::const}}))
          
   

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
         :answer (:answer dive)})  
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

