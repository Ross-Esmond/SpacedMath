(ns spacedmath.problems
  (:require 
    [goog.string :as gstring]
    [goog.string.format]
    [clojure.string :as string])
  (:require-macros [utils :as ut]))

(defn convert [target]
  (cond
    (vector? target) (vec (map convert target))
    (number? target) target
    (char? target) target
    :else (keyword "spacedmath.problems" (name target))))

(defn variance
  [func]
  (cond
    (char? func) #{func}
    (vector? func) (reduce #(into %1 (variance %2)) #{} (rest func))
    :else #{}))

(defn numeric? [target] (number? target))

(defn math-fn [math]
  (cond
    (vector? math) (first math)
    (numeric? math) ::numeric
    (char? math) ::symbol
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
  (string/join
    (first
      (reduce
        (fn [[result prev] item]
          [(into
             result
             (cond
               (= -1 item) ["-"]
               (and (number? item) (number? prev)) [(parens (latex item))]
               :else [(latex item)]))
           item])
        [[] nil]
        (rest func)))))
(defmethod latex ::power [func] (str (latex (nth func 1)) "^{" (latex (nth func 2)) "}"))
(defmethod latex ::derive [func]
  (let [[_ eq target] func]
    (if (and (vector? eq) (= (first eq) ::fn))
      (let [[_ ident variable] eq]
        (str ident "'" (parens variable)))
      (str "\\frac{d}{d" (nth func 2) "}" (parens (latex (nth func 1)))))))
(defmethod latex ::equal [func] (str (latex (nth func 1)) " = " (latex (nth func 2))))
(defmethod latex ::div [func]
  (str "\\frac{" (latex (nth func 1)) "}{" (latex (nth func 2)) "}"))
(defmethod latex ::fn [func] (let [[_ ident variable] func] (str ident (parens variable))))
(defmethod latex :default [_] "Nothing")


(def skills (atom #{::add ::power ::chain ::const}))

(derive ::add ::commutative)
(derive ::mult ::commutative)
(derive ::add ::associative)
(derive ::mult ::associative)
(derive ::trig ::unary)
(derive ::trig ::exec)
(derive ::sin ::trig)
(derive ::cos ::trig)
(derive ::tan ::trig)
(derive ::sec ::trig)
(derive ::csc ::trig)
(derive ::cot ::trig)
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

(defn remove-inconsequential-operators [root]
  (if (not (contains? non-operand (first root))) root
    (let [[target value] ((first root) non-operand)]
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


(defn combine-associative-operands [root]
  (if (or (not (vector? root)) (not (isa? (first root) ::associative))) root
    (do
      (defn flat-out [[operator & operands]]
        (reduce
          (fn [stack op]
            (into stack (if (and (vector? op) (= (first op) operator)) (rest op) [op])))
          [operator]
          (map
            (fn [op] (if (and (vector? op) (= (first op) operator)) (flat-out op) op))
            operands)))
      (flat-out root))))

(defn abs [n] (max n (- n)))

(defn consolidate-negation [root]
  (if (not (= (first root) ::mult)) root
    (let [[normalized n]
          (reduce
            (fn [[stack n] item]
              (cond
                (number? item) [(conj stack (abs item)) (if (< item 0) (+ n 1) n)] 
                :else [(conj stack item) n]))
            [[::mult] 0]
            (rest root))]
      (if (even? n) normalized
        (if (number? (nth normalized 1))
          (into [::mult] (concat [(- (nth normalized 1))] (subvec normalized 2)))
          (into [::mult -1] (rest normalized)))))))
  

(defn simplify [func]
  (cond
    (vector? func)
    (let [[operator & operands] func
           root (into [operator] (map simplify operands))]
      (-> root
        combine-associative-operands
        consolidate-negation
        remove-inconsequential-operators))
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

(defn gcd [a b]
  (if (zero? b) a
    (recur b (mod a b))))

(defn reduce-div [div]
  (if (not (= (first div) ::div)) div
    (let [[_ div-num div-den] div
          divisor (gcd div-num div-den)
          frac [::div (/ div-num divisor) (/ div-den divisor)]]
      (if (= (nth frac 2) 1) (nth frac 1) frac))))

(defn get-frac [target]
  (cond
    (number? target) [::div target 1]
    (= (first target) ::div) target
    :else target))

(defn crunch-numbers [target]
  (let [frac-target (get-frac target)]
    (into [::div]
      (reduce
        (fn [[top bot] current]
          (cond
            (number? current) [(* top current) bot]
            (= (first current) ::div)
            (let [[_ c-top c-bot] current]
              [(* top c-top) (* bot c-bot)])
            :else (throw (str current " not implemented yet"))))
        [1 1]
        (if (= (first frac-target) ::mult) (rest frac-target) [frac-target])))))

(defn insta-add [a b]
  (let [[_ a-num a-den] (get-frac (crunch-numbers a))
        [_ b-num b-den] (get-frac (crunch-numbers b))]
    (reduce-div [::div (+ (* a-num b-den) (* b-num a-den)) (* a-den b-den)])))


(defn math-fn-type [[_ math variable]]
  (cond
    (not (contains? (variance math) variable)) ::numeric
    (vector? math) (first math)
    (char? math) ::symbol
    :else math))

(defmulti prime-step math-fn-type)
(defmethod prime-step ::numeric [[_ func variable]]
  {:text (str "The expression $" (latex func) "$ is just a number, so its derivative is 0.")
   :math 0
   :skills #{::const}})
(defmethod prime-step ::symbol [[_ func variable]]
  {:text (str "The derivative of $" (latex func) "$ is just 1.\n")
   :math 1
   :skills #{}})
(defmethod prime-step ::ident [[_ func variable]]
  {:text "Use the identity to find"
   :math (distrinput ((first func) identities) variable)
   :skills #{(first func)}})
(defmethod prime-step ::add [[_ func variable]]
  (let [math (into [::add] (map (fn [_] [::derive _ variable]) (rest func)))]
    {:text (str
             "Distribute over addition.\n$$" (latex [::equal [::derive func variable] math]) "$$")
     :math math
     :skills #{::add}}))
(defmethod prime-step ::mult [[_ func variable]]
  (cond
    (not (contains? (variance (nth func 1)) variable))
    (let [math [::mult (nth func 1) [::derive (nth func 2) variable]]]
      {:text (str
               "Since $" (latex (nth func 1)) "$ is just a number, then"
               "$$" (latex [::equal [::derive func variable] math]) "$$")
       :math math
       :skills #{::scaler}})
    :else
    (let [math [::add
                [::mult (nth func 1) [::derive (nth func 2) variable]]
                [::mult [::derive (nth func 1) variable] (nth func 2)]]]
      {:text (str "Apply the Product Rule.$$" (latex [::equal [::derive func variable] math]) "$$")
       :math math
       :skills #{::product}})))
(defmethod prime-step ::power [[_ func variable]]
  (let [math [::mult (nth func 2) [::power (nth func 1) (insta-add (nth func 2) -1)]]]
    {:text (str "Apply the Power Rule.\n$$" (latex [::equal [::derive func variable] math]) "$$")
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
      (let [step (prime-step func)
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
    (let [[_ solution equation] func
          variable (if (and (vector? solution) (= (first solution) ::fn)) (last solution) (first (variance equation)))
          derived (prime-dive [::derive equation variable])]
      {:problem (str "Differentiate the function\n$$" (latex func) "$$")
       :steps (:text derived)
       :skills (:skills derived)
       :answer (str "$$" (latex [::equal [::derive solution variable] (:answer derived)]) "$$")})
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

