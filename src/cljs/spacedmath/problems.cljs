(ns spacedmath.problems
  (:require 
    [goog.string :as gstring]
    [goog.string.format]
    [clojure.string :as string]
    [clojure.set :refer [intersection map-invert]])
  (:require-macros [utils :as ut]))

(defn convert [target]
  (cond
    (vector? target) (vec (map convert target))
    (number? target) target
    (char? target) target
    :else (keyword "spacedmath.problems" (name target))))

(defn variance [func]
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
(defmethod latex ::named [sym] (str "\\" (str (name sym) " ")))
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
(defmethod latex ::power [func]
  (let [target (nth func 1)
        printed (if (or (char? target) (number? target)) target (parens (latex target)))]
    (str printed "^{" (latex (nth func 2)) "}")))
(defmethod latex ::derive [func]
  (let [[_ eq target] func]
    (if (and (vector? eq) (= (first eq) ::fn))
      (let [[_ ident variable] eq]
        (str ident "'(" variable ")"))
      (str "\\frac{d}{d" (nth func 2) "}" (parens (latex (nth func 1)))))))
(defmethod latex ::equal [func] (str (latex (nth func 1)) " = " (latex (nth func 2))))
(defmethod latex ::div [func]
  (str "\\frac{" (latex (nth func 1)) "}{" (latex (nth func 2)) "}"))
(defmethod latex ::fn [[_ ident variable]] (str ident \( variable \)))
(defmethod latex ::root [[_ target root]] (str "\\sqrt[" (latex root) "]{" (latex target) "}"))
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

(def identities {::sin [::cos \x]
                 ::cos [::mult -1 [::sin \x]]
                 ::tan [::power [::sec \x] 2]
                 ::cot [::mult -1 [::power [::csc \x] 2]]
                 ::sec [::mult [::sec \x] [::tan \x]]
                 ::csc [::mult -1 [::csc \x] [::cot \x]]
                 ::exp [::exp \x]})

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

(defn consolidate-exponents [root]
  (if (or
        (not (vector? root))
        (not (= (first root) ::power))
        (not (vector? (nth root 1)))
        (let [nested (first (nth root 1))] (not (or (= nested ::power) (= nested ::root)))))
    root
    (let [[power [nested inner a] b] root]
      (if (= nested ::power)
        [::power inner (* a b)]
        [::power inner [::div b a]]))))

(defn addition [root]
  (if (or (not (vector? root)) (not (= (first root) ::add))) root
    (let [[stack sum]
          (reduce
            (fn [[math sum] item] (if (number? item) [math (+ item sum)] [(conj math item) sum]))
            [[::add] 0]
            (rest root))]
      (conj stack sum))))

(defn simplify [func]
  (cond
    (vector? func)
    (let [[operator & operands] func
           root (into [operator] (map simplify operands))]
      (-> root
        addition
        combine-associative-operands
        consolidate-negation
        remove-inconsequential-operators
        consolidate-exponents))
    :else func))

(def greek #{::pi})

(swap! skills (fn [sk] (into sk (keys identities))))

(doseq [n (keys identities)] (derive n ::ident))

(doseq [n greek] (derive n ::named))

(defn distrinput [target input]
  (into [] (map
             (fn [item]
               (if (= item \x) input (if (vector? item) (distrinput item input) item)))
             target)))

(defn is-math? [target math]
  (and (vector? target) (= math (first target))))

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
  (cond
    (or (is-math? a ::div) (is-math? b ::div))
    (let [[_ a-num a-den] (get-frac (crunch-numbers a))
          [_ b-num b-den] (get-frac (crunch-numbers b))]
      (reduce-div [::div (+ (* a-num b-den) (* b-num a-den)) (* a-den b-den)]))
    (and (number? a) (number? b))
    (+ a b)
    :else
    [::add a b]))

(defn insta-flip [a]
  (let [[_ numer denom] (get-frac a)]
    [::div denom numer]))

(defn insta-invert [a]
  (if (not (vector? a)) [::power a -1]
    (let [[func & remainder] a]
      (cond
        (= func ::power) [::power (first remainder) (- (last remainder))]
        (= func ::root) [::power (first remainder) [::mult -1 (insta-flip (last remainder))]]
        :else [::power a -1]))))


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
  (let [math (distrinput ((first func) identities) (last func))]
    {:text (str "Use the identity to find$$" (latex [::equal [::derive func variable] math]) "$$")
     :math math
     :skills #{(first func)}}))
(defmethod prime-step ::add [[_ func variable]]
  (let [math (into [::add] (map (fn [_] [::derive _ variable]) (rest func)))]
    {:text (str
             "Distribute over addition.\n$$" (latex [::equal [::derive func variable] math]) "$$")
     :math math
     :skills #{::add}}))
(defmethod prime-step ::mult [[_ func variable]]
  (let [f-stat (group-by #(if (contains? (variance %) variable) \t \f) (rest func))]
    (cond
      (= 1 (count (get f-stat \t)))
      (let [nums (into [::mult] (get f-stat \f))
            math (conj nums [::derive (first (get f-stat \t)) variable])]
        {:text (str
                 "Since $" (latex nums) "$ is just a number, then"
                 "$$" (latex [::equal [::derive func variable] math]) "$$")
         :math math
         :skills #{::scaler}})
      (= 2 (count (get f-stat \t)))
      (let [math [::add
                  [::mult (first (get f-stat \t)) [::derive (last (get f-stat \t)) variable]]
                  [::mult [::derive (first (get f-stat \t)) variable] (last (get f-stat \t))]]]
        {:text (str "Apply the Product Rule.$$" (latex [::equal [::derive func variable] math]) "$$")
         :math math
         :skills #{::product}}))))
(defmethod prime-step ::power [[_ func variable]]
  (let [math [::mult (nth func 2) [::power (nth func 1) (insta-add (nth func 2) -1)]]]
    {:text (str "Apply the Power Rule.\n$$" (latex [::equal [::derive func variable] math]) "$$")
     :math math
     :skills #{::power}}))
(defmethod prime-step ::div [[_ [_ numer denom] variable]]
  (cond
    (contains? (variance denom) variable)
    (let [math [::mult numer (insta-invert denom)]]
      {:text (str "Invert the denominator."
                  "$$" (latex [::equal [::div numer denom] math]) "$$")
       :math [::derive math variable]
       :skills #{}})))
(defmethod prime-step ::root [[_ [_ target root] variable]]
  (let [math [::power target (insta-flip root)]]
    {:text (str "Convert the root to an exponent.$$" (latex [::equal [::root target root] math]) "$$")
     :math [::derive math variable]
     :skills #{}}))
(defmethod prime-step :default [[_ func variable]]
  {:text (str "Structure " (name (first func)) " not supported.")
   :math 0
   :skills #{}})
   

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


(def rules
  (atom
    [{:match [::derive \c \x]
      :result 0
      :skills #{::const}
      :text #(str "The expression $" (latex (nth % 1)) "$ is just a number, so its derivative is 0."
                  "$$" (latex [::equal %1 %2]) "$$")}
     {:match [::derive \x \x]
      :result 1
      :skills #{}
      :text #(str "The derivative of $" (latex (nth % 1)) "$ is just 1.\n"
                  "$$" (latex [::equal %1 %2]) "$$")}
     {:match [::derive [::add [\f \x] [\g \x]] \x]
      :result [::add [::derive [\f \x] \x] [::derive [\g \x] \x]]
      :skills #{::add}
      :text #(str "Distribute derivation over addition."
                  "$$" (latex [::equal %1 %2]) "$$")}
     {:match [::derive [::mult \c [\f \x]] \x]
      :result [::mult \c [::derive [\f \x] \x]]
      :skills #{::scaler}
      :text #(str
               "Since $" (latex (get %3 \c)) "$ is just a number, then"
               "$$" (latex [::equal %1 %2]) "$$")}
     {:match [::derive [::mult [\f \x] [\g \x]] \x]
      :result [::add [::mult [::derive [\f \x] \x] [\g \x]] [::mult [\f \x] [::derive [\g \x] \x]]]
      :skills #{::product}
      :text #(str
               "Apply the Product Rule."
               "$$" (latex [::equal %1 %2]) "$$")}
     {:match [::derive [::div \c [\f \x]] \x]
      :result [::derive [::mult \c [::power [\f \x] -1]] \x]
      :skills #{}
      :text #(str "Invert the denominator."
                  "$$" (latex [::equal %1 %2]) "$$")}
     {:match [::derive [::power \x \c] \x]
      :result [::mult \c [::power \x [::add \c -1]]]
      :skills #{::power}
      :text #(str "Apply the power rule."
                  "$$" (latex [::equal %1 %2]) "$$")}
     {:match [::derive [::root \x \c] \x]
      :result [::derive [::power \x [::div 1 \c]] \x]
      :skills #{}
      :text #(str "Convert root to an exponent."
                  "$$" (latex [::equal %1 %2]) "$$")}]))


(swap! rules (fn [current]
               (concat
                 current
                 (map
                   (fn [[func res]] {:match [::derive [func \x] \x]
                                     :result res
                                     :skills #{func}
                                     :text (fn [origional solution _]
                                             (str "Use the identity to find"
                                                  "$$" (latex [::equal origional solution]) "$$"))})
                   identities))))

(defn my-keys [item]
  (let [key-store (keys item)]
    (if (= nil key-store) '#{} (into #{} key-store))))

(defn variance-invert [mapping]
  (let [backwards (map-invert mapping)
        pairs (into [] backwards)
        variant (map (fn [[a b]] [(variance a) b]) pairs)
        spread (mapcat (fn [[a b]] (map (fn [c] [c b]) a)) variant)]
    (into {} spread)))

(defn spare-variance-invert [mapping]
  (let [inverted (variance-invert mapping)
        whitelisted (into {} (map (fn [[l t]] [l (if (vector? t) (into #{} (rest t)) t)]) inverted))]
    whitelisted))

(defn value-compatible? [a b]
  (do
    (cond
      (and (char? a) (char? b)) (= a b)
      (and (set? a) (set? b)) true
      (and (set? a) (char? b)) (contains? a b)
      (and (set? b) (char? a)) (contains? b a)
      :default false)))

(defn clean-merge [a b]
  (if (or (= nil a) (= nil b)) nil
    (let [a-inverse (spare-variance-invert a)
          b-inverse (spare-variance-invert b)
          val-collisions (intersection (my-keys a-inverse) (my-keys b-inverse))
          val-compatible (every? #(value-compatible? (get a-inverse %) (get b-inverse %)) val-collisions)
          key-collisions (intersection (my-keys a) (my-keys b))
          key-compatible (every? #(= (get a %) (get b %)) key-collisions)]
      (if (and val-compatible key-compatible) (merge a b) nil))))

(defn math-match [math pattern]
  (cond
    (char? pattern) {pattern math}
    (and (keyword? math) (= math pattern)) {}
    (vector? pattern)
    (cond
      (char? (first pattern))
      {pattern math}
      (vector? math)
      (cond
        (= (count math) (count pattern))
        (let [mapped (map math-match math pattern)]
          (reduce clean-merge mapped))
        (and (= (first math) (first pattern)) (isa? (first pattern) ::associative))
        (let [[operator left & right] math
              fixed [operator left (into [operator] right)]]
          (math-match fixed pattern))))))

(defn first-rule [math rules]
  (first (remove #(nil? (first %)) (map #(do [(math-match math (:match %)) %]) rules))))

(defn symbol-replace [math mapping]
  (if (not (vector? math)) math
    (into [] (map
               (fn [item]
                 (if (contains? mapping item) (get mapping item) (if (vector? item) (symbol-replace item mapping) item)))
               math))))

(defn prime-pattern [func]
  (if (not (vector? func))
    {:text []
     :skills #{}
     :answer func}
    (let [simpler (simplify func)]
      (if (= ::derive (first simpler))
        (let [[mapping rule] (first-rule simpler @rules)
              next-math (simplify (symbol-replace (:result rule) mapping))
              recursion (prime-pattern next-math)
              textual (if (not (fn? (:text rule))) "Failed to solve." ((:text rule) simpler next-math mapping))]
          {:text (concat [textual] (:text recursion))
           :skills (into (:skills rule) (:skills recursion))
           :answer (simplify (:answer recursion))})  
        (reduce (fn [stack op]
                  (let [result (prime-pattern op)]
                    {:text (concat (:text stack) (:text result))
                     :skills (into (:skills stack) (:skills result))
                     :answer (conj (:answer stack) (:answer result))}))
                {:text [] :skills #{} :answer [(first simpler)]}
                (rest simpler))))))


(defn basic-derivation [func]
  (if (= (nth func 0) ::equal)
    (let [[_ solution equation] func
          variable (if (and (vector? solution) (= (first solution) ::fn)) (last solution) (first (variance equation)))
          derived (prime-pattern [::derive equation variable])]
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

