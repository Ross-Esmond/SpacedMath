(ns spacedmath.problems
  (:require 
    [goog.string :as gstring]
    [goog.string.format]
    [clojure.string :as string]
    [clojure.set :refer [intersection map-invert]]
    [instaparse.core :as insta]
    [clojure.core.match :refer [match]])
  (:require-macros [utils :as ut]))

(defn convert [target]
  (cond
    (vector? target) (vec (map convert target))
    (set? target) (set (map convert target))
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

(defn ensure-vector [item] (if (vector? item) item [item]))

(defmulti latex-multi math-fn)
(defn latex [item] (if (isa? item ::operator) (latex-multi (ensure-vector item)) (latex-multi item)))
(defmethod latex-multi ::exec [func] (str "\\" (name (first func)) (parens (latex (first (rest func))))))
(defmethod latex-multi ::symbol [sym] sym)
(defmethod latex-multi ::named [sym] (str "\\" (str (name sym) " ")))
(defmethod latex-multi ::add [func] (string/join " + " (map latex (rest func))))
(defmethod latex-multi ::subtract [[_ l r]] (str (latex l) " - " (latex r)))
(defmethod latex-multi ::exp [func] (str "e^{" (latex (first (rest func))) "}"))
(defmethod latex-multi ::numeric [number] (str number))
(defmethod latex-multi ::mult [func]
  (string/join
    (first
      (reduce
        (fn [[result prev] item]
          [(into
             result
             (cond
               (= -1 item) ["-"]
               (or
                 (and (number? item) (number? prev))
                 (and (vector? item) (or (= ::add (first item)) (= ::subtract (first item))))) [(parens (latex item))]
               :else [(latex item)]))
           item])
        [[] nil]
        (rest func)))))
(defmethod latex-multi ::power [func]
  (let [target (nth func 1)
        printed (if (or (char? target) (number? target)) target (parens (latex target)))]
    (str printed "^{" (latex (nth func 2)) "}")))
(defmethod latex-multi ::derive [func]
  (let [[_ eq target] func]
    (if (and (vector? eq) (= (first eq) ::fn))
      (let [[_ ident variable] eq]
        (str ident "'(" variable ")"))
      (str "\\frac{d}{d" (nth func 2) "}" (parens (latex (nth func 1)))))))
(defmethod latex-multi ::equal [func] (str (latex (nth func 1)) " = " (latex (nth func 2))))
(defmethod latex-multi ::div [func]
  (str "\\frac{" (latex (nth func 1)) "}{" (latex (nth func 2)) "}"))
(defmethod latex-multi ::fn [[_ ident variable]] (str ident \( variable \)))
(defmethod latex-multi ::root [[_ target root]] (str "\\sqrt[" (latex root) "]{" (latex target) "}"))
(defmethod latex-multi :default [_] "Nothing")

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

(defmulti pretty-print math-fn)
(defmethod pretty-print ::add [root]
  (let [recursive (into [::add] (map pretty-print (rest root)))
        result
        (reduce
          (fn [stack item]
            (cond
              (and (number? item) (< item 0))
              [::add [::subtract stack (- item)]]
              (and (vector? item) (= ::mult (first item)) (number? (nth item 1)) (< (nth item 1) 0))
              [::add [::subtract stack (into [::mult (- (nth item 1))] (nthrest item 2))]]
              :else
              (conj stack item)))
          (into [] (take 2 recursive))
          (nthrest recursive 2))]
    (simplify result)))
(defmethod pretty-print :default [math] (if (not (vector? math)) math
                                          (let [[func & items] math]
                                            (into [func] (map pretty-print items)))))


(defn im [eq] (str "$" (latex (pretty-print eq)) "$"))
(defn mm [eq] (str "$$" (latex (pretty-print eq)) "$$"))


(def skills (atom #{::add ::power ::chain ::const}))

(derive ::binary ::operator)
(derive ::add ::binary)
(derive ::mult ::binary)
(derive ::unary ::operator)
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
(derive ::power ::operator)
(derive ::power ::numbered)
(derive ::root ::operator)

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

(def greek #{::pi})

(swap! skills (fn [sk] (into sk (keys identities))))

(doseq [n (keys identities)] (derive n ::ident))

(doseq [n greek] (derive n ::named))


(def rules
  (atom
    [{:match [::derive \c \x]
      :result 0
      :skills #{::const}
      :text #(str "The expression " (im (nth % 1)) " is just a number, so its derivative is 0."
                  (mm [::equal %1 %2]))}
     {:match [::derive \x \x]
      :result 1
      :skills #{}
      :text #(str "The derivative of " (im (nth % 1)) " is just 1.\n"
                  (mm [::equal %1 %2]))}
     {:match [::derive [::add [\f \x] [\g \x]] \x]
      :result [::add [::derive [\f \x] \x] [::derive [\g \x] \x]]
      :skills #{::add}
      :text #(str "Distribute derivation over addition."
                  (mm [::equal %1 %2]))}
     {:match [::derive [::mult \c [\f \x]] \x]
      :result [::mult \c [::derive [\f \x] \x]]
      :skills #{::scaler}
      :text #(str
               "Since " (im (get %3 \c)) " is just a number, then"
               (mm [::equal %1 %2]))}
     {:match [::derive [::mult [\f \x] [\g \x]] \x]
      :result [::add [::mult [::derive [\f \x] \x] [\g \x]] [::mult [\f \x] [::derive [\g \x] \x]]]
      :skills #{::product}
      :text #(str
               "Apply the Product Rule."
               (mm [::equal %1 %2]))}
     {:match [::derive [::div \c [\f \x]] \x]
      :result [::derive [::mult \c [::power [\f \x] -1]] \x]
      :skills #{}
      :text #(str "Invert the denominator."
                  (mm [::equal %1 %2]))}
     {:match [::derive [::power \x \c] \x]
      :result [::mult \c [::power \x [::add \c -1]]]
      :skills #{::power}
      :text #(str "Apply the power rule."
                  (mm [::equal %1 %2]))}
     {:match [::derive [::root \x \c] \x]
      :result [::derive [::power \x [::div 1 \c]] \x]
      :skills #{}
      :text #(str "Convert root to an exponent."
                  (mm [::equal %1 %2]))}
     {:match [::derive [::div [\f \x] [\g \x]] \x]
      :result [::div
               [::add [::mult [\g \x] [::derive [\f \x] \x]] [::mult -1 [\f \x] [::derive [\g \x] \x]]]
               [::power [\g \x] 2]]
      :skills #{::div}
      :text #(str "Apply the Quotient Rule."
                  (mm [::equal %1 %2]))}]))


(swap! rules (fn [current]
               (concat
                 current
                 (map
                   (fn [[func res]] {:match [::derive [func \x] \x]
                                     :result res
                                     :skills #{func}
                                     :text (fn [origional solution _]
                                             (str "Use the identity to find"
                                                  (mm [::equal origional solution])))})
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
      {:problem (str "Differentiate the function" (mm func))
       :steps (:text derived)
       :skills (:skills derived)
       :answer (str (mm [::equal [::derive solution variable] (:answer derived)]))})
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

(def parser (insta/parser (ut/swig "mafs.bnf")))

(defn parse-convert [node]
  (match node
    (:or [:exp input] [:exp "(" input ")"])
    (parse-convert input)
    [:multary left operator & remainder]
    (into
      [(get {\* ::mult \+ ::add} operator) (parse-convert left)]
      (map parse-convert (remove #(= % operator) remainder)))
    (:or [:multary "(" left operator & remainder] [:multary left operator & remainder])
    (into
      [(get {\* ::mult \+ ::add} operator) (parse-convert left)]
      (map parse-convert (remove #(or (= % operator) (= % ")")) remainder)))
    (:or [:binary "(" left operator right ")"] [:binary left operator right])
    [(get {\/ ::div \^ ::power \= ::equal} operator) (parse-convert left) (parse-convert right)]
    [:call ident _ args _]
    (into
      [(convert (keyword (string/join (map parse-convert (rest ident)))))]
      (map parse-convert (remove #(= % ",") (rest args))))
    [:number & digits]
    (js/parseFloat (string/join digits))
    [:function ident "(" variable ")"]
    [::fn (parse-convert ident) (parse-convert variable)]
    [:name & letters]
    (convert (keyword (string/join (map parse-convert letters))))
    [:char letter]
    letter
    :else
    node))

(defn parse-mafs [code] (simplify (parse-convert (parser code))))
