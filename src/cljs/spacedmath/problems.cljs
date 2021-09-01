(ns spacedmath.problems
  (:require 
    [goog.string :as gstring]
    [goog.string.format]
    [clojure.string :as string]
    [clojure.set :refer [intersection map-invert]]
    [instaparse.core :as insta]
    [clojure.core.match :refer [match]]
    [clojure.math.combinatorics :as combo]
    ["mathjs" :as mathjs])
  (:require-macros [utils :as ut]))

(set! *warn-on-infer* false)

(defn convert [target]
  (cond
    (vector? target) (vec (map convert target))
    (set? target) (set (map convert target))
    (number? target) target
    (char? target) target
    (nil? target) nil
    :else (keyword "spacedmath.problems" (name target))))

(defn variance [func]
  (cond
    (char? func) #{func}
    (vector? func) (reduce #(into %1 (variance %2)) #{} (rest func))
    :else #{}))

(defn math-fn [math]
  (cond
    (vector? math) (first math)
    (number? math) ::numeric
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

(declare rule-simplify)
(defn normalize-structure [func]
  (if (not (vector? func)) func
    (let [[operator & operands] func
           root (into [operator] (map normalize-structure operands))]
      (combine-associative-operands root))))

(defn simplify [func] (rule-simplify (normalize-structure func)))

(defmulti pretty-print math-fn)
(defmethod pretty-print ::add [root]
  (let [recursive (into [::add] (map pretty-print (rest root)))
        result
        (reduce
          (fn [stack item]
            (cond
              (and (number? item) (< item 0))
              [[::subtract (if (< 1 (count stack)) (into [::add] stack) (first stack)) (- item)]]
              (and (vector? item) (= ::mult (first item)) (number? (nth item 1)) (< (nth item 1) 0))
              [[::subtract
                (if (< 1 (count stack)) (into [::add] stack) (first stack))
                (into [::mult (- (nth item 1))] (nthrest item 2))]]
              :else
              (conj stack item)))
          [(nth recursive 1)]
          (nthrest recursive 2))]
    (simplify (if (= 1 (count result)) (first result) (into [::add] result)))))
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
(derive ::power ::binary)
(derive ::power ::numbered)
(derive ::root ::operator)
(derive ::div ::binary)

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

(def latex-cost
  {::subtract 3
   ::mult 0
   ::div 1
   ::parens 2
   ::root 1
   ::power 0
   ::exp 1})

(defn latex-score [math]
  (cond
    (= "" math) 0
    (char? math) 1
    (number? math) (count (str math))
    (keyword? math) 1
    :else (match math
            [::add & operands]
            (+ (* 3 (- (count operands) 1)) (reduce + (map latex-score operands)))
            [(operator :guard #(isa? % ::exec)) & operands]
            (+ (count (name operator)) 2 (reduce + (map latex-score operands)))
            [operator & operands]
            (+ (operator latex-cost) (reduce + (map latex-score operands)))
            :else ##Inf)))


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
               (into []
                 (concat
                   current
                   (map
                     (fn [[func res]] {:match [::derive [func \x] \x]
                                       :result res
                                       :skills #{func}
                                       :text (fn [origional solution _]
                                               (str "Use the identity to find"
                                                    (mm [::equal origional solution])))})
                     identities)))))

(swap! rules (fn [current]
               (conj current {:match [::derive [\f [\g \x]] \x]
                              :result [::mult [::derive [\g \x] \x] [::derive [\f [\g \x]] [\g \x]]]
                              :skills #{::chain}
                              :text #(str "")})))

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
        whitelisted (into {} (map (fn [[l t]] [l (if (vector? t) (variance t) t)]) inverted))]
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

(defn cross-product [items] (into [] (apply combo/cartesian-product items)))
(defn in? [coll elm] (some #(= elm %) coll))
(defn combogen [cards slots]
  (filter
    (fn [item] (every? #(in? item %) (range cards)))
    (cross-product (repeat slots (range cards)))))
(defn every-grouping [math amount]
  (->> (combogen amount (- (count math) 1))
    (map
      (fn [combo]
        (into [(first math)]
          (map #(if (= (count %) 2) (last %) %)
            (reduce
              (fn [result [m g]] (assoc result g (conj (get result g) m)))
              (into [] (repeat amount [(first math)]))
              (map (fn [m g] [m g]) (rest math) combo))))))))

(defn math-unify [math pattern]
  (cond
    (or (number? pattern) (keyword? pattern)) (if (= math pattern) [{}] [])
    (char? pattern) [{pattern math}]
    (and (vector? pattern) (char? (first pattern)))
    (if (vector? (last pattern))
      (if (and (vector? math) (vector? (last math)))
        (into [] (map #(clean-merge % {pattern math}) (math-unify (last math) (last pattern))))
        [])
      [{pattern math}])
    (and (vector? math) (vector? pattern))
    (cond
      (and (isa? (first math) ::commutative) (isa? (first math) ::associative))
      (->> (every-grouping math (- (count pattern) 1))
        (map (fn [m] (into [] (remove nil? (map #(reduce clean-merge {} %) (cross-product (map math-unify m pattern)))))))
        (flatten)
        (into []))
      :else
      (into [] (remove nil? (map #(reduce clean-merge {} %) (cross-product (map math-unify math pattern))))))
    :else []))


(defn first-rule [math rules]
  (first (remove #(nil? (first %)) (map #(do [(first (math-unify math (:match %))) %]) rules))))

(defn symbol-replace [math mapping]
  (cond
    (contains? mapping math) (get mapping math)
    (not (vector? math)) math
    :else (into [] (map #(symbol-replace % mapping) math))))

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

(defn detect-variable [func]
  (match func
    [::equal [::fn n a] _] a
    [::equal _ r] (first (variance r))
    :else nil))

(defn basic-derivation [func]
  (if (= (nth func 0) ::equal)
    (let [[_ solution equation] func
          variable (detect-variable func)
          derived (prime-pattern [::derive equation variable])]
      {:raw-problem func
       :problem (str "Differentiate the function" (mm func))
       :steps (:text derived)
       :skills (:skills derived)
       :answer (str (mm [::equal [::derive solution variable] (:answer derived)]))
       :raw-answer (:answer derived)})
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
    [:root input]
    (parse-convert input)
    (:or [:exp input] [:exp "(" input ")"] [:nested input] [:nested "(" input ")"])
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
    [(get {\/ ::div \^ ::power} operator) (parse-convert left) (parse-convert right)]
    [:equality left "=" right]
    [::equal (parse-convert left) (parse-convert right)]
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

(defn parse-mafs [code] (parse-convert (parser code)))

(def simplification-rules
  ["a+0=a"
   "1*a=a"
   "(a/1)=a"
   "(a^1)=a"
   "((a^b)^c)=(a^(b*c))"
   "(root(a,b)^c)=(a^(c/b))"
   "-1*(a/b)=(-1*a)/b"])

(defn gcd [a b] (if (zero? b) a (recur b (mod a b))))

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

(defn numeric? [target]
  (or
    (number? target)
    (match target
      [::div (left :guard number?) (right :guard number?)] true
      :else false)))

(def parsed-rules
  (->> simplification-rules
       (map parse-mafs)
       (map (fn [[_ l r]] [l r]))))

(def actual-comp
  {::add
   (fn [operands]
     (->> operands
       (map get-frac)
       (reduce
         (fn [[_ rn rd] [_ in id]] [::div (+ (* rn id) (* in rd)) (* rd id)])
         [::div 0 1])
       (reduce-div)))
   ::mult
   (fn [operands]
     (->> operands
       (map get-frac)
       (reduce
         (fn [[_ rn rd] [_ in id]] [::div (* rn in) (* rd id)])
         [::div 1 1])
       (reduce-div)))
   ::power
   (fn [operands]
     (match operands
       [[::div bn bd] [::div en ed]] (reduce-div [::div (Math/pow bn (/ en ed)) (Math/pow bd (/ en ed))])
       [[::div bn bd] e] (reduce-div [::div (Math/pow bn e) (Math/pow bd e)])
       [b [::div en ed]] (Math/pow b (/ en ed))
       [b e] (Math/pow b e)))
   ::div
   (fn [[n d]]
     (let [[_ nn nd] (get-frac n)
           [_ dn dd] (get-frac d)]
       (reduce-div [::div (* nn dd) (* nd dn)])))})
(defn compute-numeric [math]
  (if (not (vector? math)) math
    (let [[operation & pre-operators] math
          operators (map compute-numeric pre-operators)
          numeric (group-by numeric? operators)
          requisite-operands 2
          result
          (if (and (<= requisite-operands (count (get numeric true))) (contains? actual-comp operation))
            (let [computed ((operation actual-comp) (get numeric true))]
              (cond
                (= 0 (count (get numeric false))) computed
                (= operation ::mult) (into [operation computed] (get numeric false))
                (= operation ::add) (into [operation] (concat (get numeric false) [computed]))))
            (into [operation] operators))]
      (if (< (latex-score math) (latex-score result)) math result))))

(defn recursive-simplify [math]
  (let [recursed (if (vector? math) (into [] (map recursive-simplify math)) math)
        simplified
        (reduce
          (fn [res [pattern output]]
            (let [match (first (math-unify res pattern))]
              (if (nil? match) res (symbol-replace output match))))
          recursed
          parsed-rules)]
    (if (= simplified math) math (recursive-simplify simplified))))

(defn is-nan? [maybe] (false? (= maybe maybe)))
(defn contains-nan [thing]
  (if (vector? thing) (some contains-nan thing) (is-nan? thing)))

(defn rule-simplify [math]
  (if (contains-nan math) math
    (let [post (compute-numeric (recursive-simplify math))]
      (if (= post math) math (rule-simplify post)))))

(defn obj->clj
  [obj]
  (if (goog.isObject obj)
    (-> (fn [result key]
          (let [v (goog.object/get obj key)]
            (if (= "function" (goog/typeOf v))
              result
              (assoc result key (obj->clj v)))))
        (reduce {} (.getKeys goog/object obj)))
    obj))

(defn mjs->clj [math]
  (match math
    {"op" "+", "args" {"0" l, "1" r}} [::add (mjs->clj l) (mjs->clj r)]
    {"op" "*", "args" {"0" l, "1" r}} [::mult (mjs->clj l) (mjs->clj r)]
    {"op" "^", "args" {"0" l, "1" r}} (let [b (mjs->clj l) p (mjs->clj r)] (if (= b \e) [::exp p] [::power b p]))
    {"op" "/", "args" {"0" l, "1" r}} [::div (mjs->clj l) (mjs->clj r)]
    {"fn" "unaryMinus", "args" {"0" a}} (let [i (mjs->clj a)] (if (number? i) (- i) [::mult -1 i]))
    {"fn" "subtract", "args" {"0" l "1" r}} [::add (mjs->clj l) [::mult -1 (mjs->clj r)]]
    {"type" "AssignmentNode", "object" l, "value" r} [::equal (mjs->clj l) (mjs->clj r)]
    {"type" "FunctionAssignmentNode", "name" n, "params" {"0" a}, "expr" e} [::equal [::fn n (mjs->clj a)] (mjs->clj e)]
    {"type" "FunctionNode", "fn" {"name" (n :guard char?)} "args" {"0" a}} [::fn n (mjs->clj a)]
    {"type" "FunctionNode" "fn" {"name" "root"} "args" {"0" b "1" r}} [::root (mjs->clj b) (mjs->clj r)]
    {"type" "FunctionNode", "fn" {"name" n} "args" {"0" a}} [(keyword "spacedmath.problems" n) (mjs->clj a)]
    {"type" "ParenthesisNode", "content" c} (mjs->clj c)
    {"type" "ConstantNode", "value" v} (js/parseFloat v)
    {"type" "SymbolNode", "name" c} (if (< 1 (count c)) (keyword "spacedmath.problems" c) c)
    :else math))
(defn print-mjs [math]
  (match math
    [::add & r] (str "(" (string/join "+" (map print-mjs r)) ")")
    [::mult & r] (str "(" (string/join "*" (map print-mjs r)) ")")
    [::power l r] (str "(" (print-mjs l) "^" (print-mjs r) ")")
    [::div l r] (str "(" (print-mjs l) "/" (print-mjs r) ")")
    [::root l r] (str "root(" (print-mjs l) "," (print-mjs r) ")")
    [::equal l r] (str (print-mjs l)  "=" (print-mjs r))
    [::fn n a] (str n "(" a ")")
    [(n :guard #(isa? % ::unary)) i] (str (name n) "(" (print-mjs i) ")")
    (c :guard char?) c
    (n :guard #(isa? % ::named)) (name n)
    :else math))

(defn parse-mjs [code]
  (try
    (normalize-structure (mjs->clj (obj->clj (mathjs/parse code))))
    (catch js/Error _ nil)))
(defn prime-mjs [math v] (try
                           (normalize-structure (mjs->clj (obj->clj (mathjs/derivative (print-mjs math) v))))
                           (catch js/Error _ nil)))
(defn simplify-mjs [math] (normalize-structure (mjs->clj (obj->clj (mathjs/simplify (print-mjs math))))))
