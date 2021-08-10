(ns spacedmath.eq
  (:require
    [reagent.dom :as rdom]
    [reagent.core :as reagent]
    [clojure.string :refer [capitalize trim]]
    [cljs.core.async :refer [go]]
    [cljs-http.client :as http]
    [spacedmath.problems :as p :refer [parse-mafs mm]])
  (:require-macros
    [utils :as ut]))

(def user (reagent/atom nil))

(go (let [response (<! (http/get "/api/profile"))]
      (reset! user (:body response))))

(defn logout []
  (go (let [response (<! (http/get "/api/logout"))]
        (reset! user ""))))

(defn add-problem [problem answer skills]
  (http/post "/api/problems" {:json-params (into
                                            {:Problem problem :Answer answer}
                                            (map (fn [i] [(capitalize (name i)) 1]) skills))}))

(def equation (reagent/atom ""))
(def output (atom nil))

(defn jsonify [input]
  (.stringify js/JSON (clj->js input)))

(defn handle-add []
  (let [parsed (parse-mafs @equation)
        answer (:answer (p/prime-pattern [::p/derive (last parsed) \x]))
        problem (p/basic-derivation parsed)]
    (add-problem (jsonify parsed) (jsonify answer) (:skills problem))))

(defn render []
  (do
    (-> @output
      (.-innerHTML)
      (set! (let [parsed (parse-mafs @equation)]
              (str
                (mm parsed)
                (if (and (vector? parsed) (= (first parsed) ::p/equal))
                  (let [problem (p/basic-derivation parsed)]
                    (str
                      (map #(name %) (:skills problem))
                      (:answer problem))))))))
    (. js/MathJax typeset)))

(add-watch equation nil render)

(def explanation "
You must be logged in to add equations, but feel free to do so. Note that this project is in active development, and this tool in particular is only intended for internal use. As such, it lacks some “nice-to-haves.” There is no verification of the equations before they are added and there will be no visual indication that the equation was added successfully. You must simply check the main page.

The language used is custom. I call it mafs. It looks like this: `f(x)=(3*(x^2))+(4*x)+5`. You may notice it relies heavily on parenthesis. If an equation doesn’t look right, try adding more parenthesis. The system also requires a left and a right side of an equality, with the left being the function name and the right being the equation to solve. A function name may be in the form of f(x) or just a variable like y, but you may use any single letter function and variable names you’d like. Common function calls like sin(x) may also be used, though I have only implemented trig and exp().

So far, the system should be able to derive polynomials, the exp() function, trig functions, product rule, and quotient rule. More to come.

Try: f(x)=exp(x)+(5*(x^2))+tan(x)+sin(2) Notice that the system not only finds the derivative, but the skills involved in finding it. This is the essence of the system.
  ")

(defn main-panel []
  (reagent/create-class
    {:reagent-render
      (fn []
        [:<>
          [:header
            [:div
              [:div [:a {:href "/"} "Home"]]
              (cond
                (= @user "") [:div "\u202F"
                                           [:a {:id "login" :href "/api/auth/google"} "log in"]]
                (= @user nil) [:div "Checking login status..."]
                :else [:div @user
                       [:span {:id "logout" :on-click #(logout)} "log out"]])]]
          [:p explanation]
          [:main
           [:div
            [:textarea {:value @equation :on-change #(reset! equation (trim (-> % .-target .-value)))}] [:br]
            [:button {:on-click handle-add :disabled (= "" @user)} "Add Problem"]]
           [:div {:style {:margin "10px"} :ref (fn [el] (reset! output el))}]]])}))

(defn init []
  (let [root-el (.getElementById js/document "app")]
    (rdom/render [main-panel] root-el)))
