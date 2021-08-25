(ns spacedmath.app
  (:require
    [spacedmath.problems :as pr]
    [reagent.dom :as rdom]
    [reagent.core :as reagent]
    [cljs.core.async :refer [go]]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.set :refer [union]]
    [cljs-http.client :as http]
    [cljs.core.async :refer [<!]])
  (:require-macros
    [utils :as ut]))

(set! *warn-on-infer* false)


(defn json->clj [string]
  (js->clj (.parse js/JSON string)))

(defn keywordify [data]
  (if (vector? data) (into [(keyword (first data))] (map keywordify (rest data))) data))

(def problem-list (reagent/atom []))
(def available-skills (reagent/atom []))

(go (let [response (<! (http/get "/api/problems"))]
      (let [prl (map (fn [r] [r (keywordify (json->clj r))]) (:body response))]
        (reset! problem-list prl)
        (reset! available-skills
                (reduce
                  (fn [res item]
                    (union res (:skills (pr/basic-derivation (pr/convert item)))))
                  #{}
                  (map last prl))))))

(def user (reagent/atom nil))

(go (let [response (<! (http/get "/api/profile"))]
      (reset! user (:body response))))

(defn logout []
  (go (let [response (<! (http/get "/api/logout"))]
        (reset! user ""))))

(def math (reagent/atom (pr/basic-derivation (pr/convert [:equal \y [:add [:power \x 5] [:power 5 6]]]))))

(def filters (reagent/atom #{}))
(def active-filters (reagent/atom @filters))

(def selected (reagent/atom nil))

(def target (atom nil))

(defn card-build []
  (do
    (-> @target
      (.-innerHTML)
      (set! (str
              "Skills: " (map #(name %) (:skills @math)) "<br /><br />"
              (:problem @math)
              (clojure.string/join " " (:steps @math))
              (:answer @math))))
    (. js/MathJax typeset)))

(add-watch math nil card-build)

(defn handle-update [] (if @selected (reset! math (pr/basic-derivation (pr/convert (nth (map last @problem-list) @selected))))))
(defn handle-delete [ev]
  (if (= (.-ctrlKey ev) true)
    (let [target (first (nth @problem-list @selected))]
      (go (let [response (<! (http/delete "/api/problems" {:json-params {:Problem target}}))
                data (json->clj (:body response))]
            (if data (swap! problem-list #(remove (fn [p] (= (first p) target)) %))))))))

(defn main-panel []
  (reagent/create-class
    {:reagent-render
      (fn []
        [:<>
          [:header
            [:div
              [:div [:a {:href "/eq.html"} "Problem Editor"]]
              (cond
                (= @user "") [:div "\u202F"
                                           [:a {:id "login" :href "/api/auth/google"} "log in"]]
                (= @user nil) [:div "Checking login status..."]
                :else [:div @user
                       [:span {:id "logout" :on-click #(logout)} "log out"]])]]
          [:main 
           [:nav
            (let [filt @filters]
              (map
                (fn [skill]
                  [:div
                   [:input {:type "checkbox"
                            :on-change (fn [e] (if
                                                 (-> e .-target .-checked)
                                                 (swap! filters #(conj % skill))
                                                 (swap! filters #(disj % skill)))) 
                            :checked (skill filt)}]
                   [:label (name skill)]])
                @available-skills))
            [:button {:on-click (fn [] (reset! active-filters @filters))}
             "Filter"]
            (let [filt @active-filters]
              (map
                (fn [n] [:div
                           [:input {:type "radio" :name "problems" :value n :on-change #(reset! selected n)}]
                           [:label (pr/im (pr/convert (nth (map last @problem-list) n)))]])
                (filter
                  (fn [n]
                    (let [t (nth (map last @problem-list) n)]
                      (every? #(contains? (:skills t) %) filt)))
                  (range (count (map last @problem-list))))))
            [:button {:on-click handle-update} "Update"]
            [:button {:on-click handle-delete} "Ctrl-Click to Delete"]]
           [:div {:class "card" :style {:margin "20px"} :ref (fn [el] (reset! target el))}]]])
     :component-did-update card-build}))
    
  
  

(defn init []
  (let [root-el (.getElementById js/document "app")]
    (rdom/render [main-panel] root-el)))
