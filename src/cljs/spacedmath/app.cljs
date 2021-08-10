(ns spacedmath.app
  (:require
    [spacedmath.problems :as pr]
    [spacedmath.list :as ls]
    [reagent.dom :as rdom]
    [reagent.core :as reagent]
    [cljs.core.async :refer [go]]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.set :refer [union]]
    [cljs-http.client :as http]
    [cljs.core.async :refer [<!]])
  (:require-macros
    [utils :as ut]))


(def detailed-list (map #(pr/basic-derivation (pr/convert %)) @ls/math-list))

(def available-skills
  (reduce
    (fn [res item]
      (union res (:skills item)))
    #{}
    detailed-list))


(set! *warn-on-infer* false)


(defn json->clj [string]
  (js->clj (.parse js/JSON string)))

(defn keywordify [data]
  (if (vector? data) (into [(keyword (first data))] (map keywordify (rest data))) data))

(def problem-list (reagent/atom []))

(go (let [response (<! (http/get "/api/problems"))]
      (reset! problem-list (map #(keywordify (json->clj (:Problem %))) (:body response)))))


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


(defn main-panel []
  (reagent/create-class
    {:reagent-render
      (fn []
        [:<>
          [:header
            [:div
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
                available-skills)) 
            [:button {:on-click (fn [] (reset! active-filters @filters))}
             "Filter"]
            (let [filt @active-filters]
              (map
                (fn [n] [:div
                           [:input {:type "radio" :name "problems" :value n :on-change #(reset! selected n)}]
                           [:label (pr/im (pr/convert (nth @problem-list n)))]])
                (filter
                  (fn [n]
                    (let [t (nth detailed-list n)]
                      (every? #(contains? (:skills t) %) filt)))
                  (range (count @problem-list)))))
            [:button {:on-click (fn [] (if @selected (reset! math (pr/basic-derivation (pr/convert (nth @problem-list @selected))))))}
             "Update"]]
           [:div {:class "card" :style {:margin "20px"} :ref (fn [el] (reset! target el))}]]])
     :component-did-update card-build}))
    
  
  

(defn init []
  (let [root-el (.getElementById js/document "app")]
    (rdom/render [main-panel] root-el)))
