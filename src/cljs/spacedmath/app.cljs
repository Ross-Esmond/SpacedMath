(ns spacedmath.app
  (:require
    [spacedmath.problems :as pr]
    [spacedmath.list :as ls]
    [re-frame.core :as re-frame]
    [reagent.dom :as rdom]
    [reagent.core :as reagent]
    [cljs.core.async :refer [go]]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.set :refer [union]]))


(def detailed-list (map #(pr/basic-derivation (pr/convert [:equal \y %])) ls/math-list))

(def available-skills
  (reduce
    (fn [res item]
      (union res (:skills item)))
    #{}
    detailed-list))


(set! *warn-on-infer* false)


(def auth0 (reagent/atom nil))
(def user (reagent/atom nil))
(def log (.-log js/console))


(go
  (let [client (<p! (js/createAuth0Client #js {"domain" "spacedmath.us.auth0.com" "client_id" "RIlgkboudsnnS6LDxnzZdmcV3K6WnKct"}))
        auth_state (<p! (. client isAuthenticated))]
    (do
      (reset! auth0 client)
      (if (not auth_state)
        (let [query (.. js/window -location -search)]
          (if (and (. query includes "code=") (. query includes "state="))
            (do
              (<p! (. @auth0 handleRedirectCallback))
              (. (. js/window -history) replaceState #js {} (. js/document -title) "/")
              (reset! user (<p! (. @auth0 getUser))))
            (reset! user false)))
        (reset! user (<p! (. @auth0 getUser)))))))


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
                (= @user false) [:div "\u202F"
                                 [:button {:id "login" :on-click (fn [] (. @auth0 loginWithRedirect #js {"redirect_uri" "http://localhost:3000/"}))}
                                  "log in"]]
                (= @user nil) [:div "Checking login status..."]
                :else [:div (.-name @user)
                       [:button {:id "logout" :on-click (fn [] (. @auth0 logout #js {"returnTo" (. js/location -origin)}))}
                        "log out"]])]]
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
                           [:label (str "$" (pr/latex (pr/convert (nth ls/math-list n))) "$")]])
                (filter
                  (fn [n]
                    (let [t (nth detailed-list n)]
                      (every? #(contains? (:skills t) %) filt)))
                  (range (count ls/math-list)))))
            [:button {:on-click (fn [] (if @selected (reset! math (pr/basic-derivation (pr/convert [:equal \y (nth ls/math-list @selected)])))))}
             "Update"]]
           [:div {:class "card" :style {:margin "20px"} :ref (fn [el] (reset! target el))}]]])
     :component-did-update card-build}))
    
  
  

(defn init []
  (let [root-el (.getElementById js/document "app")]
    (rdom/render [main-panel] root-el)))
