(ns spacedmath.eq
  (:require
    [reagent.dom :as rdom]
    [reagent.core :as reagent]
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

(def equation (reagent/atom ""))
(def output (atom nil))

(defn render [] 
  (do
    (-> @output
      (.-innerHTML)
      (set! (let [parsed (parse-mafs @equation)]
              (str
                (mm parsed)
                (if (and (vector? parsed) (= (first parsed) ::p/equal)) (:answer (p/basic-derivation parsed)))))))
    (. js/MathJax typeset)))

(add-watch equation nil render)

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
           [:textarea {:value @equation :on-change #(reset! equation (-> % .-target .-value))}]
           [:div {:style {:margin "10px"} :ref (fn [el] (reset! output el))}]]])}))

(defn init []
  (let [root-el (.getElementById js/document "app")]
    (rdom/render [main-panel] root-el)))
