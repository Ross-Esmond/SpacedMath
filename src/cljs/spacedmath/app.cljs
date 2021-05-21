(ns spacedmath.app
  (:require
    [spacedmath.problems :as pr]
    [re-frame.core :as re-frame]
    [reagent.dom :as rdom]
    [reagent.core :as reagent]
    [cljs.core.async :refer [go]]
    [cljs.core.async.interop :refer-macros [<p!]]))


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


(defn main-panel []
  (reagent/create-class
    {:reagent-render
      (fn []
        [:<>
          [:header
            [:div
              (cond
                (= @user false) [:button {:id "login" :on-click (fn [] (. @auth0 loginWithRedirect #js {"redirect_uri" "http://localhost:3000/"}))} "log in"]
                (= @user nil) [:div "Checking login status..."]
                :else [:div (.-name @user) [:button {:id "logout" :on-click (fn [] (. @auth0 logout #js {"returnTo" (. js/location -origin)}))} "log out"]])]]
          [:main "$" (pr/latex [::pr/exp ::pr/x]) "$"]])
        
     :component-did-mount (fn [] (. js/MathJax typeset))}))
    
  

(defn init []
  (let [root-el (.getElementById js/document "app")]
    (rdom/render [main-panel] root-el)))
