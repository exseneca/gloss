(ns gloss.server
  (:require [gloss.stemmer :as stemmer]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [ring.adapter.jetty :as jetty]
            [hiccup.core :as h]
            [reitit.ring :as ring]
            [ring.util.response :as ring-resp]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.util.codec :as codec]
            [ring.middleware.params :refer [wrap-params]]))

(defn gloss-handler [request]
  (println (:params request))
  (let [input-text (get-in request [:params :input-text])
        tokens (str/split input-text #"[\s|\.|-|,|?|¿|:|'|\"]")
        lower-tokens (map str/lower-case tokens)
        stemmed (mapv stemmer/stem lower-tokens)
        distinct-stemmed (frequencies stemmed)
        sorted (rest (sort-by second #(compare %2 %1) distinct-stemmed))] ;; Take rest to remove the space
    (println "INPUT TEXT:" input-text)
    (println "TOKENS: " tokens)
    (println "STEMMED: " stemmed)
    (ring-resp/redirect (str "/"
                             "?gloss="
                             (codec/url-encode sorted)))))

(defn home-handler [req]
  (println "query-params: " (:query-params req))
  (let [gloss (-> (get-in req [:query-params "gloss"])
                  (codec/url-decode)
                  (edn/read-string))]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body
     (str (h/html
           [:html
            [:head [:title "Stem"]]
            [:body [:h1 "Stem"]
             [:form {:action "/gloss" :method "POST"
                     :accept-charset "UTF-8"}
              [:textarea {:name "input-text"}]
              [:button {:type "submit"} "Gloss"]]
             (when gloss
               [:ul
                (for [[word freq] gloss]
                  [:li [:b word] " " freq])])]]))}))

(defn router []
  (ring/router [["/" {:get home-handler}]
                ["/gloss" {:post gloss-handler}]]))

(defn app []
  (ring/ring-handler
   (router) nil
   {:middleware [wrap-params
                 wrap-keyword-params]}))

(defonce server (atom nil))

(defn start-server []
  (reset! server
          (jetty/run-jetty (app) {:port 8080 :join? false}))
  (println "server started on port 8080"))

(defn stop-server []
  (when @server
    (.stop @server)
    (reset! server nil)
    (println "stopped server")))

(defn reset []
  (stop-server)
  (start-server))

;; PROBLEMS: stop words?
;; STEMMER cuts things too short for definitions
;; SOME ENCODING ISSUE
