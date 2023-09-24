(ns mimi.bible_app.bible-app
  (:require [clj-http.client :as http]))

(defn get-chapter [book chapter]
  (let [url (str "https://bible-api.com/" book "+" chapter)
        response (http/get url {:as :json})]
    (:verses (:body response))))

(defn -main [& args]
  (println (get-chapter "genesis" 1)))

(-main)

