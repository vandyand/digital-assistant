(ns mimi.optimizer.agent
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]))

(defn parse-response-body [response]
  (json/read-str (response :body) :key-fn keyword))

(defn post-gpt 
  ([query-params]
   (println query-params)
  (let [url "https://api.openai.com/v1/chat/completions"
        headers {"Content-Type" "application/json"
                 "Authorization" (str "Bearer " (System/getenv "OPENAI_API_KEY"))}
        body {:model (:model query-params)
              :messages [{:role "user" :content (:query query-params)}]
              :temperature (:temperature query-params)
              :max_tokens (:max_tokens query-params)}
        json-body (json/write-str body)
        byte-array-body (.getBytes json-body "UTF-8")]
    (http/post url {:body byte-array-body
                    :headers headers}))))
(defn p+ [x]
  x
  (do
    (println x)
    x))

(defn get-query-params 
  ([query] (get-query-params query "gpt-3.5-turbo"))
  ([query model] (get-query-params query model 1.2))
  ([query model temperature] (get-query-params query model temperature 256))
  ([query model temperature max-tokens] (get-query-params query model temperature max-tokens 1))
  ([query model temperature max-tokens n]
  {:model model
   :query query
;;    :messages [{:role "user" :content query}]
   :temperature temperature
   :max_tokens max-tokens
;;    :n n
   }))

(defn *query-gpt [query-params] 
  (-> (post-gpt query-params)
      parse-response-body
      :choices
      first
      :message
      :content
    ;;   p+
      ))

(defn query-gpt [& args]
  (*query-gpt (apply get-query-params args)))

(let [x "What did the cyclopse say to the upside-down turtle?"]
  (query-gpt x "gpt-4" 0.5))