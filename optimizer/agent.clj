(ns mimi.optimizer.agent
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]))

(defn parse-response-body [response]
  (json/read-str (response :body) :key-fn keyword))

(defn post-gpt 
  ([messages query-params]
   (println "post-gpt args" messages query-params)
  (let [url "https://api.openai.com/v1/chat/completions"
        headers {"Content-Type" "application/json"
                 "Authorization" (str "Bearer " (System/getenv "OPENAI_API_KEY"))}
        body {:model (:model query-params)
              :messages messages
              :temperature (:temperature query-params)
              :max_tokens (:max_tokens query-params)
              :n (:n query-params)}
        json-body (json/write-str body)
        byte-array-body (.getBytes json-body "UTF-8")]
    (http/post url {:body byte-array-body
                    :headers headers}))))

(def default-gpt-params 
  {:model "gpt-3.5-turbo"
   :temperature 0.7
   :max_tokens 100
   :n 1})

(defn get-first-message-content [response]
  (-> response
      parse-response-body
      :choices
      first
      :message
      :content
      ))

(defn build-message [role content]
  {:role role
   :content content})


(def default-messages
  [(build-message "system" "you are a helpful agent.")
   (build-message "user" "what is the square root of -1?")])

(defn get-roles-vec [n]
  (vec (cons "system" (take (dec n) (cycle ["user" "assistant"])))))

(defn build-messages [& messages]
  (let [n (count messages)
        roles (get-roles-vec n)]
  (mapv #(build-message %1 %2) roles messages)))

(defn build-gpt-params [model temperature max-tokens n]
  {:model model
   :temperature temperature
   :max_tokens max-tokens
   :n n})

(defn reason-unit [messages gpt-params]
  (parse-response-body (post-gpt messages gpt-params)))

#_(reason-unit default-messages default-gpt-params)

#_(reason-unit
   (build-messages "sys message" "what up broooooo?" "hey you know me man. how's it going?" "yeah yeah I need your help with something" "ok, shoot bro" "ok, what's the koochies kooch you've ever done?")
   default-gpt-params)