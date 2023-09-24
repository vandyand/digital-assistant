(ns mimi.bible_app.runner
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]))

(defn parse-response-body [response]
  (json/read-str (response :body) :key-fn keyword))

(defn post-gpt [query]
  (let [url "https://api.openai.com/v1/chat/completions"
        headers {"Content-Type" "application/json"
                 "Authorization" (str "Bearer " (System/getenv "OPENAI_API_KEY"))}
        body {:model "gpt-3.5-turbo"
              :messages [{:role "user" :content query}]
              :temperature 0.7}
        json-body (json/write-str body)
        byte-array-body (.getBytes json-body "UTF-8")]
    (client/post url {:body byte-array-body
                      :headers headers})))

(defn p+ [x]
  (do
    (println x)
    x))

(defn query-gpt [query]
  (-> query
      post-gpt
      parse-response-body
      :choices
      first
      :message
      :content
    ;;   p+
      ))

(defn gen-agent-body []
  (query-gpt "Please generate a sentance of random words for me. No ai voice. No ai description. No ai summary."))

(defn gen-agent 
  ([] (gen-agent (gen-agent-body)))
  ([body] 
  {:id (str "agent-" (.getTime (java.util.Date.)) "-" (rand-int 10000))
   :body body}))

;; (gen-agent (gen-agent-body))

(defn gen-agents [bodies]
  (for [body bodies]
    (gen-agent body)))

(defn gen-agent-bodies [num-bodies]
  (for [_ (range num-bodies)]
    (gen-agent-body)))


