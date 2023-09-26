(ns mimi.optimizer.agent
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]))

(defn parse-response-body [response]
  (json/read-str (response :body) :key-fn keyword))

(defn post-gpt
  ([messages gpt-params]
   (println "post-gpt args" messages gpt-params)
   (let [url "https://api.openai.com/v1/chat/completions"
         headers {"Content-Type" "application/json"
                  "Authorization" (str "Bearer " (System/getenv "OPENAI_API_KEY"))}
         body {:model (:model gpt-params)
               :messages messages
               :temperature (:temperature gpt-params)
               :max_tokens (:max_tokens gpt-params)
               :n (:n gpt-params)}
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
      :content))

(defn build-message [role content]
  {:role role
   :content content})

(def default-messages
  [(build-message "system" "you are a helpful agent.")
   (build-message "user" "what is the square root of -1?")])

(defn get-roles-vec [n]
  (vec (cons "system" (take (dec n) (cycle ["user" "assistant"])))))

(defn build-messages-from-strings [& messages]
  (let [n (count messages)
        roles (get-roles-vec n)]
    (mapv #(build-message %1 %2) roles messages)))

(defn build-gpt-params [model temperature max-tokens n]
  {:model model
   :temperature temperature
   :max_tokens max-tokens
   :n n})

(defn reason-unit [messages gpt-params]
  (get-first-message-content (post-gpt messages gpt-params)))

#_(reason-unit default-messages default-gpt-params)

#_(reason-unit
   (build-messages-from-strings "sys message" "what up broooooo?" "hey you know me man. how's it going?" "yeah yeah I need your help with something" "ok, shoot bro" "ok, what's the koochies kooch you've ever done?")
   default-gpt-params)

#_(reason-unit
   (build-messages-from-strings "you are an expert theologian" "what did Paul mean when he said 'For me to live is Christ'?")
   (build-gpt-params "gpt-4" 1.2 100 1))

; CRUD system using atoms
(def db (atom []))

(defn generate-id []
  (str (java.util.UUID/randomUUID)))

(defn create [record]
  (let [_record (assoc record :id (generate-id) :timestamp (System/currentTimeMillis))]
    (swap! db conj _record)
    _record))

(defn read-db
  ([] @db)
  ([id]
   (some #(when (= (:id %) id) %) @db)))

(defn update-db [id updated-record]
  (let [index (some #(when (= (:id (nth @db %)) id) %) (range (count @db)))]
    (when index
      (swap! db assoc-in [index] (assoc updated-record :id id)))))

(defn delete [id]
  (let [index (some #(when (= (:id (nth @db %)) id) %) (range (count @db)))]
    (when index
      (swap! db remove #(= (:id %) id)))))

#_(let [john (create {:name "john" :age 34})
      jill (create {:name "jill" :age 33})
      jane (create {:name "jane" :age 31})
      joan (create {:name "joan" :age 36})
      jack (create {:name "jack" :age 35})
      jeff (create {:name "jeff" :age 35})]
  (read-db)
  (read-db (:id john))
  (update-db (:id john) {:name :John :age 34})
  (read-db (:id john)))

(defn create-agent [name system-message gpt-params role]
  (create {:name name
           :system-message system-message
           :gpt-params gpt-params
           :type ::agent
           :role role}))

(defn create-db-message [from-agent to-agent content]
  (let [message-record {:from-agent-id (:id from-agent)
                        :role (:role from-agent)
                        :to-agent-id (:id to-agent)
                        :content content
                        :type ::message}]
    (create message-record)))

(def db (atom []))

(def agent1 (create-agent "AI Researcher"
                          "you are a curious and enthusiastic AI researcher."
                          (assoc default-gpt-params
                                 :model "gpt-4"
                                 :temperature 1.25
                                 :max-tokens 100)
                          "user"))

(def agent2 (create-agent "Witty AI"
                          "you are a witty and slightly sarcastic AI model."
                          (assoc default-gpt-params 
                                 :model "gpt-4"
                                 :temperature 1.0
                                 :max-tokens 100)
                          "assistant"))

(create-db-message agent1 agent2 "Hi there! Well met!")
(create-db-message agent2 agent1 "To you as well good sir!")
(create-db-message agent1 agent2 "And to what do I owe the honor?")
(create-db-message agent2 agent1 "Well you mentioned something about boostrapping agi...?")

(defn read-messages []
  (filter #(= ::message (:type %)) @db))

(defn read-last-messages ([] (read-last-messages 7))
  ([x] (take-last 
   (if (int? x) x 7) 
   (sort-by :timestamp (read-messages)))))

(defn get-agents []
  (filter #(= ::agent (:type %)) @db))

(defn get-agent-by-id [id]
  (first (filter #(= id (:id %)) (get-agents))))

(defn parse-db-message [db-message]
  {:role (:role db-message)
   :content (:content db-message)})

(defn build-messages-from-db-messages 
  ([] (build-messages-from-db-messages (read-messages)))
  ([db-messages] (mapv parse-db-message db-messages)))

#_(build-messages-from-db-messages)

(let 
 [from-agent agent2
  to-agent agent1
  gpt-messages (build-messages-from-db-messages)
  new-message (reason-unit 
               (into
                [(build-message "system" (:system-message from-agent))]
                gpt-messages)
               (:gpt-params from-agent))]
  (create-db-message from-agent to-agent new-message))

(read-db)
(read-messages)

;; potential next steps
;; - evolve agent system prompts by "conversation quality"