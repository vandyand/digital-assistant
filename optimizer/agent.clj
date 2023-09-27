(ns mimi.optimizer.agent
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]))

(defn parse-response-body [response]
  (json/read-str (response :body) :key-fn keyword))

(defn post-gpt
  ([messages gpt-params]
   (println "post-gpt args" messages gpt-params)
   (try
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
                       :headers headers}))
     (catch Exception e
       (println "error: " e)))))

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

(defn build-db-record [from-agent to-agent content]
  {:from-agent-id (:id from-agent)
   :role (:role from-agent)
   :to-agent-id (:id to-agent)
   :content content
   :type ::message})

(defn create-db-record [from-agent to-agent content]
  (let [message-record (build-db-record from-agent to-agent content)]
    (create message-record)))

(def db (atom []))

(defn read-db-records []
  (filter #(= ::message (:type %)) @db))

(defn read-last-messages ([] (read-last-messages 7))
  ([x] (take-last
        (if (int? x) x 7)
        (sort-by :timestamp (read-db-records)))))

(defn get-agents []
  (filter #(= ::agent (:type %)) @db))

(defn get-agent-by-id [id]
  (first (filter #(= id (:id %)) (get-agents))))

(defn parse-db-record [db-record]
  {:role (:role db-record)
   :content (:content db-record)})

(defn build-messages-from-db-records
  ([] (build-messages-from-db-records (read-db-records)))
  ([db-records] (mapv parse-db-record db-records)))

#_(build-messages-from-db-records)

#_(let
   [from-agent agent1
    to-agent room
    gpt-messages (build-messages-from-db-records)
    new-message (reason-unit
                 (into
                  [(build-message "system" (:system-message from-agent))]
                  gpt-messages)
                 (:gpt-params from-agent))]
    (create-db-record from-agent to-agent new-message))

(defn iterate-blob-content [blob agent]
  (let [messages [(build-message "system" (:system-message agent))
                  (build-message "user" (str (:content blob) "\n\n"))]]
    (reason-unit messages (:gpt-params agent))))

#_(let [mission "Create a data representation of iteratively self-improving artificial digital life"
        adder-agent (create-agent
                     "grower"
                     (str "Please add data to 'Blob' object according to mission. Please remain concise and to the point.\n\n"
                          "Mission: " mission "\n\n")
                     (assoc default-gpt-params :max_tokens 256 :temperature 1.0)
                     "user")
        pruner-agent (create-agent
                      "pruner"
                      (str "Please prune data from 'Blob' object according to mission. Please return pruned 'Blob' object. Please remain consise and to the point.\n\n"
                           "Mission: " mission "\n\n")
                      (assoc default-gpt-params :max_tokens 256 :temperature 1.0)
                      "user")
        blob (create-db-record adder-agent pruner-agent "Example content")]
    (println "blob: " (read-db (:id blob)))
    (loop [i 0]
      (if (< i 7)
        (let [new-blob-content (iterate-blob-content (read-db (:id blob)) adder-agent)
              var (update-db (:id blob) (build-db-record adder-agent pruner-agent new-blob-content))
              pruned-blob-content (iterate-blob-content (read-db (:id blob)) pruner-agent)
              var (update-db (:id blob) (build-db-record pruner-agent adder-agent pruned-blob-content))]
          (recur (inc i)))
        blob)))

(read-db)

;; potential next steps
;; - evolve agent system prompts by "conversation quality"