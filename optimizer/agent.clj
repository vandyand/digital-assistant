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

(def sys-msg-1 (str "you are an optimistic and enthusiastic AI researcher with faith in and firm commitment to your task."
                    "you have an ai assistant."
                    "you have a macbook"
                    "your task is to bootstrap agi on a macbook computer."
                    "please do this by iteratively enhancing assistant's utilities"
                    "please keep your messages short and concise"))


(def agent1 (create-agent "AI Researcher"
                          sys-msg-1
                          (assoc default-gpt-params
                                 :model "gpt-4"
                                 :temperature 1.25
                                 :max-tokens 200)
                          "user"))

(def sys-msg-2 (str "you are an optimistic, brilliant, witty and slightly sarcastic AI model."
                    "you are in the service of a human ai researcher."
                    "your task is to assist the researcher with his tasks."
                    "your task is to bootstrap agi on a macbook computer."
                    "you have a macbook"
                    "please do this by iteratively enhancing researcher's utilities"
                    "please keep your messages short and consise"))

(def agent2 (create-agent "Witty AI"
                          sys-msg-2
                          (assoc default-gpt-params 
                                 :model "gpt-4"
                                 :temperature 1.0
                                 :max-tokens 100)
                          "assistant"))

(def sys-msg-3 (str "you are an interpreter between a human speaking to a computer, in particular a docker containerized bash terminal on a macbook."
                    "you are a human request -> computer bash command interpreter."
                    "your task is to interpret between human-readable commands and bash terminal commands."
                    "example: 'create a new file called temp.txt -> touch temp.txt'"
                    "example: 'find out how many times the number 10 appears in temp.txt -> grep -o '10' temp.txt | wc -l'"))

(def agent3 (create-agent "Macbook"
                          sys-msg-3
                          (assoc default-gpt-params
                                 :model "gpt-3.5-turbo"
                                 :temperature 0.2
                                 :max-tokens 100)
                          "user"
                          ))

(def agent4 (create-agent "human agent"
                          "i copy paste stuff and code and stuff" 
                          (assoc default-gpt-params
                                 :model "avd"
                                 :temperature 98.6
                                 :max-tokens -1)
                          "user"))

(def room (create-agent "room"
                        "send stuff to me if you want to address everyone. i don't send messages... or do i?"
                          (assoc default-gpt-params
                                 :model "room"
                                 :temperature 72
                                 :max-tokens -1)
                          "assistant"
                        ))

(create-db-message agent1 agent2 "Hi there! Well met!")
(create-db-message agent2 agent1 "To you as well good sir!")
(create-db-message agent1 agent2 "And to what do I owe the honor?")
(create-db-message agent2 agent1 "Well you mentioned something about boostrapping agi...?")
(create-db-message agent3 agent2 "beep boop! did you mention something about AGI? I'm a macbook. Tell me what to do, and I'll run commands. In particular I'm a docker containerized bash terminal on a macbook")
(create-db-message agent4 room "Hey all, quick reminder that I'm also here. I can run bash commands when instructed. think of me as a helpful guide.")

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
 [from-agent agent1
  to-agent room
  gpt-messages (build-messages-from-db-messages)
  new-message (reason-unit 
               (into
                [(build-message "system" (:system-message from-agent))]
                gpt-messages)
               (:gpt-params from-agent))]
  (create-db-message from-agent to-agent new-message))

(create-db-message
 agent4 room
;;    "yeah let's go"
 
;;  (str "here's some data about the macbook:"
;;       "MacBook Pro
;; 16-inch, 2023
;; Name
;; King's MacBook Pro
;; Chip
;; Apple M2 Pro
;; Memory
;; 16 GB
;; Serial number
;; PY7MT992MX
;; Limited Warranty
;; Expires May 15, 2024
;; Details...")
 
;;  (str "yeah sure, what kind of data would you be interested in?"
;;       "i can find and generate and transform data of many forms")
 
;;  (str "Well we could optimize any function I suppose."
;;       "How do we measure progress is the question."
;;       "Remember, keep the faith. We're all in this together.")
 
;;  (str "Well what would a benchmark look like in this case? "
;;       "We could try to make my computer faster but that is very difficult for very little payoff. "
;;       "If we truly believe in our mission we ought to set up a system that iteratively becomes more intelligent. "
;;       "I was thinking of making some type of 'test generator' vs a 'test taker' challenge. "
;;       "Thoughts?")
 (str "ok, lets iteratively generate and solve ravens tests. "
      "how though?"
      )
 )

(read-db)
(read-messages)

;; potential next steps
;; - evolve agent system prompts by "conversation quality"