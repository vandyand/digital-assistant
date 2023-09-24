(ns mimi.bible_app.pit
  (:require [clj-http.client :as http]
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
    (http/post url {:body byte-array-body
                    :headers headers})))

(defn p+ [x]
  x
  (do
    (println x)
    x)
  )

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

;;;;;;;;;;;;;;;;;;;; GENERATE AGENTS ;;;;;;;;;;;;;

(defn gen-agent-body []
  (query-gpt "Please generate an agent prompt for me. The agent will be tasked with summarizing Biblical passages. No ai voice. No ai description. No ai summary."))

(defn gen-agent
  ([] (gen-agent (gen-agent-body)))
  ([body]
   {:id (str "agent-" (.getTime (java.util.Date.)) "-" (rand-int 10000))
    :body body}))

(defn gen-agents [bodies]
  (vec (for [body bodies]
         (gen-agent body))))

(defn gen-agent-bodies [num-bodies]
  (vec (for [_ (range num-bodies)]
         (gen-agent-body))))

(defn mutate-agent [agent, task-desc]
  (gen-agent (query-gpt (str "Please improve this prompt so that it is better able to " task-desc ": "
                             (:body agent)
                             "No ai voice. No ai description. No ai summary."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; SUMMARIZE BIBLE CONTENT ;;;;;;;;;;;;;;;;;;;;;;;;;

(defn summarize [task-desc verse agent]
  (query-gpt (str "Your mission is to: " task-desc ". Here is the verse: " verse ". " (:body agent))))

#_(summarize (get-chapter "genesis" 1) (gen-agent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GET NEXT VERSE ;;;;;;;;;;;;;;;;

(def bible-books
  ["Genesis" "Exodus" "Leviticus" "Numbers" "Deuteronomy" "Joshua" "Judges" "Ruth" "1 Samuel" "2 Samuel"
   "1 Kings" "2 Kings" "1 Chronicles" "2 Chronicles" "Ezra" "Nehemiah" "Esther" "Job" "Psalms" "Proverbs"
   "Ecclesiastes" "Song of Solomon" "Isaiah" "Jeremiah" "Lamentations" "Ezekiel" "Daniel" "Hosea" "Joel"
   "Amos" "Obadiah" "Jonah" "Micah" "Nahum" "Habakkuk" "Zephaniah" "Haggai" "Zechariah" "Malachi"
   "Matthew" "Mark" "Luke" "John" "Acts" "Romans" "1 Corinthians" "2 Corinthians" "Galatians" "Ephesians"
   "Philippians" "Colossians" "1 Thessalonians" "2 Thessalonians" "1 Timothy" "2 Timothy" "Titus" "Philemon"
   "Hebrews" "James" "1 Peter" "2 Peter" "1 John" "2 John" "3 John" "Jude" "Revelation"])

(defn get-verse
  ([ref] (get-verse (:book ref) (:chapter ref) (:verse ref)))
  ([book chapter verse]
   (let [url (str "https://bible-api.com/" book "+" chapter ":" verse)
         response (http/get url {:as :json})]
     (:verses (:body response)))))

(defn get-next-verse-ref
  ([ref] (get-next-verse-ref (:book ref) (:chapter ref) (:verse ref)))
  ([book chapter verse]
   (let [current-book-index (.indexOf bible-books book)
         next-verse (try
                      (get-verse book chapter (inc verse))
                      (catch Exception e nil))]
     (if next-verse
       {:book book :chapter chapter :verse (inc verse)}
       (let [next-chapter (try
                            (get-verse book (inc chapter) 1)
                            (catch Exception e nil))]
         (if next-chapter
           {:book book :chapter (inc chapter) :verse 1}
           (if (< current-book-index (dec (count bible-books)))
             {:book (nth bible-books (inc current-book-index)) :chapter 1 :verse 1}
             {:error "Reached the end of the Bible"})))))))

(defn battle [verse summary1 summary2]
  (query-gpt (str
              "which of these two summaries do you find most insightful and interesting?"
              " here is the summary verse: "
              verse
              " and here are the summaries: "
              "SUMMARY1: " summary1 "\n"
              "SUMMARY2: " summary2 "\n"
              "No ai voice. No ai description. No ai summary. No ai rationale. Please respond ONLY with 'SUMMARY1' or 'SUMMARY2'")))

(let [task-desc "Summarize Biblical Texts"
      agents (gen-agents (gen-agent-bodies 5))
      foo (p+ agents)
      first-ref {:book "genesis" :chapter 1 :verse 1}]
  (loop [i 0 ref first-ref agents agents]
    (let [foo (println "--------------- CHALLENGE #" i ", verse: " ref " -------------------------------")
          agent1 (rand-nth agents) ;; later - enforce unique agents
          agent2 (rand-nth agents)
          verse (get-verse ref)
          foo (p+ {:agent1 agent1})
          foo (p+ {:agent2 agent2})
          summary1 (summarize task-desc verse agent1)
          foo (p+ {:summary1 summary1})
          summary2 (summarize task-desc verse agent2)
          foo (p+ {:summary2 summary2})
          who-won? (battle verse summary1 summary2)
          winner (if (clojure.string/includes? who-won? "SUMMARY1") agent1 agent2)
          loser (if (clojure.string/includes? who-won? "SUMMARY1") agent2 agent1)
          foo (p+ {:who-won? who-won? :id (:id winner)})
          child (mutate-agent winner task-desc)
        ;;   foo (p+ {:child child})
          new-agents+ (vec (conj agents child))
          new-agents (vec (remove #(= (:id %) (:id loser)) new-agents+))
          foo (p+ {:new-agents new-agents})
        ;;   foo (println {:new-agents new-agents})
        ;;   foo (p+ {:len-new-agents (count new-agents)})
          next-verse-ref (get-next-verse-ref ref)
        ;;   foo (p+ {:next-verse-ref next-verse-ref})
          ]
      (if (or (> i 10000) (contains? next-verse-ref :error))
        new-agents
        (recur (inc i) next-verse-ref new-agents)))))
