(ns mimi.optimizer.main
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
              :temperature 0.35
              :max_tokens 2560}
        json-body (json/write-str body)
        byte-array-body (.getBytes json-body "UTF-8")]
    (http/post url {:body byte-array-body
                    :headers headers})))

(defn p+ [x]
  x
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


(defn gen-prompt []
  ;; Generate a random computational prompt
  )

(defn fitness [prompt]
  ;; Evaluate the fitness of the prompt
  )

(defn select [population]
  ;; Select prompts from the population for reproduction
  )

(defn crossover [prompt1 prompt2]
  ;; Combine prompts to create a new prompt
  )

(defn mutate [prompt]
  ;; Randomly modify the prompt
  )

(defn evolve [population]
  (let [selected (select population)
        offspring (map #(crossover (rand-nth selected) (rand-nth selected)) (range (count population)))
        mutated (map mutate offspring)]
    (concat selected mutated)))

(defn run-genetic-algorithm []
  (let [initial-population (repeatedly 100 gen-prompt)]
    (loop [population initial-population
           generation 0]
      (if (or (>= generation 10000) (some #(>= (fitness %) 0.99) population))
        (first (sort-by fitness > population))
        (do
          (println "Generation" generation "Best fitness:" (fitness (first (sort-by fitness > population))))
          (recur (evolve population) (inc generation)))))))