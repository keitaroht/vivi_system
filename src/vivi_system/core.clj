(ns vivi-system.core
  (:require [clojure.string :as str])
  (:gen-class))


(def op_char \()
(def cp_char \))

(def query_filename "resources/fake_queries/test.sql")

(defn remove_comments
  [query]
  (str/replace (str/replace query #"(?s)/\*.*?\*/" "") #"--.*" "")
)

(defn collapse_white_space
  [query]
  (str/replace query #"\s+" " ")
)

(defn consume_string_until_conditions
  (
    [string]
    (consume_string_until_conditions string #"(?i)([\,\"\(\)]| FROM )")
  )

  (
    [string regex]
    (def split_result (str/split string regex))
    (if (= (count split_result) 1)
      (println "********\nERRO LENDO SELECT\n********")
    )

    (def consumed_part (nth split_result 0))
    (def match_char (nth string (count consumed_part)))
    (def not_consumed_part (subs string (inc (count consumed_part))))
    ; (println (str \' consumed_part \'))
    ; (println match_char)
    (list consumed_part match_char not_consumed_part)
  )
)

(defn get_select_field
  [string]
  (loop [select_field "" remaining_str string num_open_par 0]
    (let [[consumed, match_char, not_consumed] (consume_string_until_conditions remaining_str)]
      ; (println (str \' consumed \'))
      (def new_select_field (str select_field consumed))
      (cond
        (= match_char op_char)
          (recur (str new_select_field op_char) not_consumed (inc num_open_par))
        (= match_char cp_char)
          (recur (str new_select_field cp_char) not_consumed (dec num_open_par))
        (= match_char \")
          (let [[more_consumed, we, not_consumed] (consume_string_until_conditions not_consumed #"\"")]
            (recur (str new_select_field \" more_consumed \") not_consumed num_open_par)
          )
        (> num_open_par 0)
          (recur (str new_select_field match_char) not_consumed num_open_par)
        (or (= match_char \,) (= match_char \space))
          ; finalmente...
          (list (str/trim new_select_field) not_consumed)

        :else (println (str "********\nNEM SEI OQ ACONTECEU! (" match_char ")\n********"))
      )
    )
  )
)

(defn get_select_fields
  [query]
  (def removed_select_query (subs query (count "SELECT ")))
  (
    loop [fields '() remaining_query removed_select_query]
    (if (str/starts-with? (str/lower-case remaining_query) "from")
      fields
      (let [[new_field, remaining_query] (get_select_field remaining_query)]
        (recur (concat fields [new_field]) remaining_query)
      )
    )
  )
)

(defn separate_field_expression
  [select_field]
  (if (str/includes? (str/lower-case select_field) " as ")
    ; using AS
    (str/split select_field #"(?i) as ")

    ; not using AS

    ; entire epression as its name
    (list select_field select_field)
  )
)

(defn get_select_map
  "returns a map with {field_name field_expr}"
  [query]
  
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (def queries (str/split (slurp query_filename) #";"))

  ; test
  (def original_query (nth queries 1))
  
  (def cleaned_query (str/trim (collapse_white_space (remove_comments original_query))))

  (println "\n=== cleaned query ===")
  (println cleaned_query)

  (println "\n=== get select fields ===")
  ; (println
  ;   (nth (get_select_field cleaned_query) 0)
  ; )
  ; (println (get_select_map cleaned_query))
  (def select_fields (get_select_fields cleaned_query))
  (dorun (map #(println %) select_fields))

  (println "\n=== parse select fields ===")
  (def test_field (nth select_fields 0))
  (println test_field)
  (println (separate_field_expression test_field))
  

)
