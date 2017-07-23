(ns vivi-system.core
  (:require [clojure.string :as str])
  (:gen-class))

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
    (consume_string_until_conditions string #"(?i)(.*?)([\,\"\(\)]|\bFROM\b)")
  )

  (
    [string regex]

    (let [[match consumed stop] (re-find regex string)]
      (if (nil? match)
        match
        (list consumed stop (subs string (count match)))
      )
    )
  )
)

(defn get_select_field
  [string]
  (loop [select_field "" remaining_str string num_open_par 0]
    (let [[consumed, match, not_consumed] (consume_string_until_conditions remaining_str)]
      ; (println (str \' consumed \'))
      (def new_select_field (str select_field consumed))
      (cond
        (= match "(")
          (recur (str new_select_field "(") not_consumed (inc num_open_par))
        (= match ")")
          (recur (str new_select_field ")") not_consumed (dec num_open_par))
        (= match "\"")
          (let [[more_consumed, we, not_consumed] (consume_string_until_conditions not_consumed #"(.*?)\"")]
            (recur (str new_select_field \" more_consumed \") not_consumed num_open_par)
          )
        (> num_open_par 0)
          (recur (str new_select_field match) not_consumed num_open_par)
        (= match ",")
          ; finalmente...
          (list (str/trim new_select_field) not_consumed)
        (= match "FROM")
          ; finalmente...
          (list (str/trim new_select_field) (str "FROM" not_consumed))

        :else (println (str "********\nNEM SEI OQ ACONTECEU! (" match ")\n********"))
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
      (list fields remaining_query)
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

(defn remove_function_calls
  [string]
  (str/replace string #"\w+\s*(?=\()" "")
)

(def select_keywords '("case" "when" "end" "then" "else"))
; (def select_keywords_regex (re-pattern (str "(?i)(" (str/join "|" (map #(str "(\\W|^)" % "(?=(\\W|$))") select_keywords)) ")")))
(def select_keywords_regex (re-pattern (str "(?i)(" (str/join "|" (map #(str "\\b" % "\\b") select_keywords)) ")")))
(println select_keywords_regex)

(defn remove_sql_keywords
  [string]
  (str/trim (str/replace string select_keywords_regex ""))
)

(defn remove_sql_strings
  [string]
  (loop [cleaned_str "" remaining_string string]
    (let [[first_consumed we first_not_consumed] (consume_string_until_conditions remaining_string #"(.*?)\"")]

      (if (nil? first_consumed)
        (str cleaned_str remaining_string)
        (let [[second_consumed we second_not_consumed] (consume_string_until_conditions first_not_consumed #"(.*?)\"")]
          ; (println cleaned_str "||" first_consumed)
          ; (println second_consumed "||" second_not_consumed)
          (recur (str cleaned_str first_consumed) (str second_not_consumed))
        )
      )
    )
  )
)

(defn get_used_table_fields
  [select_expr]
  (let [cleaned_expr (remove_function_calls (remove_sql_keywords (remove_sql_strings select_expr)))]
    (re-seq #"(?i)[a-z][\w\.]*" cleaned_expr)
  )
)

(defn separate_table_from_column
  [string]
  (let [string_parts (str/split string #"\.")]
    (cond
      (= (count string_parts) 1)
        nil
      :else
        {:table (str/join "." (butlast string_parts))
         :column (last string_parts)
        }
    )
  )
)

(defn get_select_map
  "returns a map with {field_name field_expr}"
  [query]
  (def fields (map separate_field_expression (nth (get_select_fields query) 0)))

  (reduce
    (fn
      [select_map [field_exp field_name]]
      (assoc select_map field_name field_exp)
    )
    {} fields
  )
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (def queries (str/split (slurp query_filename) #";"))

  ; test
  (def original_query (nth queries 0))

  (def cleaned_query (str/trim (collapse_white_space (remove_comments original_query))))

  (println "\n=== cleaned query ===")
  (println cleaned_query)

  (println "\n=== get select fields ===")
  ; (println
  ;   (nth (get_select_field cleaned_query) 0)
  ; )
  ; (println (get_select_map cleaned_query))
  (def select_fields (nth (get_select_fields cleaned_query) 0))
  (def query_after_from (nth (get_select_fields cleaned_query) 1))
  (dorun (map #(println %) select_fields))

  (println "\n=== parse select fields ===")
  (def test_field (nth select_fields 4))
  ; (println test_field)

  (def test_expr (nth (separate_field_expression test_field) 0))
  ; (println (get_used_table_fields test_expr))

  (println (map separate_table_from_column (get_used_table_fields test_expr)))

  (println "\n=== parse from and joins ===")
  (println query_after_from)

  (def from_regex #"(?i)(.*?)(JOIN|INNER JOIN|LEFT JOIN|RIGHT JOIN|OUTER JOIN|WHERE|GROUP BY|LIMIT)")
  (def return1 (consume_string_until_conditions query_after_from from_regex))
  (println "match:" (nth return1 1))
  (def return2 (consume_string_until_conditions (nth return1 2) from_regex))
  (println "match:" (nth return2 1))


  ; (println (remove_sql_strings "yayay \\o/\"tem q sair\" naynay\"esse tb\""))
  ; (println (remove_sql_keywords "case when pc.is_private_label then 1 when pc.is_crossdocking then 1 else 1 end"))

  ; (println "\n=== final map ===")
  ; (println (get_select_map cleaned_query))
)
