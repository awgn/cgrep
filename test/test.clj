;; Comment: This is a single-line comment in Clojure.
;; Reserved Keyword: 'ns' is a special form for defining a namespace.
(ns my-app.core)

;; Reserved Keyword: 'def' is a special form for defining a variable (Var).
;; Identifier: 'greeting-message'
(def greeting-message "Hello from Clojure!") ; String Literal (double quotes)

;; 1. Multi-line String Literal (not standard, but common via function, or triple quotes in some contexts)
;; Since triple quotes aren't standard Clojure syntax, we show a standard string with a newline.
(def long-message "Line one.\nLine two.")

;; 2. Character Literal (represented by backslash-notation)
;; Identifier: 'terminator-char'
(def terminator-char \!) ; Char Literal

;; Reserved Keyword: 'defn' is a macro used to define a function (def function).
;; Identifier: 'process-data' (Function identifier)
(defn process-data [input-value]
  ;; Reserved Keyword: 'let' is a special form for defining local bindings (variables).
  (let [
        ;; Identifier: 'local-var'
        local-var (str input-value " processed") ; Function identifier: 'str'
        ]
    ;; Reserved Keyword: 'if' is a special form for conditional logic.
    (if (> (count local-var) 10) ; Function identifier: '>' and 'count'
      ;; Reserved Keyword: 'true' (Boolean literal)
      true
      ;; Reserved Keyword: 'false' (Boolean literal)
      false)))

;; Main function (entry point convention)
;; Identifier: '-main'
(defn -main []
  ;; Function identifier: 'println' (I/O)
  (println greeting-message terminator-char)
  (println (process-data "data"))
  (println long-message))
