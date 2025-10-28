;; Clojure test example file

(ns test-clojure-example
  (:require [clojure.test :refer :all]))

;; Regular helper function (not a test)
(defn add [a b]
  (+ a b))

;; Another helper function
(defn multiply [x y]
  (* x y))

;; Helper function
(defn factorial [n]
  (if (<= n 0)
    1
    (* n (factorial (dec n)))))

;; Regular record (not a test)
(defrecord Calculator [value])

;; Helper functions for Calculator
(defn make-calculator
  ([] (->Calculator 0))
  ([initial] (->Calculator initial)))

(defn add-to-calc [calc n]
  (update calc :value + n))

(defn get-value [calc]
  (:value calc))

(defn reset-calc [calc]
  (assoc calc :value 0))

;; clojure.test tests with deftest
(deftest test-addition
  (testing "addition works correctly"
    (let [result (add 2 3)]
      (is (= 5 result)))))

(deftest test-multiplication
  (testing "multiplication works"
    (let [result (multiply 4 5)]
      (is (= 20 result)))))

(deftest test-factorial
  (testing "calculates factorial of 5"
    (let [result (factorial 5)]
      (is (= 120 result))))
  
  (testing "handles zero"
    (let [result (factorial 0)]
      (is (= 1 result)))))

;; Helper function (not a test)
(defn process-data [coll]
  (->> coll
       (filter pos?)
       (map #(* 2 %))))

;; Calculator tests
(deftest test-calculator-init
  (testing "Calculator starts with zero"
    (let [calc (make-calculator)]
      (is (= 0 (get-value calc))))))

(deftest test-calculator-with-value
  (testing "Calculator can be initialized with value"
    (let [calc (make-calculator 10)]
      (is (= 10 (get-value calc))))))

(deftest test-calculator-operations
  (testing "Calculator operations"
    (testing "adds numbers correctly"
      (let [calc (make-calculator)
            calc (-> calc
                     (add-to-calc 5)
                     (add-to-calc 10))]
        (is (= 15 (get-value calc)))))
    
    (testing "handles negative numbers"
      (let [calc (make-calculator)
            calc (add-to-calc calc -5)]
        (is (= -5 (get-value calc)))))
    
    (testing "reset works"
      (let [calc (make-calculator)
            calc (add-to-calc calc 100)
            calc (reset-calc calc)]
        (is (= 0 (get-value calc)))))))

;; Helper functions (not tests)
(defn reverse-string [s]
  (apply str (reverse s)))

(defn capitalize-string [s]
  (clojure.string/upper-case s))

;; String tests
(deftest test-string-operations
  (testing "String operations"
    (testing "reverses a string"
      (is (= "olleh" (reverse-string "hello"))))
    
    (testing "capitalizes a string"
      (is (= "WORLD" (capitalize-string "world"))))
    
    (testing "handles empty string"
      (is (= "" (reverse-string ""))))))

;; Regular record (not a test)
(defrecord Person [name age])

(defn make-person [name age]
  (->Person name age))

;; Helper function
(defn format-number [n]
  (format "%.2f" n))

;; More helper code
(defn filter-positive [data]
  (filter pos? data))

;; Test with are for multiple assertions
(deftest test-multiple-additions
  (testing "Multiple addition cases"
    (are [x y expected] (= expected (add x y))
      1 1 2
      2 3 5
      10 20 30
      -5 5 0)))

;; Run all tests
(run-tests)