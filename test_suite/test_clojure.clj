(defn compute-cgrep [x]
  (def cgrep_prod_1 1)
  (* x 2))

;; Production function named `testing` called with a non-string argument:
;; `clojure.test/testing` requires a string description as its first argument!
(defn check-status [state]
  (def cgrep_prod_3 3)
  (testing cgrep_prod_4))

(deftest my-test
  (def cgrep_test_1 1)
  (testing "inner"
    (def cgrep_test_2 2)))

;; Namespace-qualified `t/deftest`, `clojure.test/deftest`, private `deftest-`, and `defspec`:
(t/deftest namespaced-test
  (def cgrep_test_3 3)
  (t/testing "namespaced testing"
    (def cgrep_test_4 4)))

(deftest- private-unit-test
  (def cgrep_test_5 5))

(defspec property-based-test 100
  (def cgrep_test_6 6))

(defn runTestHarness-cgrep [x]
  (def cgrep_prod_2 2)
  (+ x 1))
