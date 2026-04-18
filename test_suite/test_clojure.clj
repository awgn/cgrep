(defn compute-cgrep [x]
  (def cgrep_prod_1 1)
  (* x 2))

(deftest my-test
  (def cgrep_test_1 1)
  (testing "inner"
    (def cgrep_test_2 2)))

(defn runTestHarness-cgrep [x]
  (def cgrep_prod_2 2)
  (+ x 1))
