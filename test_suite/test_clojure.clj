(defn compute-cgrep [x]
  (def CGREP_IDENTIFIER 1)
  (* x 2))

(deftest my-test
  (def CGREP_IDENTIFIER_TEST 1)
  (testing "inner"
    (def CGREP_IDENTIFIER_TEST 2)))

(defn runTestHarness-cgrep [x]
  (def CGREP_IDENTIFIER 2)
  (+ x 1))
