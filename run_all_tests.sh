#!/usr/bin/env bash

for file in test_suite/*; do
    echo "====================================="
    echo "Testing: $file"
    echo "--- Prod (-T False) ---"
    stack run -- -S cgrep_ -T False "$file" | grep cgrep_ | head -n 3
    echo "--- Test (-T True) ---"
    stack run -- -S cgrep_ -T True "$file" | grep cgrep_ | head -n 3
    echo "--- Prod Strict (--strict -T False) ---"
    stack run -- --strict -S cgrep_ -T False "$file" | grep cgrep_ | head -n 3
    echo "--- Test Strict (--strict -T True) ---"
    stack run -- --strict -S cgrep_ -T True "$file" | grep cgrep_ | head -n 3
    echo "====================================="
done
