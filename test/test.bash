# This is a single-line comment in Bash.
# Reserved keyword: 'function' for defining a function. (Keyword)
function demonstrate_literals() {
    # Variable identifier: 'local_result'
    local_result=0

    # 1. Standard String Literal (Single quotes)
    standard_string='Hello, Bash!'

    # 2. String Literal (Double quotes)
    another_string="This is a standard string."

    # 3. Conditional check using reserved keywords: 'if', 'then', 'else', 'fi'
    if [ "$local_result" -eq 0 ]; then
        first_char="H"
    else
        first_char="None"
    fi
}

# Variable assignments
final_count=42
final_data="test"

demonstrate_literals "$final_data" "$final_count"
echo "Result: $final_data, Count: $final_count"
