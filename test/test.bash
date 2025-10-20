# This is a single-line comment.
# Reserved keyword: 'def' for defining a function. (Keyword)
def demonstrate_literals(input_value):
    # Variable identifier: 'local_result'
    local_result = 0
    
    # 1. Standard String Literal (Single quotes)
    standard_string = 'Hello, Python!'
    
    # 2. String Literal (Double quotes)
    another_string = "This is a standard string."

    # 3. Raw String Literal (Ignores backslashes) (Raw String)
    raw_path = r'C:\Users\Name\Desktop\test.txt'
    
    # 4. Multi-line String (Triple quotes)
    long_text = """
    This string spans
    multiple lines.
    """
    
    # 5. Character Literal (Python treats single characters as strings of length 1)
    # Reserved keyword: 'if' (Keyword)
    if len(standard_string) > 0:
        # Character literal represented as a single-character string.
        first_char = standard_string[0] # first_char is 'H'
        
        # Reserved keyword: 'return'
        return standard_string, raw_path, first_char

# Variable identifier: 'final_data'
final_data, raw_info, initial = demonstrate_literals("test")

# Reserved keyword: 'print'
print(f"Result: {final_data}, Initial: {initial}")
