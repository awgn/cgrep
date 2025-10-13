# Comment: This is a standard single-line comment in CoffeeScript.

###
Multi-line comment block.
Reserved keyword: 'class' (for defining classes, though not used here).
###

# Reserved keyword: 'my_var' is an identifier.
my_var = 10 

# Reserved keyword: 'if' (Keyword)
if my_var > 5
    # Identifier: 'is_large'
    # Reserved keyword: 'true' (Boolean literal)
    is_large = true 

# Reserved keyword: 'func' is a macro/keyword for defining a function (an identifier).
# Function identifier: 'processData'
processData = (input_value) ->
    # 1. Standard String Literal (double quotes for interpolation)
    # Identifier: 'result_msg'
    result_msg = "The value is #{input_value}."

    # 2. String Literal (single quotes for literal string, no interpolation)
    # Identifier: 'literal_msg'
    literal_msg = 'This text will not interpolate #{variables}.'

    # 3. Block String Literal (multi-line string literal using triple quotes)
    long_string = """
        This text spans multiple lines.
        It preserves formatting and newlines.
    """
    
    # Implicit return of the function
    return [result_msg, literal_msg, long_string]

# Execution and output
# Identifier: 'results'
[results, literal, long] = processData(my_var)

# Reserved keyword: 'console.log' (via JavaScript environment)
console.log(results)
console.log(long)
