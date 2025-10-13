// Comment: This is a single-line comment in Chapel.
// Reserved keyword: 'use' is for importing modules.
use IO;

// Reserved keyword: 'proc' defines a procedure (function).
// Identifier: 'processData'
proc processData(count: int): int {
    // Identifier: 'my_status'
    // Reserved keyword: 'var' for mutable variables.
    var my_status: string; 
    
    // 1. Standard String Literal (String Literal)
    my_status = "Processing start..."; 
    
    // 2. Reserved keyword: 'if' (Keyword)
    if count > 10 {
        // Identifier: 'local_flag'
        // Reserved keyword: 'const' for immutable variables.
        const local_flag = true; 
        
        // Reserved keyword: 'return'
        return 1;
    } else { // Reserved keyword: 'else' (Keyword)
        return 0;
    }
}

// Function identifier: 'main' (The entry point procedure)
// Reserved keyword: 'config' defines a command-line configurable variable.
config const GREETING = "Hello, Chapel!"; // String Literal

proc main() {
    // 3. Multi-line String Literal (using triple quotes)
    const long_message = """
    This is line one.
    This is line two.
    """;
    
    // 4. Character Literal (Chapel uses single quotes for characters)
    // Identifier: 'separator_char'
    const separator_char = '!'; // Char Literal
    
    // Function identifier: 'processData' is called.
    var status_code = processData(12);

    // Reserved keyword: 'writeln' (Built-in I/O procedure)
    writeln(GREETING, separator_char);
    writeln("Status code: ", status_code);
    writeln(long_message);
}
