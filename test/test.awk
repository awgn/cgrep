# This block runs BEFORE any input line is processed.
BEGIN {
    # Initialize a counter for the lines that meet our criteria
    count = 0
    # Set the field separator (FS) to a comma, useful for CSV-like data
    FS = ","
    # Print a header for the output
    print "--- Filtered Data ---"
}

# This is the main action block, which runs for every line.
# The action is only performed if the pattern "TEST" or "FAILURE" is found in the line.
/TEST|FAILURE/ {
    # Print the line number (NR) and the entire line ($0)
    # $0 represents the entire record (line)
    print NR ": " $0

    # Increment the counter for matching lines
    count++
}

# This block runs AFTER all input lines have been processed.
END {
    # Print the final count of matching lines
    print "--- Summary ---"
    print "Total matching lines found: " count
}
