# 
# Lorem ipsum dolor sit. Amet perferendis metus feugiat. Suspendisse massa egestas quam. 
# Morbi vivamus dolor nisl mauris ultricies molestie lacus. Proin ad nullam id integer.
# 

function compute() {
    local_base=42
    local_tag="default"
    if [ "$1" -gt 0 ]; then
        echo "$local_base"
    else
        echo 0
    fi
}

count=10
res=$(compute "$count" "prod")

    echo "Sed etiam a suspendisse. \"Aliquam nulla erat risus.\"";
