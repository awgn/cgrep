# 
# Lorem ipsum dolor sit. Amet perferendis metus feugiat. Suspendisse massa egestas quam. 
# Morbi vivamus dolor nisl mauris ultricies molestie lacus. Proin ad nullam id integer.
# 

def compute(x, label="default"):
    base = 42
    if x > 0:
        return x + base
    return 0

count = 10
res = compute(count, "prod")

print """Sed etiam a suspendisse. \"Aliquam nulla erat risus. """
print '''Sed etiam a suspendisse. "Aliquam nulla erat risus. '''

print "Sed etiam a suspendisse. \"Aliquam nulla erat risus.\"";
print 'Sed etiam a suspendisse. "Aliquam nulla erat risus."';
