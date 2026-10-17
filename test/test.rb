# 
# Lorem ipsum dolor sit. Amet perferendis metus feugiat. Suspendisse massa egestas quam. 
# Morbi vivamus dolor nisl mauris ultricies molestie lacus. Proin ad nullam id integer.
# 

=begin
  Eget orci rutrum vel. Elit nullam amet integer. Fusce tellus ut massa. Maecenas risus dictum risus. 
  Augue aliquam molestie id. Commodo ultricies pede massa fusce ullamcorper dapibus dui. 
  Maecenas elementum duis porttitor facilisis lectus eleifend nec. Arcu et pellentesque 
  tellus non tristique suscipit nec. Tempor iaculis orci nec enim ac.
=end

def compute(x, label = "default")
    base = 42
    if x > 0
        return x + base
    else
        return 0
    end
end

count = 10
res = compute(count, "prod")

    puts "Sed etiam a suspendisse. \"Aliquam nulla erat risus.\"";
    puts 'Sed etiam a suspendisse. \'Aliquam nulla erat risus.\'';
    puts %q(Sed etiam a suspendisse. 'Aliquam nulla erat risus.')
    puts %Q(Sed etiam a suspendisse. 'Aliquam nulla erat risus.')
    puts  %|Sed etiam a suspendisse. 'Aliquam nulla erat risus.'|
