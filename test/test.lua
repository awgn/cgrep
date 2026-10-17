-- 
-- Lorem ipsum dolor sit. Amet perferendis metus feugiat. Suspendisse massa egestas quam. 
-- Morbi vivamus dolor nisl mauris ultricies molestie lacus. Proin ad nullam id integer.
-- 

--[[
Eget orci rutrum vel. Elit nullam amet integer. Fusce tellus ut massa. Maecenas risus dictum risus. 
Augue aliquam molestie id. Commodo ultricies pede massa fusce ullamcorper dapibus dui. 
Maecenas elementum duis porttitor facilisis lectus eleifend nec. Arcu et pellentesque 
tellus non tristique suscipit nec. Tempor iaculis orci nec enim ac.
--]]

local function compute(x, label)
    local base = 42
    local tag = "default"
    if x > 0 then
        return x + base
    else
        return 0
    end
end

local count = 10
local res = compute(count, "prod")

    print "Sed etiam a suspendisse. \"Aliquam nulla erat risus.\""
    print 'Sed etiam a suspendisse. "Aliquam nulla erat risus.'

    print [===[Sed etiam a suspendisse. [==[ Aliquam nulla erat risus. ]===]
    print [==[Sed etiam a suspendisse. [=[ Aliquam nulla erat risus. ]==]
    print [=[Sed etiam a suspendisse. [[ Aliquam nulla erat risus. ]=]
    print [[Sed etiam a suspendisse. Aliquam nulla erat risus. ]]
