-- Test file for Lua semantic test filtering (Busted, Lust, LuaUnit)

local M = {}

-- Production function
function M.calculate_prod(a, b)
    local cgrep_prod_calc = a + b
    if cgrep_prod_calc > 10 then
        for i = 1, 3 do
            cgrep_prod_calc = cgrep_prod_calc + i
        end
    end
    return cgrep_prod_calc
end

-- False positive guard: function starting with "test" as part of a longer word ("testify")
function testify(x)
    local cgrep_prod_testify = x * 2
    return cgrep_prod_testify
end

-- False positive guard: production setup function (not Busted setup(function() ... end))
function setup(config)
    local cgrep_prod_setup_fn = config or {}
    return cgrep_prod_setup_fn
end

-- False positive guard: method call .test or :test on an object without callback
local regex = {}
function regex:test(pattern)
    local cgrep_prod_regex_test = pattern
    return cgrep_prod_regex_test ~= nil
end

local cgrep_prod_call = regex:test("abc")

-- 1. LuaUnit top-level and local test functions
function test_addition()
    local cgrep_test_luaunit_top = M.calculate_prod(2, 3)
    if cgrep_test_luaunit_top > 0 then
        for i = 1, 2 do
            cgrep_test_luaunit_top = cgrep_test_luaunit_top + i
        end
    end
    assert(cgrep_test_luaunit_top > 0)
end

local function testSubtraction()
    local cgrep_test_luaunit_camel = 10 - 4
    repeat
        cgrep_test_luaunit_camel = cgrep_test_luaunit_camel - 1
    until cgrep_test_luaunit_camel <= 5
    assert(cgrep_test_luaunit_camel == 5)
end

-- 2. LuaUnit table-based test suite
TestCalculator = {}

function TestCalculator:setUp()
    self.cgrep_test_luaunit_setup = 42
end

function TestCalculator:test_multiply()
    local cgrep_test_luaunit_method = self.cgrep_test_luaunit_setup * 2
    assert(cgrep_test_luaunit_method == 84)
end

function TestCalculator:tearDown()
    self.cgrep_test_luaunit_teardown = nil
end

-- 3. Busted style describe / context / it / setup / before_each / pending
describe("Calculator suite", function()
    local cgrep_test_busted_describe = 100

    setup(function()
        local cgrep_test_busted_setup = true
    end)

    before_each(function()
        local cgrep_test_busted_before = 1
    end)

    context("when adding numbers", function()
        it("returns the sum", function()
            local cgrep_test_busted_it = M.calculate_prod(1, 2)
            assert.are.equal(3, cgrep_test_busted_it)
        end)

        pending("todo test", function()
            local cgrep_test_busted_pending = 0
        end)
    end)

    after_each(function()
        local cgrep_test_busted_after = 0
    end)
end)

-- Production variable at end of file
local cgrep_prod_end = M.calculate_prod(5, 5)
return M
