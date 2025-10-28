# Julia test example file

using Test

# Regular helper function (not a test)
function add(a, b)
    return a + b
end

# Another helper function
function multiply(x, y)
    return x * y
end

# Helper function
function factorial_calc(n)
    if n <= 0
        return 1
    end
    return n * factorial_calc(n - 1)
end

# Test with @test macro
@test add(2, 3) == 5
@test multiply(4, 5) == 20

# Regular struct (not a test)
mutable struct Calculator
    value::Int
    
    Calculator(initial::Int = 0) = new(initial)
end

# Helper functions for Calculator
function add!(calc::Calculator, n::Int)
    calc.value += n
    return calc
end

function get_value(calc::Calculator)
    return calc.value
end

function reset!(calc::Calculator)
    calc.value = 0
end

# Test set with @testset
@testset "Math operations" begin
    @test add(1, 1) == 2
    @test add(10, 20) == 30
    
    @test multiply(3, 4) == 12
    @test multiply(0, 5) == 0
end

# Regular helper function (not a test)
function process_data(arr)
    return filter(x -> x > 0, arr) .* 2
end

# Nested test sets
@testset "Factorial tests" begin
    @testset "Basic calculations" begin
        @test factorial_calc(5) == 120
        @test factorial_calc(0) == 1
        @test factorial_calc(1) == 1
    end
    
    @testset "Edge cases" begin
        @test factorial_calc(3) == 6
        @test factorial_calc(4) == 24
    end
end

# Calculator tests
@testset "Calculator" begin
    @test begin
        calc = Calculator()
        get_value(calc) == 0
    end
    
    @test begin
        calc = Calculator(10)
        get_value(calc) == 10
    end
    
    @testset "Addition operations" begin
        @test begin
            calc = Calculator()
            add!(calc, 5)
            add!(calc, 10)
            get_value(calc) == 15
        end
        
        @test begin
            calc = Calculator()
            add!(calc, -5)
            get_value(calc) == -5
        end
    end
    
    @testset "Reset functionality" begin
        @test begin
            calc = Calculator()
            add!(calc, 100)
            reset!(calc)
            get_value(calc) == 0
        end
    end
end

# Helper function
function format_number(n::Float64)
    return @sprintf("%.2f", n)
end

# More regular code
struct Person
    name::String
    age::Int
end

function create_person(name::String, age::Int)
    return Person(name, age)
end

# Helper function at the end
function reverse_string(s::String)
    return reverse(s)
end