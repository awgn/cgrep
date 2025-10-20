# Comment: This sets the minimum required version of CMake.
# It is a best practice to specify a relatively modern version.
cmake_minimum_required(VERSION 3.10)

# Reserved keyword: 'project'. Defines the project name and optionally the languages.
# Identifier: 'MyApp' (The project name)
project(MyApp
    VERSION 1.0
    LANGUAGES CXX
)

# Reserved keyword: 'set'. Defines a variable.
# Identifier: 'CMAKE_CXX_STANDARD' (A built-in variable)
set(CMAKE_CXX_STANDARD 17) # String literal: '17'
set(CMAKE_CXX_STANDARD_REQUIRED True) # Keyword: 'True' (Boolean constant)

# Identifier: 'SOURCE_FILE'
# String literal: 'main.cpp'
set(SOURCE_FILE "main.cpp") 

# Reserved keyword: 'add_executable'. Creates an executable target.
# Identifier: 'MyAppExecutable' (The target name)
# Variable substitution: ${SOURCE_FILE}
add_executable(MyAppExecutable ${SOURCE_FILE}) 
