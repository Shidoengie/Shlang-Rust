# Shlang
Shlang originally started as dumbSyth
Syth is my dream programing language but as i never had made a language before i couldnt make good design decisions, so i decided to actually make a language.

Originally Shlang was to have no controlflow, no functions, no variables nothing like that, the goal was to print out something in the screen,
therefore only 2 keywords existed `print` and `input`, as more work was put on the interpreter originally made in c++, more features were added and it quickly went much further then the original goal, somehow print and input stayed as keywords even when moving to rust, only removed fairly recently.

# Keywords
- `and`
- `break`
- `continue`
- `do`
- `else`
- `false`
- `func`
- `if`
- `loop`
- `not`
- `null`
- `or`
- `return`
- `true`
- `var`
- `while`
- `struct`
- `new`
# The language
Shlangs has no statements as so everything is an expression



## Variables
Variables are a way of storing values in named containers, can be declared using `var`, as shlang is dynamic it doesnt have type annotation.
```swift
var a = 1;
# or
var a;
```
By default a variable will have `null`
## Numbers
In shlang all numbers are floats, meaning that all numbers can have decimal points
```swift
9.0
9
104.32
```

## Strings
Strings are a way of representing text
```swift
"heyo!"
"hey hey 
hey!"
```
## Booleans
Booleans can be either `true` or `false`, booleans are used to build complex logic.
### Boolean logic
In shlang you can use `and`,`or`,`not` or `&`,`|`,`!` for boolean algebra.

```swift
(true and false) or (true & false) and (not true) | (!true)
```
## Nothing
Shlang has 2 types of nothing `void` and `null`, `null` can be assigned to variables while `void` cant.
## Custom data types
User definable data types can be created using the `struct` block
```swift
struct foo {
    var msg;
    func hello(){
        println(hi);
    }
}
```
Each struct field can be accessed using `.`
```swift
foo.msg = "hello world!";
foo.hello();
```
To construct the struct ie: making a new instance of that struct you use the `new` keyword
```swift
var new_foo = new foo{msg:1};
```
Note modifying the instance with `.` will not modify its definiton
```swift
foo.msg = "hey";
var new_foo = new foo{};
# msg is stil null
```
To construct a struct inside of itself you pass in `Self` in the struct name
```swift
struct foo {
    var msg;
    func hello(){
        println(hi);
    }
    func make(_msg){
        return new Self{msg:_msg};
    }
}
```
## Comments
Coments are lines of code that are used for documentation, and are completly ignored.
- Single line comments
```py
# weeee
```
- Mutiline line comments
```
#* 
weeee
*#
```
### Compound Assignment
In shlang there many ways to assign a variable the most common being `a = 1;`, but there exist shorthands to further facilitate these tasks.
- `+=` assigns the added value of the variable and the value of the expression.
- `-=` assigns the subtracted value of the variable and value of the expression.
- `*=` assigns the multiplies value of the variable and value of the expression.
- `/=` assigns the divided value of the variable and value of the expression.
## Blocks
A block is a list of expression that are envolved by `{}`
```swift
do{
#this expression contains a block
}
```
## Functions

Shlang has first class functions, meaning you can treat them as any other value.
Creating a function object can be done with `func`.
```swift
var foo = func(bar){
    return bar;
};
foo();
```
On functions the return value will be implicitely `null` if nothing is returned

Defining functions can be done by giving a name to `func`
```swift
func foo(bar){
    return bar;
}
foo();
```

## Results and returns
### Return
`return` is used to return values from functions.
```swift
func foo() {
    return 1;
}
```
appon a return all other code will be ignored
### Implicit returns/results
Results work by returning the last statement in a block and only if that last statement doesnt have a semicolon
```swift
var foo = do {
    1;
    2;
    3 # results in 3
};# foo now has 3 
```
```swift
var foo = do {
    1
    2
    3;
};
# foo is now null as no value was returned 
```
results can be used in variable assignment and binary ops
```swift
print("hi"+if true {"a"} else {"b"})
```
## If expressions
Ifs are expressions which their block will only be evaluated if a condition is true.
as if is an expression you can use it in assignment/declaration, turning it into a sort of ternary statement
```swift
var inp = input();
var stuff = if inp == "hey"{"heyo!"} else {":("};
```
you can chain ifs with `else if`
```swift
var inp = input();
var stuff = if inp == "hey"{
    "heyo!"
} 
else if inp == "ahoy"{
    "arggggggg"
}
else{
    ":("
};
```


## Do expressions
Do blocks are expressions with blocks that immidately execute, think of like this a `if true`
```swift
var num = do {
    var inserted = input();
    str_to_num(inserted)
};
```
## Loops
Loops are expressions that will continually evaluate a block
there are 2 types of loops, `while` loops and `loop`
### Break and Continue

`break` and `continue` allow to control the flow of execution of a loop
- `break` stops the loop
- `continue` continues to the next evaluation of the block ignoring all other code

Note using results or returns will have the same effect as break
### Loop
This loop will run indefinitely. 
```rs
loop {
    print("hi");
}
```
### While
This loop will run while a condition is true.
```swift
var a = 0;
while a < 10 {
    a += 1;
    print(a);
}
```
# Language Defaults

## Functions
- `println()` takes in n arguments and logs them to the console with a space on each argument, and flushes with a newline
```swift
println("hey","ho");
println("ahoy");
#*
hey ho
ahoy
*#
```
- `print()` takes in n arguments and logs them to the console with a space on each argument, and doesnt print a newline
```swift
print("hey","ho");
print("ahoy");
# hey hoahoy
```
- `input()` optionally takes in a prompt and retrieves user input as a string
```swift
var a = input("insert 2:");
var b = input();
# hey hoahoy
```
- `parse_num()` parses numbers from strings
- `to_str(val)` converts a value to a string
- `typeof(val)` returns the type of a value as a string
```swift

typeof(1)           # "num"
typeof(true)        # "bool"
typeof("s")         # "str"
typeof(1)           # "num"
typeof(null)        # "null"
typeof(println)     # "func"
typeof(var a = 1)   # "void"
```
## String methods
- `.char_at(index)` returns a character from a specified index returning `null` if the index is out of bounds
```swift
"abc".char_at(1); # "b"
"a".char_at(9); # null
```
- `.substr(start,length)` returns a slice of the string that starts at the start index and towards a length, returning null if either value is invalid
```swift
"hello world".substr(0,4); # "hello"
"a".substr(1,4); # null
```
- `.parse_num()` parses a number from a value returning null if the number is invalid
```swift
"1_000.1".parse_num(); # 1000.1
"abc".parse_num() # null
```
## Number methods
- `.to_string()` converts a number to a string