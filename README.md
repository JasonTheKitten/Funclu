# Funclu

## Functions that return functions the return functions

Funclu is an experimental library that adds a ton of functions to Lua.
The functions return other functions, which allows you to pass parameters to them
without needing any commas.
These functions can be composed to allow you to create entire programs without ever
even needing to write a "while" loop.

Note: Funclu is in a beta state. You will encounter bugs, and new versions will break compatibility.

Here's a basic "Hello World" program using a Funclu function:

```lua
require("funclu").enableDebug().install(_ENV, { ... })

(defn "main" (prints "Hello, world!"))
```

Because Funclu is designed to be able to work in a standard Lua environment, you can run this code on ComputerCraft, OpenComputers, and even PUC Lua.

!(CC Hello, World)[images/cc_hello.png]
!(OC Hello, World)[images/oc_hello.png]
!(PUC Hello, World)[images/puc_hello.png]

You can, of course, write more complex and interactive programs with Funclu:

```lua
require("funclu").enableDebug().install(_ENV, { ... })

(defn "main" (block
  (prints "Hello, what is your name?")
  (bind "name" (getLine))
  (cond (eq "exit" (a.name)) (void) (block
    (prints (strc "Hello, " (a.name) "!"))
    (prints "Oh no, I already forgot your name!")
    (f.main)))
))
```

!(Interactive Hello)[images/interactive_hello.png]

Funclu includes a Sequence API, which allows you to work with lists of numbers:

```lua
require("funclu").enableDebug().install(_ENV, { ... })

(defn "fib" (args "count")
  (cond (lt (a.count) (2))
    (a.count)
    (add
      (f.fib (sub (a.count) (1)))
      (f.fib (sub (a.count) (2))))))

(defn "fibSeq" (map (f.fib)))

(defn "main" (block
  (prints "How many numbers of the fibonacci sequence do you want?")
  (bind "n" (getLine))
  (bindval "s" (take (num (a.n)) (f.fibSeq (iterator))))
  (prints "Computing values. This may take a while...")
  (prints "The first" (a.n) "numbers of the fibonacci sequence are" (applySeq (a.s)))
))
```

!(Numerical Sequence Example)[images/fib_example.png]

Notice that, even though the "Computing values" line comes after fibSeq is bound, it still runs before the computation.
This is because Funclu supports lazy evaluation.

Funclu also includes built-in constructs for handling errors:

```lua
require("funclu").enableDebug().install(_ENV, { ... })

(using "builtin")
(defn "main" (block
  (prints "What would you like to divide 5 by?")
  (bind "a" (getLine))
  (bind "n" (try (asIO (div (5) (num (a.a))))))
  (match (a.n)
    (t.left (t.err (a.err) (a.callsites))) (prints "Division Error:" (a.err) "at" (a.callsites))
    (t.left (t.failure (a.err))) (prints "Division Error:" (a.err))
    (t.right (a.val)) (prints "Value:" (a.val)))
  (f.main)
))
```

In the above example, the result of division will be shown if the division is successful.
But if the user attempts to enter a non-numerical value, the error will be caught and displayed to the user.

!(Error Handling Example)[images/error_example.png]

Although Funclu is designed to be portable across Lua environments, it also has constructs that
allow for doing things specific to a given environment. Furthermore, it also has constructs that
allow dynamically loading a library at runtime, so that the same program can intergrate with
multiple environments.

The code for this example is quite long, so you can find it in the `examples/oc_cc_textcolor` folder.

The provided example shows using environment-specific functions to set the color of text in CC and OC.

!(CC Environment-Specific Example)[images/cc_envspec.png]

!(OC Environment-Specific Example)[images/oc_envspec.png]

However, the constructs provided for doing this are extremely clunky and bug-prone at this time,
and so will likely be refined in future updates.

Funclu also comes with a small builtin library that you can use by prefixing function names or `(using "builtin)`.
It defines a variety of types and traits that may be useful or are used internally.

It provides the following types:

* `just a` - A type that represents the presence of the value `a`.
* `empty` - A type that represents the absence of a value.
* `left a` - Represents one of two possibilities, the first of which is `a`.
* `right b` - Represents the other of two possibilities, which is `b`.
* `failure reason` - Represents a failure with a reason.
* `err reason callsites` - Represents an error with a reason and a list of predicted relevant callsites.

It provides the following traits:

* `alternative` - Represents a type that is one of two options. It has the method `|` used to choose between this and another alternative.
* `monad` - Represents a type that can be sequenced. It has the methods `>>=` (bind) and `>>` (then) for sequencing. Bind can pass an additional value.

It provides these functions:

* `>>=` - Run the `>>=` method of a given monad.
* `>>` - Run the `>>` method of a given monad.

Funclu programs should start with the line
```lua
require("funclu").enableDebug().install(_ENV, { ... })
```

Funclu modules should start with the line
```lua
return require("funclu").modules(_ENV)
```

Funclu provides many functions and values available through _ENV for your convenience.

* `void` - A value that represents nothing. Use this instead of `nil`, as `nil` will break Funclu.
* `wrap` - Prevent evaluation of a function during the next evaluation.
* `unwrap` - Evaluate a function so that a wrapped function can be evaluated.
* `only` - Primarily used to treat an argument like a function when it needs to be called with it's own arguments. (Calling arguments will need fixed in the future)
* `undefined` - Throws an error when evaluated. Useful for debugging.
* `errs` - Like `undefined`, but with a message.
* `f` - Prefix for custom functions. You can do `f.namespace.functionName` or `f"namespace.functionName"`.
* `defn` - Define a function, in the form `(defn "name" (args "myArg1") (body))`. Leave name as an empty string for lambdas. Args can be omitted.
* `a` - Prefix for arguments. You can do `a.myArg` or `a"myArg"`.
* `args` - Part of a function signature. See `defn`.
* `newtype` - Define a new type. Use `newtype "name" "myArg1"`.
* `eq2` - Compare two values for equality. Is "smarter" than `eq` and can handle custom type comparison.
* `member` - Get a named member from a type or table. Call like `(member "name" (a.myType))`.
* `match` - Match a custom type against a series of possible values. Can have a default condition. Use like `(match (a.myType) (t.myType (a.myArg)) ... (default))`.
You can do multiple comparisons. Put an extra value if you want a default. Can deconstruct custom types too - put a variable reference (eg `a.x`) in the pattern type.
* `any` - A value that represents any value (primarily meant for use with `match`).
* `_` - Alias for `any`.
* `t` - Prefix for types. You can do `t.myType "myArg1"` or `t"myType" "myArg1"`.
* `defmethod` - Define a method for a type. Use like `(defmethod "myMethod" (args "myArg1") (body))`. Should be inside a trait or instance definition. The body is optional.
* `deftrait` - Define a trait. Use like `(deftrait "myTrait" (methods))`. Methods should be a list of `defmethod` calls. You can also include `extends` calls.
* `instance` - Define an instance of a trait for a type. Use like `(instance "myTrait" (methods))`. Methods should be a list of `defmethod` calls.
If a method did not have a body when defined in the trait, then it must have a body here.
* `extends` - Extend a trait for a type. Use like `(extends "myTrait")`. Should be inside a trait definition.
* `method` - Run a given trait's method on a custom type that implements the trait. Use like `(method "myTrait" "myMethod" (a.myType) ...)`.
* `isInstance` - Check if a value is an instance of a type. Use like `(isInstance (a.myType) "myTrait")`.
* `loadmod` - Load a module from another file. Use like `(loadmod "myModule")`.
* `loadprovider` - Also loads a module, but associated with a custom name. The first call for a given module must have two arguments -
the first being the custom name, the second being the module name. Subsequent calls only need the custom name. Use like `(loadprovider "myCustomName" "myModule")`.
* `modulename` - If called with an argument, sets the name of the current module. Regardless, returns the name of the current module.
Primarily meant for standard libraries. Prefer `submodule` for other libraries.
* `submodule` - Define a submodule whose name is appended to the current module's name. Use like `(submodule "mySubmodule")`.
* `using` - If called with one argument, attempts to unprefix any available utilities with a name starting with that argument. Otherwise, renames the utilities to the given names.
Use like `(using "myPrefix")` or `(using "myPrefix" "myNewPrefix")`.
* `exportsf`, `exportst`, `exportstr` - Export a set of functions, types, or traits (respectively). Use like `(exportsf "myFunction1" "myFunction2")`.
* `nilToVoid` - Convert a value to void if it is nil, else retain the value. Useful for integrating with an external Lua environment, as `nil` will typically break Funclu.
* `luaf` - Allows using a Lua function within Funclu. Use like `(luaf (numArgs) (function))`. It isn't very configurable, so it is recommended that you use `luaf2` instead.
* `luaf2` - Allows using a Lua function within Funclu. Use like `(luaf (numArgs) (options) (function))`.
Options is a table with the marshallToLua, marshallFromLua, returnSeq, and hasSideEffects fields.
* `add`, `sub`, `mul`, `div`, `mod` - Basic arithmetic functions.
* `inc`, `dec` - Increment and decrement functions.
* `neg` - Negate a number.
* `eq`, `lt`, `gt`, `lte`, `gte`, `neq` - Comparison functions.
* `seq` - Create a sequence of numbers. Example: `(seq (1) (2) (3))`
* `toseq` - Convert a table to a sequence.
* `applySeq` - Treat each value of a sequence as if it were instead a parameter to a function.
* `and_`, `or_`, `not_`, `xor` - Logical functions. `and_` and `or_` are short-circuiting and return appropriate values from the input.
* `shl`, `shr`, `ashr`, `band`, `bor`, `bxor`, `bnot` - Bitwise functions.
* `cond` - Conditional function. A series of fallback conditions followed by actions. Can be followed by a default action. Use like `(cond (check1) (value1) ... (default))`.
* `with` - Bind a series of values to names. Use like `(with "myValue1" (computation) ... (func))`. You can the access the value as `(a.myValue1)`.
* `prints` - Print a series of values. Use like `(prints "Hello, world!")`.
* `map` - Map a function over a sequence. Use like `(map (f.myFunction) (a.mySequence))`. This transforms values.
* `filter` - Filter a sequence based on a predicate. Use like `(filter (f.myPredicate) (a.mySequence))`. This removes values.
* `foldl`, `foldr` - Fold a function over a sequence. Use like `(foldl (f.myFunction) (a.accumulator) (a.mySequence))`.
Left folds run the function from left-to-right, and right folds run the function from right-to-left. Folds basically "combine" many values of a sequence.
* `iterator` - By default, returns an infinite sequence (starting at one and increasing by one).
Optional parameters `start`, `stepSize`, and `numStep` alter the sequence, where start is the first value, stepSize is the difference between values, and numStep is the number of values.
* `range` - Create a sequence of numbers ranging from `start` to `end` and increasing by `stepSize`. Use like `(range (1) (10) (2))`.
* `cycle` - Create an infinite sequence that cycles through the given nunbers. Use like `(cycle (1) (2) (3))`.
* `take` - Create a subsequence of a sequence containing only the first n values of that sequence. Use like `(take (5) (a.mySequence))`.
* `head` - Get the first value of a sequence.
* `tail` - Skip `n` values of a sequence and return the rest.
* `nth` - Get the `n`th value of a sequence.
* `subseq` - Get a subsequence of a sequence. Use like `(subseq (1) (5) (a.mySequence))`.
* `splice` - Return a sequence that omits the values between `start` and `end` of the given sequence. Use like `(splice (1) (5) (a.mySequence))`.
* `concat` - Combine many sequences, such that the values of the first sequence are followed by the values of the second sequence, and so on.
* `count` - Count the number of values in a sequence.
* `upper` - Convert a string to uppercase.
* `lower` - Convert a string to lowercase.
* `strc` - Format and concatonate many values.
* `str` - Convert a value to a string.
* `seqstr` - Convert a sequence to a string
* `num` - Convert a value to a number.
* `tbl` - Convert a sequence to a table.
* `withKeys` - Clone a table but add a series of key-value pairs to it. Use like `(withKeys (a.myTable) "myKey1" (myValue1) ...)`.
* `newTbl` - Create a new table with a series of key-value pairs. Use like `(newTbl "myKey1" (myValue1) ...)`.
* `keys` - Given a table, get a sequence of its keys.
* `values` - Given a table, get a sequence of its values.
* `block`, `bind`, `bindval` - Use to write code that runs sequentially.
`block` accepts the series of functions to run.
`bind` takes a custom type holding another type and binds the inner value to a variable (eg `(bind "x" (t.builtin.just(5)))` will bind 5 to a.x).
`bindval` just binds a value to a variable (eg `(bindval "x" (5))` will bind 5 to a.x).
* `getArgs` - When bound using `bind`, gets a sequence of arguments passed to the program.
* `getLine` - When bound using `bind`, gets a line of input from the user.
* `asIO` - Convert a value to an IO value, which can be bound with `bind`.
* `try` - When bound using `bind` and passed an IO value, runs the associated action, and returns a `t.builtin.left(t.builtin.err)`
if it errors, a `t.builtin.left(value)` if the returned value is a `t.builtin.failure` or `t.builtin.err`, and a `t.builtin.right(value)` otherwise.
* `try2` - When bound using `bind` and passed an IO value, runs the associated action, and returns a `t.builtin.left(t.builtin.err)`
if it errors and a `t.builtin.right(value)` otherwise.