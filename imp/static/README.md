# Imperative language with static scoping

Implement the small step semantics of an imperative language which features statically scoped blocks of declarations and functions, with the following abstract syntax:

```ocaml
type ide = string
  
type expr =
  | True
  | False
  | Var of ide
  | Const of int
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr
  | Call of ide * expr     
  | CallExec of cmd * expr (* Runtime only: c is the cmd being reduced, e is the return expr *)
  | CallRet of expr        (* Runtime only: e is the return expr *)
              
and cmd =
  | Skip
  | Assign of string * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | While of expr * cmd
  | Expr of expr            (* Expression statement *)
  | Decl of decl * cmd
  | Block of cmd

and decl =
  | EmptyDecl
  | IntVar of ide 
  | Fun of ide * ide * cmd * expr
  | DSeq of decl * decl

type prog = Prog of decl * cmd

```

The syntax of this language combines that of [blocks](../blocks/lib_bart/parser.mly) and [fun](../fun/lib_bart/parser.mly) and introduces the *expression statement*, which is an expression followed by a semicolon. The execution of an expression statement causes the associated expression to be evaluated along with the desired side effects.

## The static environment

The *scoping rule* of a language determines the part of a program where a name binding is valid. It allows identically named variables to refer to different values when used in different contexts. This is crucial for resolving the names used within functions, whose execution is typically deferred to a different part of the program (a different context) to that they are *declared* in.

In a **statically scoped** language, the scope of a function - the name bindings that are valid during the execution of its instance - is uniquely determined at the time of lexical analysis (hence the name static) by the position where the function declaration appears in the program.

All non local references of a function definition must be known at the time of its declaration, as opposed to *dynamic scoping*, where they may be found by means of a search in the runtime call stack. 

Take the following code for example:
```
fun f(n) { 
  return n + x
}
int x;

x := f(n);
```
Its computation diverges because the variable `x` is not in the scope of the function definition. Thanks to static scoping, this kind of error can be detected at compile time.

## Closures

In order to implement static scoping, the runtime environment at the point of a function declaration must be saved somewhere and be efficiently accessed when the function is instantiated for later execution.

This can be accomplished by associating a **closure** to each function identifier in the environment. A closure is a data structure that carries the code of the function and the environment recorded until the point of its declaration, i.e. its lexical context. 
 
Take the following valid code for example:
```
int x;
fun f(n) { return x+n }
int y;
fun g(m) { y := f(m); return y }

x:=1;
g(2);
```
We can deduce the names known to `f` and `g` at execution time without the need to run the code, by considering the lexical structure of the program: besides formal parameters, only `x` will be visible to `f`, while `x`, `y` and `f` will be visible to `g`. This is independent of the fact that `f` is called within `g`. 

Closures are encoded in the ```IFun``` constructor of the ```envval``` type, by including an ```env``` component. Since the two entities now depend on each other, a mutually recursive definition is necessary:
```ocaml
type env = ide -> envval
and envval = IVar of loc | IFun of ide * env * cmd * expr
```

## Small-step semantics

Implement the small-step semantics under the static scoping rule. You will need to:
* Modify the semantics of declarations to create a closure when processing a function declaration
* Modify the semantics of the call expression to retrieve a function's associated closure and push it onto the environment stack

Take this piece of code for example:
```
int n;
fun f(x) { return n+x }
fun g(n) { return f(n) }

n:=1;
g(10);
```
The output of ```trace``` for it should look like this:
```
<n:=1; g(10);, [<fun>/f,0/n,<fun>/g], [], 1>
 -> <g(10);, [<fun>/f,0/n,<fun>/g], [1/0], 1>
 -> <exec{skip; ret f(n)};, [<fun>/f,1/n,<fun>/g], [1/0,10/1], 2>
 -> <{ret f(n)};, [<fun>/f,1/n,<fun>/g], [1/0,10/1], 2>
 -> <{ret f(10)};, [<fun>/f,1/n,<fun>/g], [1/0,10/1], 2>
 -> <{ret exec{skip; ret n+x}};, [2/x,<fun>/f,0/n], [1/0,10/1,10/2], 3>
 -> <{ret {ret n+x}};, [2/x,<fun>/f,0/n], [1/0,10/1,10/2], 3>
 -> <{ret {ret 1+x}};, [2/x,<fun>/f,0/n], [1/0,10/1,10/2], 3>
 -> <{ret {ret 1+10}};, [2/x,<fun>/f,0/n], [1/0,10/1,10/2], 3>
 -> <{ret {ret 11}};, [2/x,<fun>/f,0/n], [1/0,10/1,10/2], 3>
 -> <{ret 11};, [<fun>/f,1/n,<fun>/g], [1/0,10/1,10/2], 3>
 -> <11;, [<fun>/f,0/n,<fun>/g], [1/0,10/1,10/2], 3>
 -> [<fun>/f,0/n,<fun>/g], [1/0,10/1,10/2], 3
```

### Note on recursion

As for the implementation of recursive functions, the static environment of a function must include its own binding. This requires a simple ```bind``` operation after retrieving the static environment from the closure.
