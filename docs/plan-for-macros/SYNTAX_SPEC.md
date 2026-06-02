# Fun Language Syntax Specification

## Design Principles

- Ruby/Elixir surface feel, ML semantics.
- Expression-oriented.
- Lexical scoping, curried functions, algebraic data types, pattern matching, effects/handlers.
- Hygienic macros via enforestation — extensible forms are recognized by first-token dispatch.
- Honu-style enforestation: reader flattens delimiters, enforest consumes one form per call.
- Indentation is not significant. Newlines are the primary expression/statement separators. Semicolons (`;`) are supported for inline use but not recommended.

---

## Lexical Grammar

```
hnumberi     ::= [0-9]+
hstringi     ::= " ... "
hchari       ::= ' ... '              (* with escape sequences *)
hidentifieri ::= [a-zA-Z_][a-zA-Z0-9_]*[?!]?
               | operator-char+
hcommai      ::= ,
```

Delimiters recognized by the reader:

```
hlparensi    ::= (
hrparensi    ::= )
hlbracki     ::= [
hrbracki     ::= ]
hlbracei     ::= {
hrbracei     ::= }
hbar         ::= |
hequal       ::= =
hcolon       ::= :
hthinarrow   ::= ->
```

Keywords (not reserved; can be shadowed):

```
fn  do  end  if  else  match  with  effect  perform  resume
type  module  struct  impl  trait  pub  import  open  macro
self  Self  ref  deref  can  <-  _  true  false  Unit
```

Built-in type keywords:

```
Type  I64  Bool  Unit  Char  String  Absurd  Ref  EffectRow
```

Comments:

```
hcommenti    ::= '#' hnon-newlinei*
               | '#|' hany-char* '|#'
               | '#_' htermi          (* reader skips next complete term *)
```

Line continuation: `\` at end of line suppresses newline.

Literals:

```
hliterali     ::= hnumberi | hstringi | hchari | 'true' | 'false' | '()'
```

`()` is the Unit value and Unit pattern. `Unit` is the Unit type.

---

## Reader Terms

```
htermi       ::= hnumberi | hstringi | hchari | hidentifieri
               | hcommai | hthinarrowi | hbar
               | ';'
               | '(' htermi* ')'    (* parenthesized group *)
               | '[' htermi* ']'    (* bracket group *)
               | '{' htermi* '}'    (* brace group *)
               | hkeywordi
```

Comment terms are consumed by the reader and not passed to enforestation.

---

## Top-Level Module

A source file is a module body. The file is a sequence of top-level declarations/expressions
separated by newlines (or `;`). Top-level bindings with `pub` are module-exports.

```
hmodule-bodyi ::= htop-stmti*
htop-stmti    ::= hbindingi | hexpressioni
```

---

## Bindings

Binding modifiers (`pub`, `rec`) appear in fixed order: `pub` before `rec`.

```
hbindingi    ::= (pub)? 'rec'? hnamei ':' htypei '=' hexpressioni    (* annotated assignment *)
               | (pub)? 'rec'? hnamei '=' hexpressioni               (* simple assignment *)
               | (pub)? 'rec'? 'fn' hnamei hfn-parami* (':' htypei)? hfn-bodyi   (* fn sugar *)
               | (pub)? 'type' htype-namei '(' htype-parami (',' htype-parami)* ')' '='
                    hctori ('|' hctori)*                              (* ADT declaration *)
               | (pub)? 'type' htype-namei '=' hctori ('|' hctori)*  (* ADT, no params *)
               | (pub)? 'effect' htype-namei ('(' htype-parami (',' htype-parami)* ')')? '=' hsignature-modulei
               | (pub)? 'trait' htype-namei '(' htype-parami ')' '=' hsignature-modulei
               | (pub)? 'impl' htype-pathi '(' htypei ')' '=' 'module' hbindingi* 'end'
               | (pub)? 'module' hnamei 'do' hbindingi* 'end'
               | (pub)? 'struct' hnamei 'do' hstruct-fieldi* hbindingi* 'end'
               | (pub)? 'macro' hnamei hfn-parami* hfn-bodyi
               | (pub)? 'import' hstringi     (* returns module value *)
```

Binding keywords are declaration forms only. They appear at the top level, inside `do ... end`, and inside module/struct bodies. There is no `KEYWORD <binding> in <expression>` expression syntax; write a `do` block instead:

```fun
do
  type Option A = Some A | None
  Option(I64)
end
```

hsignature-modulei ::= 'module' hsignature-module-fieldi+ 'end'
                     | 'sig' hsignature-sig-fieldi+ 'end'
hsignature-module-fieldi ::= hnamei '=' htypei          (* must be a Pi/function type *)
hsignature-sig-fieldi    ::= hnamei ':' htypei          (* sugar for module name = Type *)

Effects with no parameters omit the parameter list: `effect Exc = module ... end`. Parameterized effects use a non-empty parenthesized parameter list: `effect State(S) = module ... end`. Empty parameter lists such as `effect Exc() = ...` are invalid.

Traits always take exactly one type parameter: `trait Eq(A) = module ... end`.

`sig ... end` is accepted as syntax sugar for a module whose fields are type values. `sig x : T end` means the same effect/trait signature field as `module x = T end`:

```fun
effect State(S) = module
  get = Unit -> S
  put = S -> Unit
end

effect State(S) = sig
  get : Unit -> S
  put : S -> Unit
end

trait Eq(A) = sig
  eq : A -> A -> Bool
end
```

### Function params

```
hfn-parami   ::= '(' hparami (',' hparami)* ')'     (* explicit params *)
               | '(' ')'                            (* Unit param *)
               | '[' hparami (',' hparami)* ']'    (* implicit params *)
hparami      ::= hnamei   (* untyped *)
               | hnamei ':' htypei
```

All implicit params must come before any explicit params.

```
# valid
fn [A : Type] (x : A) do ... end

# invalid — implicit param in the middle
fn (x : A) [B : Type] do ... end
```

### Function bodies

```
hfn-bodyi    ::= 'do' hexpressioni* 'end'      (* block body *)
               | '->' hexpressioni              (* one-liner arrow *)
```

### Recursive bindings

`rec` makes the bound name visible in its own right-hand side.
`fn f(x) do ... end` without `rec` is non-recursive (cannot reference `f` in the body).

```
# recursive function — explicit rec
rec fn f(x) do
  if x == 0 do 1
  else x * f(x - 1)
  end
end

# non-recursive function — fn sugar without rec
fn f(x) do
  x + 1    # f not visible here
end

# recursive non-function
rec x = x + 1    # infinite loop at definition time, but valid

# mutually recursive
rec f = fn(x) do ... g(x) ... end
rec g = fn(x) do ... f(x) ... end
```

### Module type (signature) sugar

`sig x : I64 end` is sugar for `module pub x = I64 end`. Only declarations, no computed bindings.

```
sig x : I64; y : Bool end         # type: module pub x = I64; pub y = Bool end
module pub x : I64 = 1 end        # has type: sig x : I64 end

# module type used as type annotation
fn (m : sig x : I64 end) : I64 do m.x end
```

### Struct field declarations

```
hstruct-fieldi ::= hnamei ':' htypei ';'
```

```
hstruct-fieldi ::= hnamei ':' htypei ';'
```

---

## Expressions

```
hexpressioni ::= hliterali
               | hidentifieri                          (* variable *)
               | 'self'                                (* receiver reference *)
               | 'Self'                                (* struct type reference *)
               | 'fn' hfn-parami* (':' htypei)? hfn-bodyi   (* anonymous function *)
               | 'if' hexpressioni 'do' hexpressioni* 'else' hexpressioni* 'end'
               | 'match' hexpressioni 'do' ('|')? hmatch-clausei ('|' hmatch-clausei)* 'end'
                | 'do' hexpressioni+ 'end'              (* block — sequence, value is last expr *)

               | 'ref' '(' hexpressioni ')'             (* reference creation; function-like exception *)
               | 'deref' '(' hexpressioni ')'           (* reference read; function-like exception *)
               | '()'                                  (* Unit value *)
               | '(' hexpressioni (',' hexpressioni)* ')'   (* tuple value *)
               | '(' hexpressioni ')'                  (* parenthesized expression *)
               | '{' hrecord-entryi (',' hrecord-entryi)* '}'   (* record constructor *)
               | 'import' hstringi                     (* returns module value *)
                | 'open' hexpressioni                   (* opens module scope for subsequent exprs *)
               | 'module' hbindingi* 'end'                (* anonymous module expression *)
               | 'struct' hstruct-fieldi* hbindingi* 'end' (* anonymous struct expression *)
               | 'perform' htype-pathi '.' hop-namei hexpressioni  (* effect perform *)
                 | 'resume' hexpressioni              (* resume continuation; use resume () for Unit *)
               | hexpressioni ':' htypei               (* type annotation *)
               | hexpressioni '.' hnamei               (* field access *)
                | hexpressioni '(' hcall-argsi ')'      (* explicit application *)
                | hexpressioni '(' ')'                (* application to Unit *)
                | hexpressioni '[' hcall-argsi ']'      (* implicit application *)
                 | 'rec'? hexpressioni '<-' hexpressioni          (* ref assignment — rec for cyclic structures *)
               | hunary-operatori hexpressioni          (* unary operator *)
               | hexpressioni hbinary-operatori hexpressioni  (* binary operator *)
               | hexpressioni '@' '(' hexpressioni ')'  (* macro invocation — provisional *)
```

Removed legacy expression forms:
```
let x: T = v in b       # use: do x : T = v; b end
type T A = ... in b     # use: do type T A = ...; b end
effect E = sig ... end in b     # use: do effect E = sig ... end; b end
trait T(A) = sig ... end in b    # use: do trait T(A) = sig ... end; b end
impl T(A) = module ... end in b # use: do impl T(A) = module ... end; b end
open M in b             # use: do open M; b end
import "m" in b         # use: do M = import "m"; b end
```

`ref(expr)` and `deref(expr)` are intentionally function-like built-ins. `resume` and `perform` are keyword prefix forms, not keyword calls.

---

## Patterns (for match clauses)

```
hmatch-clausei ::= hpati '->' hexpressioni*
                 | 'effect' htype-pathi '.' hop-namei hpati '->' hexpressioni*

hpati        ::= '_'                                    (* wildcard *)
               | hnamei                                 (* binder *)
               | hliterali                              (* literal *)
               | '()'                                   (* Unit value pattern *)
               | hctori                                 (* constructor pattern *)
               | '(' hpati (',' hpati)* ')'             (* tuple *)
               | '{' hpat-fieldi (',' hpat-fieldi)* '}' (* record pattern *)
               | hpati '|' hpati                        (* or-pattern *)
               | htype-pathi '.' hctori                 (* qualified constructor *)

hpat-fieldi  ::= hnamei '=' hpati                       (* named field match *)
               | hnamei                                 (* field shorthand: name = name *)

hctori       ::= htype-pathi '.' hnamei                 (* qualified *)
               | hnamei                                 (* unqualified *)
               | hnamei '(' hpati (',' hpati)* ')'      (* with tuple payload *)
               | hnamei '(' htypei (',' htypei)* ')'    (* with comma-separated type arguments *)
```

Branch-leading `|` is recommended for readability. A `|` before a branch arrow remains part of the pattern, so unparenthesized or-patterns are accepted:

```fun
match value do
| 0 | 1 -> 42
| _ -> 0
end
```
```
# Example: recursive ADT with comma-separated payloads
type List(a) = Cons(a, List(a)) | Nil
```

---

## Types

```
htypei       ::= htype-atomi
               | htypei '->' htypei                     (* pure arrow — explicit Pi *)
               | '(' hnamei ':' htypei ')' '->' htypei  (* named explicit Pi *)
               | '[' hparami (',' hparami)* ']' '->' htypei  (* implicit Pi *)
               | htypei '*' htypei                      (* tuple/product type, binds tighter than -> *)
               | htypei 'can' heffect-rowi               (* effectful arrow *)

htype-atomi  ::= hidentifieri                            (* nominal type *)
               | hidentifieri '(' htypei (',' htypei)* ')'   (* applied nominal *)
               | 'Type'
               | 'I64' | 'Bool' | 'Unit' | 'Char' | 'String' | 'Absurd'
               | 'Ref' '(' htypei ')'                   (* reference type *)
               | 'EffectRow'
               | '(' htypei ')'                         (* parenthesized *)
               | htype-pathi '.' hnamei                 (* qualified type *)

heffect-rowi ::= htypei                                  (* single closed effect *)
               | '{' '}'                                 (* explicitly pure row *)
               | '{' htypei (',' htypei)* '}'            (* closed effect row *)
               | '{' htypei (',' htypei)* '|' hnamei '}' (* open row with prefix effects *)
               | '{' '|' hnamei '}'                      (* tail-only open row *)
```

Implicit Pi binders use brackets, not braces:

```fun
[r : EffectRow] -> Unit -> I64 can {IO, Log | r}
```

Named explicit Pi binders use parentheses:

```fun
(r : EffectRow) -> Unit -> I64 can {| r}
```

---

## Operators

Built-in binary operators (inherited from current system):

```
+  -  *  /  %  ==  !=  <  >  <=  >=
eq_i64  neq_i64  eq_bool  neq_bool  eq_char  neq_char
eq_unit  neq_unit  eq_string  neq_string
```

Built-in unary operators:

```
not
```

All operators are reserved names in the prelude; they work via the enforestation operator table.

---

## Curried Application

The semantics of `f(a, b, c)` is `f(a)(b)(c)`, where `f(a)` returns a function.

```fun
# f(a, b, c)   =  f(a)(b)(c)
# f(a)(b, c)   =  (f(a))(b)(c)     -- same
# f(a)(b)(c)   ok
# f(a)         ok (partial application)
```

Combined with implicit args:

```fun
# f[A, B](x, y)  =  f[A](x)(y)   -- after implicit application f[A] produces a function
```

---

## Zero-Argument Function Sugar

`fn f() do ... end` is sugar for a function with a single Unit parameter.
`f()` is sugar for `f(())`. Every function call passes at least one argument.

```
fn f() do body end                 # desugars to: fn f(x) do body end
                                   # type: Unit -> ...
f()                                # desugars to: f(())
```

---

## Important Notes

- **Newlines and Sequencing**: Every binding/expression is terminated by a newline or semicolon within its containing block. Use `\` to continue a line.

- **Do-blocks**: `do ... end` sequences evaluate expressions in order, yielding the value of the last expression. The type of the block is the type of its last expression. Bindings inside a `do` block are visible from their point of definition to the end of the block. Empty `do end` is a parse error — use `()` for a unit value.

- **Top-level pub**: `pub x = 1` at the top level of a file is equivalent to an exported module member. `pub` is valid only inside `module`, `struct`, or at the top level.

- **Module vs Struct**: `module` is for namespace and access-control (public/private members, signatures via `sig` sugar). `struct` is for data records with named fields, optional methods, and `self`/`Self` references. A struct value does not double as a module signature.

- **Signatures**: `sig x : I64 end` desugars to `module pub x = I64 end`. Only field-name : type pairs; no computed bindings.

- **Braces `{}`**: Used exclusively for record construction and record patterns. Not used for blocks (use `do/end`), not used for implicit params (use `[]`), not used for sets or anything else.

- **Implicit-param interleaving** happens naturally through curried Pi types. A function can return another function that introduces its own implicit params:

```fun
# fn's own params: all implicit [] before explicit ()
fn [A : Type] (x : A) : [B : Type] -> (fn (f : A -> B) -> f(x)) do
  fn (f) -> f(x)
end
```

  The restriction applies only to the immediate parameter list of a single `fn` form. The return type `[B : Type] -> ...` is a valid Pi type anywhere, so the eventual call site can use `g[A](x)[B](f)`.

- **Open**: `open M` brings `M`'s public members into scope for all subsequent expressions in the enclosing `do ... end` block. There is no `open M in expr` form.

- **Import**: `import "path"` returns a module value. Bind it in a `do` block when later expressions need it: `do M = import "path"; M.x end`. There is no `import "path" in expr` form.

- **Module type as annotation**: Module types (`sig ... end`) can appear anywhere a type annotation is expected, e.g. `fn (m : sig x : I64 end) : I64 do m.x end`.
