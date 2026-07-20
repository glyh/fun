# Binding as Sets of Scopes

Matthew Flatt

University of Utah, USA

mflatt@cs.utah.edu

> Converted from PDF.

## Abstract

Our new macro expander for Racket builds on a novel approach to hygiene. Instead of basing macro expansion on variable renamings that are mediated by expansion history, our new expander tracks binding through a set of scopes that an identifier acquires from both binding forms and macro expansions. The resulting model of macro expansion is simpler and more uniform than one based on renaming, and it is sufficiently compatible with Racket’s old expander to be practical.

**Categories and Subject Descriptors D.3.3 [Programming Languages]: Language Constructs and Features**

**Keywords Macros, hygiene, binding, scope**

## 1. Introduction: Lexical vs. Macro Scope

Racket supports a family of languages—including the main Racket language, Typed Racket, teaching languages, a documentation language, an Algol 60 implementation, and more—where the languages interoperate in natural and useful ways. Furthermore, programmers can create new languages and language extensions with the same level of support and interoperability as the predefined languages. Racket’s support for multiple languages and extensions is enabled by its macro system, which provides a representation of syntax fragments and constructs for manipulating and composing those fragments.

Racket’s representation of syntax builds on a long line of work on macros in Lisp and Scheme (Kohlbecker et al. 1986; Kohlbecker and Wand 1987; Clinger and Rees 1991; Dybvig et al. 1993). A result of that line of work is often referenced by the shorthand of hygienic macro expansion, meaning that macros can both introduce bindings and refer to binding from the macro’s definition context, and macro expansion will not trigger accidental name capture in the way that naive textual expansion would. Although Racket does not restrict language extension to forms that can be expressed as hygienic macros, a representation of syntax fragments that supports hygienic expansion is an essential ingredient to more general, composable language extensions.

Roughly, hygienic macro expansion is desirable for the same reason as lexical scope: both enable local reasoning about binding so that program fragments compose reliably. The analogy suggests specifying hygienic macro expansion as a kind of translation into lexical-scope machinery. In that view, variables must be renamed to match the mechanisms of lexical scope as macro expansion proceeds.

A specification of hygiene in terms of renaming accommodates simple binding forms well, but it becomes unwieldy for recursive definition contexts (Flatt et al. 2012, section 3.8), especially for contexts that allow a mixture of macro and non-macro definitions. The renaming approach is also difficult to implement compactly and efficiently in a macro system that supports “hygiene bending” operations, such as datum->syntax, because a history of renamings must be recorded for replay on an arbitrary symbol.

In a new macro expander for Racket, we discard the renaming approach and start with a generalized idea of macro scope, where lexical scope is just a special case of macro scope when applied to a language without macros. Roughly, every binding form and macro expansion creates a scope, and each fragment of syntax acquires a set of scopes that determines binding of identifiers within the fragment. In a language without macros, each scope set is identifiable by a single innermost scope. In a language with macros, identifiers acquire scope sets that overlap in more general ways.

Our experience is that this set-of-scopes model is simpler to use than the old macro expander, especially for macros that work with recursive-definition contexts or create unusual binding patterns. Along similar lines, the expander’s implementation is simpler than the old one based on renaming, and the implementation avoids bugs that were difficult to repair in the old expander. Finally, the new macro expander is able to provide more helpful debugging information when binding resolution fails. All of these benefits reflect the way that scope sets are precise enough for specification but abstract enough to allow high-level reasoning.

This change to the expander’s underlying model of binding can affect the meaning of existing Racket macros. A small amount of incompatibility seems acceptable and even desirable if it enables easier reasoning overall. Drastic incompatibilities would be suspect, however, both because the old expander has proven effective and because large changes to code base would be impractical. Consistent with those aims, purely pattern-based macros work with the new expander the same as with the old one, except for unusual macro patterns within a recursive definition context. More generally, our experiments indicate that the majority of existing Racket macros work unmodified, and other macros can be adapted with reasonable effort.

## 2. Background: Scope and Macros

An essential consequence of hygienic macro expansion is to enable macro definitions via patterns and templates—also known as macros by example (Kohlbecker and Wand 1987; Clinger and Rees 1991). Although pattern-based macros are limited in various ways, a treatment of binding that can accommodate patterns and templates is key to the overall expressiveness of a hygienic macro system, even for macros that are implemented with more general constructs.

> Publication note: Permission to make digital or hard copies of all or part of this work for personal or classroom use is granted without fee provided that copies are not made or distributed for profit or commercial advantage and that copies bear this notice and the full citation on the first page. Copyrights for components of this work owned by others than ACM must be honored. Abstracting with credit is permitted. To copy otherwise, or republish, to post on servers or to redistribute to lists, requires prior specific permission and/or a fee. Request permissions from Permissions@acm.org. Copyright is held by the owner/author(s). Publication rights licensed to ACM. POPL’16, January 20-22, 2016, St. Petersburg, FL, USA. ACM. 978-1-4503-3549-2/16/01...$15.00 http://dx.doi.org/10.1145/2837614.2837620

<!-- Page 2 -->

As an example of a pattern-based macro, suppose that a Racket library implements a Java-like object system and provides a send form, where

```racket
(send a-pt rotate 90)
```

evaluates a-pt to an object, locates a function mapped to the symbol 'rotate within the object, and calls the function as a method by providing the object itself followed by the argument 90. Assuming a lookup-method function that locates a method within an object, the send form can be implemented by a pattern-based macro as follows:

```racket
(define-syntax-rule (send obj-expr method-name arg)
  (let ([obj obj-expr])
    ((lookup-method obj 'method-name) obj arg)))
```

The define-syntax-rule form declares a pattern that is keyed on an initial identifier; in this case, the pattern is keyed on send. The remaining identifiers in the parenthesized send pattern are pattern variables. The second part of the definition specifies a template that replaces a match of the pattern, where each use of a pattern variable in the template is replaced with the corresponding part of the match. With this definition, the example use of send above matches the pattern with a-pt as obj-expr, rotate as method-name, and 90 as arg, so the send use expands to

```racket
(let ([obj a-pt])
  ((lookup-method obj 'rotate) obj 90))
```

Hygienic macro expansion ensures that the identifier obj is not accidentally referenced in an expression that replaces arg in a use of send (Kohlbecker et al. 1986). For example, the body of

```racket
(lambda (obj)
  (send a-pt same? obj))
```

must call the same? method of a-pt with the function argument obj, and not with a-pt itself as bound to obj in the macro template for send. Along similar lines, a local binding of lookup-method at a use site of send must not affect the meaning of lookup-method in send’s template. That is,

```racket
(let ([lookup-method #f])
  (send a-pt rotate 90))
```

should still call the rotate method of a-pt. Macros can be bound locally, and macros can even expand to definitions of macros. For example, suppose that the library also provides a with-method form that performs a method lookup just once for multiple sends:

```racket
(with-method ([rot-a-pt (a-pt rotate)]) ; find rotate
  (for ([i 1000000])
    (rot-a-pt 90))) ; send rotate to point many times
```

The implementation of with-method can make rot-a-pt a local macro binding, where a use of rot-a-pt expands to a function call with a-pt added as the first argument to the function. That is, the full expansion is

```racket
(let ([obj a-pt])
  (let ([rot-a-pt-m (lookup-method obj 'rotate)])
    (for ([i 1000000])
      (rot-a-pt-m obj 90))))
```

but the intermediate expansion is

```racket
(let ([obj a-pt])
  (let ([rot-a-pt-m (lookup-method obj 'rotate)])
    (let-syntax ([rot-a-pt (syntax-rules ()
            [(rot-a-pt arg)
              (rot-a-pt-m obj arg)])])
      (for ([i 1000000])
        (rot-a-pt 90)))))
```

where let-syntax locally binds the macro rot-a-pt. The macro is implemented by a syntax-rules form that produces an anonymous pattern-based macro (in the same way that lambda produces an anonymous function). In other words, with-method is a binding form, it is a macro-generating macro, it relies on local-macro binding, and the macro that it generates refers to a private binding obj that is also macro-introduced. Nevertheless, with-method is straightforwardly implemented as a pattern-based macro:

```racket
(define-syntax-rule
  (with-method ([local-id (obj-expr method-name)])
    body)
  (let ([obj obj-expr])
    (let ([method (lookup-method obj 'method-name)])
      (let-syntax ([local-id (syntax-rules ()
              [(local-id arg)
                (method obj arg)])])
        body))))
```

Note that the obj binding cannot be given a permanently distinct name within with-method. A distinct name must be generated for each use of with-method, so that nested uses create local macros that reference the correct obj.

In general, the necessary bindings or even the binding structure of a macro’s expansion cannot be predicted in advance of expanding the macro. For example, the let identifier that starts the with-method template could be replaced with a macro argument, so that either let or, say, a lazy variant of let could be supplied to the macro. The expander must accommodate such macros by delaying binding decisions as long as possible. Meanwhile, the expander must accumulate information about the origin of identifiers to enable correct binding decisions.

Even with additional complexities—where the macro-generated macro is itself a binding form, where uses can be nested so the different uses of the generated macro must have distinct bindings, and so on—pattern-based macros support implementations that are essentially specifications (Kohlbecker and Wand 1987). A naive approach to macros and binding fails to accommodate the specifications (Adams 2015, sections 4.2-4.5), while existing formalizations of suitable binding rules detour into concepts of marks and renamings that are distant from the programmer’s sense of the specification.

The details of a formalization matter more when moving beyond pattern-matching macros to procedural macros, where the expansion of a macro can be implemented by an arbitrary compile-time function. The syntax-case and syntax forms provide the pattern-matching and template-construction facilities, respectively, of syntax-rules, but they work as expressions within a compile-time function (Dybvig et al. 1993). This combination allows a smooth transition from pattern-based macros to procedural macros for cases where more flexibility is needed. In fact, syntax-rules is itself simply a macro that expands to a procedure:

```racket
(define-syntax-rule (syntax-rules literals
    [pattern template] ...)
  (lambda (stx)
    (syntax-case stx literals
      [pattern #'template] ; #'_ is short for (syntax _)
      ...)))
```

Besides allowing arbitrary computation mixed with pattern matching and template construction, the syntax-case system provides operations for manipulating program representations as syntax objects. Those operations include “bending” hygiene by attaching the binding context of one syntax object to another. For example, a macro might accept an identifier point and synthesize the identifier make-point, giving the new identifier the same context as point so that make-point behaves as if it appeared in the same source location with respect to binding.

<!-- Page 3 -->

Racket provides an especially rich set of operations on syntax objects to enable macros that compose and cooperate (Flatt et al. 2012). Racket’s macro system also relies on a module layer that prevents interference between run-time and compile-time phases of a program, since interference would make macros compose less reliably (Flatt 2002). Finally, modules can be nested and macro-generated, which enables macros and modules to implement facets of a program that have different instantiation times—such as the program’s run-time code, its tests, and its configuration metadata (Flatt 2013).

The module-level facets of Racket’s macro system are, at best, awkwardly accommodated by existing models of macro binding; those models are designed for expression-level binding, where α-renaming is straightforward, while modules address a more global space of mutually recursive macro and variable definitions. A goal of our new binding model is to more simply and directly account for such definition contexts.

## 3. Scope Sets for Pattern-Based Macros

Like previous models of macro expansion, our set-of-scopes expander operates on a program from the outside in. The expander detects bindings, macro uses, and references as part of the outside-to-inside traversal. The difference in our expander is the way that bindings and macro expansions are recorded and attached to syntax fragments during expansion.

### 3.1 Scope Sets

A scope corresponds to a binding context, and every identifier in a program has a set of scopes. For example, if we treat let and lambda as primitive binding forms, then in the fully expanded expression

```racket
(let ([x 1])
  (lambda (y)
    z))
```

the let form corresponds to a scope alet, and the lambda form corresponds to blam. That is, everything in the let’s body is in alet, and everything in the inner lambda’s body is in blam; the set of scopes associated with z is {alet, blam}. (Notation: the subscripts on alet and blam are just part of the names that we use to refer to abstract scope tokens; they have no meaning beyond indicating the scope’s origin.) In a macro-extensible language, expanding a use of a macro creates a new scope in the same way that a binding form creates a new scope. Starting with

```racket
(let ([x 1])
  (let-syntax ([m (syntax-rules ()
          [(m) x])])
    (lambda (x)
      (m))))
```

the right-hand side of the m binding has the scope set {alet}, while the final m has scope set {alet, bls, clam} corresponding to the let, let-syntax, and lambda forms. We can write the scope sets next to each x and m at the point where macro expansion reaches the (m) form:

```racket
(let ([x{alet} 1])
  (let-syntax ([m{alet, bls} (syntax-rules ()
          [(m) #'x{alet}])])
    (lambda (x{alet, bls, clam})
      (m{alet, bls, clam}))))
```

The expansion of (m) produces x with the scope set {alet, dintro}, where dintro is a new scope for identifiers that are introduced by the macro’s expansion:

```racket
(let ([x{alet} 1])
  (let-syntax ([m{alet, bls} (syntax-rules ()
          [(m) #'x{alet}])])
    (lambda (x{alet, bls, clam})
      x{alet, dintro})))
```

The absence of clam on the final x explains why it doesn’t refer to the inner binding of x. At the same time, if a different m places a macro-introduced x in a binding position around an x from a macro use (m x), the x from the use is not macro-introduced and doesn’t have the scope dintro, so it wouldn’t refer to the macro-introduced binding.

Lexical scoping corresponds to sets that are constrained to a particular shape: For any given set, there’s a single scope s that implies all the others (i.e., the ones around s in the program). As a result, s by itself is enough information to identify a binding for a given reference. We normally describe lexical scope in terms of the closest such s for some notion of “closest.” Given scope sets instead of individual scopes, we can define “closest” as the largest relevant set.

More generally, we can define binding based on subsets: A reference’s binding is found as one whose set of scopes is a subset of the reference’s own scopes (in addition to having the same symbolic name). The advantage of using sets of scopes is that macro expansion creates scope sets that overlap in more general ways; there’s not always a s that implies all the others. Absent a determining s, we can’t identify a binding by a single scope, but we can identify it by a set of scopes.

If arbitrary sets of scopes are possible, then two different bindings might have overlapping scopes, neither might be a subset of the other, and both might be subsets of a particular reference’s scope set. In that case, the reference is ambiguous. Creating an ambiguous reference with only pattern-based macros is possible, but it requires a definition context that supports mingled macro definitions and uses; we provide an example in section 3.5.

### 3.2 Bindings

When macro expansion encounters a primitive binding form, it

- creates a new scope;
- adds the scope to every identifier in binding position, as well as to the region where the bindings apply; and
- extends a global table that maps a <symbol, scope set> pair to a representation of a binding.

In a simplified language where bindings are local, an identifier with its scope set could be its own representation of a binding. In a more complete language, bindings can also refer to module imports. We therefore represent a local binding with a unique, opaque value (e.g., a gensym). For example,

```racket
(let ([x 1])
  (let-syntax ([m (syntax-rules ()
          [(m) x])])
    (lambda (x)
      (m))))
```

more precisely expands after several steps to

```racket
(let ([x{alet} 1])
  (let-syntax ([m{alet, bls}
        (syntax-rules ()
          [(m) #'x{alet}])])
    (lambda (x{alet, bls, clam})
      x{alet, dintro})))
x{alet} -> x4
m{alet, bls} -> m8
x{alet, bls, clam} -> x16
```

where the compile-time environment along the way (not shown) maps x4 to a variable, m8 to a macro, and x16 to another variable.

<!-- Page 4 -->

The reference x{alet, dintro} has the binding x4, because x{alet} is the mapping for x in the binding table that has the largest subset of {alet, dintro}. The distinction between the binding table and the compile-time environment is important for a purely “syntactic” view of binding, where a term can be expanded, manipulated, transferred to a new context, and then expanded again. Some approaches to macros, such as syntactic closures (Bawden and Rees 1988) and explicit renaming (Clinger 1991), tangle the binding and environment facets of expansion so that terms cannot be manipulated with the same flexibility.

The binding table can grow forever, but when a particular scope becomes unreachable (i.e., when no reachable syntax object includes a reference to the scope), then any mapping that includes the scope becomes unreachable. This weak mapping can be approximated by attaching the mapping to the scope, instead of using an actual global table. Any scope in a scope set can house the binding, since the binding can only be referenced using all of the scopes in the set. Attaching to the most recently allocated scope is a good heuristic, because the most recent scope is likely to be maximally distinguishing and have the shortest lifetime.

### 3.3 Recursive Macros and Use-Site Scopes

So far, our characterization of macro-invocation scopes works only for non-recursive macro definitions. To handle recursive macro definitions, in addition to a fresh scope to distinguish forms that are introduced by a macro, a fresh scope is needed to distinguish forms that are present at the macro use site. Consider the following letrec-syntax expression, whose meaning depends on whether a use-site identifier captures a macro-introduced identifier:

```racket
(letrec-syntax ([identity (syntax-rules ()
        [(_ misc-id)
          (lambda (x)
            (let ([misc-id 'other])
              x))])])
  (identity x))
```

Assuming that the letrec-syntax form creates a scope als, the scope must be added to both the right-hand side and body of the letrec-syntax form to create a recursive binding:

```racket
(letrec-syntax ([identity (syntax-rules ()
        [(_ misc-id)
          (lambda (x{als})
            (let ([misc-id 'other])
              x{als}))])])
  (identity x{als}))
```

If we create a scope only for introduced forms in a macro expansion, then expanding (identity x{als} ) creates the scope set bintro and produces

```racket
(lambda (x{als, bintro})
  (let ([x{als} 'other])
    x{als, bintro}))
```

where bintro is added to each of the two introduced xs. The lambda introduces a new scope clam, and let introduces dlet, producing

```racket
(lambda (x{als, bintro, clam})
  (let ([x{als, clam, dlet} 'other])
    x{als, bintro, clam, dlet}))
```

At this point, the binding of the innermost x is ambiguous: {als, bintro, clam, dlet} is a superset of both {als, bintro, clam} and {als, clam, dlet}, neither of which is a subset of the other. Instead, we want x to refer to the lambda binding. Adding a scope for the macro-use site corrects this problem. If we call the use-site scope euse, then we start with

```racket
(identity x{als, euse})
```

which expands to

```racket
(lambda (x{als, bintro})
  (let ([x{als, euse} 'other])
    x{als, bintro}))
```

which ends up as

```racket
(lambda (x{als, bintro, clam})
  (let ([x{als, clam, dlet, euse} 'other])
    x{als, bintro, clam, dlet}))
```

There’s no ambiguity, and the final x refers to the lambda binding as intended. In short, each macro expansion needs a use-site scope as the symmetric counterpart to the macro-introduction scope.

### 3.4 Use-Site Scopes and Macro-Generated Definitions

In a binding form such as let or letrec, bindings are clearly distinguished from uses by their positions within the syntactic form. In addition to these forms, Racket (like Scheme) supports definition contexts that mingle binding forms and expressions. For example, the body of a module contains a mixture of definitions and expressions, all in a single recursive scope. Definitions can include macro definitions, expressions can include uses of those same macros, and macro uses can even expand to further definitions.

With set-of-scopes macro expansion, macro definitions and uses within a single context interact badly with use-site scopes. For example, consider a define-identity macro that is intended to expand to a definition of a given identifier as the identity function:

```racket
(define-syntax-rule (define-identity id)
  (define id (lambda (x) x)))
(define-identity f)
(f 5)
```

If the expansion of (define-identity f) adds a scope to the use-site f, the resulting definition does not bind the f in (f 5). The underlying issue is that a definition context must treat use-site and introduced identifiers asymmetrically as binding identifiers. In

```racket
(define-syntax-rule (define-five misc-id)
  (begin
    (define misc-id 5)
    x))
(define-five x)
```

the introduced x should refer to an x that is defined in the enclosing scope, which turns out to be the same x that appears at the use site of define-five. But in

```racket
(define-syntax-rule (define-other-five misc-id)
  (begin
    (define x 5)
    misc-id))
(define-other-five x)
```

the x from the use site should not refer to the macro-introduced binding x.

To support macros that expand to definitions of given identifiers, a definition context must keep track of scopes created for macro uses, and it must remove those scopes from identifiers that end up in binding positions. In the define-identity and define-five examples, the use-site scope is removed from the binding identifiers x and f, so they are treated the same as if their definitions appeared directly in the source.

<!-- Page 5 -->

This special treatment of use-site scopes adds complexity to the macro expander, but it is of the kind of complexity that mutually recursive binding contexts create routinely (e.g., along the same lines as ensuring that variables are defined before they are referenced). Definition contexts in Racket have proven convenient and expressive enough to be worth the extra measure of complexity.

### 3.5 Ambiguous References

The combination of use-site scopes to solve local-binding problems (as in section 3.3) versus reverting use-site scopes to accommodate macro-generated definitions (as in section 3.4) creates the possibility of generating an identifier whose binding is ambiguous. The following example defines m through a def-m macro, and it uses m in the same definition context:

```racket
(define-syntax-rule (def-m m given-x)
  (begin
    (define x 1)
    (define-syntax-rule (m)
      (begin
        (define given-x 2)
        x))))
(def-m m x)
(m)
```

The expansion, after splicing begins, ends with an ambiguous reference:

```racket
(define-syntax-rule (def-m{adef} ....) ....)
(define x{adef, bintro1} 1)
(define-syntax-rule (m{adef})
  (begin
    (define x{adef, buse1} 2)
    x{adef, bintro1}))
(define x{adef, cintro2} 2)
x{adef, bintro1, cintro2}
```

The scope adef corresponds to the definition context, bintro1 and buse1 correspond to the expansion of def-m, cintro2 corresponds to the expansion of m. The final reference to x is ambiguous, because it was introduced through both macro layers.

Unlike the ambiguity that is resolved by use-site scopes, this ambiguity arguably reflects an inherent ambiguity in the macro. Absent the (define x 1) definition generated by def-m, the final x reference should refer to the definition generated from (define given-x 2); similarly, absent the definition generated from (define given-x 2), the final x should refer to the one generated from (define x 1). Neither of those definitions is more specific than the other, since they are generated by different macro invocations, so our new expander rejects the reference as ambiguous.

Our previous model of macro expansion to cover definition contexts (Flatt et al. 2012) would treat the final x always as a reference to the definition generated from (define x 1) and never to the definition generated from (define given-x 2). So far, we have not encountered a practical example that exposes the difference between the expanders’ treatment of pattern-based macros in definition contexts.

## 4. Procedural Macros and Modules

Although our set-of-scopes expander resolves bindings differently than in previous models, it still works by attaching information to identifiers, and so it can provide a smooth path from pattern-matching macros to procedural macros in the same way as syntax-case (Dybvig et al. 1993). Specifically, (syntax form) quotes the S-expression form while preserving its scope-set information, so that form can be used to construct the result of a macro.

More precisely, a primitive (quote-syntax form) quotes form with its scope sets in Racket. The derived (syntax form) detects uses of pattern variables and replaces them with their matches while quoting any non-pattern content in form with quote-syntax. A (syntax form) can be abbreviated #'form, and when form includes no pattern variables, #'form is equivalent to (quote-syntax form). The quasiquoting variant #`form (which uses a backquote instead of a regular quote) allows escapes within form as #,expr, which inserts the result of evaluating expr in place of the escape. The result of a quote-syntax or syntax form is a syntax object. When a syntax object’s S-expression component is just a symbol, then the syntax object is an identifier.

### 4.1 Identifier Comparisons with Scope Sets

Various compile-time functions work on syntax objects and identifiers. Two of the most commonly used functions are free-identifier=? and bound-identifier=?, each of which takes two identifiers. The free-identifier=? function is used to recognize a reference to a known binding, such as recognizing a use of else in a conditional. The bound-identifier=? function is used to check whether two identifiers would conflict as bindings in the same context, such as when a macro that expands to a binding form checks that identifiers in the macro use are suitably distinct.

These two functions are straightforward to implement with scope sets. A free-identifier=? comparison on identifiers checks whether the two identifiers have the same binding by consulting the global binding table. A bound-identifier=? comparison checks that two identifiers have exactly the same scope sets, independent of the binding table.

### 4.2 Local Bindings and Syntax Quoting

The set-of-scopes approach to binding works the same as previous models for macros that are purely pattern-based, but the set-of-scopes approach makes finer distinctions among identifiers than would be expected by existing procedural Racket macros that use #' or quote-syntax. To be consistent with the way that Racket macros have been written, quote-syntax must discard some scopes. For example, in the macro

```racket
(lambda (stx)
  (let ([id #'x])
    #`(let ([x 1])
      #,id)))
```

the x that takes the place of #,id should refer to the binding x in the generated let form. The x identifier that is bound to id, however, is not in the scope that is created for the compile-time let:

```racket
(lambda (stx{alam})
  (let ([id{alam, blet} #'x{alam}])
    #`(let ([x{alam, blet} 1])
      #,id{alam, blet})))
```

If quote-syntax (implicit in #`) preserves all scopes on an identifier, then with set-of-scopes binding, the x that replaces #,id will not refer to the x in the generated let’s binding position. It’s tempting to think that the compile-time let should introduce a phase-specific scope that applies only for compile-time references, in which case it won’t affect x as a run-time reference. That adjustment doesn’t solve the problem in general, since a macro can generate compile-time bindings and references just as well as run-time bindings and references. A solution is for the expansion of quote-syntax to discard certain scopes on its content. The discarded scopes are those from

<!-- Page 6 -->

binding forms that enclosed the quote-syntax form up to a phase crossing or module top-level, as well as any use-site scopes recorded for macro invocations within those binding forms. In the case of a quote-syntax form within a macro binding’s right-hand side, those scopes cover all of the scopes introduced on the right-hand side of the macro binding. The resulting macro system is different than the old Racket macro system. Experiments suggest that the vast majority of macro implementations work either way, but it’s easy to construct an example that behaves differently:

```racket
(free-identifier=? (let ([x 1]) #'x)
  #'x)
```

In Racket’s old macro system, the result is #f. The set-of-scopes system with a scope-pruning quote-syntax produces #t, instead, because the let-generated scope is stripped away from #'x.

If quote-syntax did not prune scopes, then not only would the result above be #f, but bound-identifier=? would produce #f for both (let ([x 1]) #'x) and (let ([y 1]) #'x). Those results reflect the switch to attaching identifier-independent scopes to identifiers, instead of attaching identifier-specific renamings.

Arguably, the issue here is the way that pieces of syntax from different local scopes are placed into the same result syntax object, with the expectation that all the pieces are treated the same way. In other words, Racket programmers have gotten used to an unusual variant of quote-syntax, and most macros could be written just as well with a non-pruning variant.

Supplying a second, non-pruning variant of quote-syntax poses no problems. Our set-of-scopes implementation for Racket implements the non-pruning variant when a #:local keyword is added to a quote-syntax form. For example,

```racket
(free-identifier=? (let ([x 1])
    (quote-syntax x #:local))
  (quote-syntax x #:local))
```

produces #f instead of #t, because the scope introduced by let is preserved in the body’s syntax object. The non-pruning variant of quote-syntax is useful for embedding references in a program’s full expansion that are meant to be inspected by tools other than the Racket compiler; Typed Racket’s implementation uses the #:local variant of quote-syntax to embed type declarations (including declarations for local bindings) in a program’s expansion for use by its type checker.

### 4.3 Ensuring Distinct Bindings

A Racket macro’s implementation can arrange for an identifier introduced by a macro expansion to have an empty scope set.[^1] More generally, a macro can arrange for identifiers that are introduced in different contexts to have the same symbol and scope set. If those identifiers appear as bindings via lambda, let, or let-syntax, then the new scope created for the binding form will ensure that the different identifiers produce different bindings. That is, the binding scope is always created after any expansion that introduced the bound identifier, so all bindings are kept distinct by those different binding scopes.

For example, assuming that make-scopeless creates an identifier that has no scopes in an expansion, then the let-x forms in

```racket
(define-syntax (let-x stx)
  (syntax-case stx ()
    [(_ rhs body)
      #`(let ([#,(make-scopeless 'x) rhs])
        body)]))
(let-x 5
  (let-x 6
    0))
```

create intermediate x identifiers that each have an empty scope set, but the full expansion becomes

```racket
(let ([x{alet} 5])
  (let ([x{blet} 6])
    0))
```

where alet and blet are created by each let (as a primitive binding form), and they distinguish the different x bindings.

[^1]: Avoiding a macro-introduction scope involves using a syntax-local-introduce function.

In a definition context (see section 3.4), macro expansion can introduce an identifier to a binding position after the scope for the definition context is created (and after that scope is applied to the definition context’s original content). That ordering risks a collision among bindings in different definition contexts, where identifiers introduced into different definition contexts all have the same symbol and set of scopes.

For example, using a block form that creates a definition context and that we treat here as a primitive form, the uses of defx in

```racket
(define-syntax (def-x stx)
  (syntax-case stx ()
    [(_ rhs)
      #`(define #,(make-scopeless 'x) rhs)]))
(block
  (define y 1)
  (def-x 5))
(block
  (define y 2)
  (def-x 6))
```

risk expanding as

```racket
(block
  (define y{adef }
    1)
  (define x{} 5))
(block
  (define y{bdef }
    2)
  (define x{} 6))
```

with conflicting bindings of x for the empty scope set. To avoid the possibility of such collisions, in a definition context that supports both definitions and macro expansion, the context is represented by a pair of scopes: an outside-edge scope that is added to the original content of the definition context, and an inside-edge scope that is added to everything that appears in the definition context through macro expansion. The outside-edge scope distinguishes original identifiers from macro-introduced identifiers, while the inside-edge scope ensures that every binding created for the definition context is distinct from all other bindings. Thus, the preceding example expands as

```racket
(block
  (define y{aout, ain} 1)
  (define x{ain} 5))
(block
  (define y{bout, bin} 2)
  (define x{bin} 6))
```

where the inside-edge scopes ain and bin distinguish the two x bindings. Meanwhile, if the definitions of y instead used the name x, they would remain distinguished from the macro-introduced xs by the outside-edge scopes aout and bout.

<!-- Page 7 -->

### 4.4 First-Class Definition Contexts

Racket exposes the expander’s support for definition contexts (see section 3.4) so that new macros can support definition contexts while potentially changing the meaning of a macro or variable definition. For example, the class macro allows local macro definitions in the class body while it rewrites specified function definitions to methods and other variable definitions to fields. The unit form similarly rewrites variable definitions to a mixture of private and exported definitions with a component.

Implementing a definition context starts with a call to syntax-local-make-definition-context, which creates a first-class (at compile time) value that represents the definition context. A macro can force expansion of forms in the definition context, it can add variable bindings to the definition context, and it can add compile-time bindings and values that are referenced by further macro expansion within the definition context. To a first approximation, a first-class definition context corresponds to an inside-edge scope that is added to any form expanded within the definition context and that houses the definition context’s bindings. A definition context also has a compile-time environment frame (extending the context of the macro use) to house the mapping of bindings to variables and compile-time values.

Like other definition contexts (see section 3.4), the compile-time environment must track use-site scopes that are generated for macro expansions within a first-class definition context. If the macro moves any identifier into a binding position in the overall expansion, then the macro normally must remove accumulated use-site scopes (for the current definition context only) by applying syntax-local-identifier-as-binding to the identifier. For example, the unit form implements a definition context that is similar to the body of a lambda, but variables are internally transformed to support mutually recursive references across unit boundaries.

```racket
(unit (import)
  (export)
  (define x 1)
  x)
```

In this example, (define x 1) is expanded to (define-values (x) 1) with a use-site scope on x, but the intent is for this definition of x to capture the reference at the end of the unit form. If the unit macro simply moved the binding x into a letrec right-hand side, the x would not capture the final x as moved into the letrec body; the use-site scope on the definition’s x would prevent it from capturing the use. The solution is for the unit macro to apply syntax-local-identifier-as-binding to the definition’s x before using it as a letrec binding. Macros that use a definition context and bound-identifier=? must similarly apply syntax-local-identifier-as-binding to identifiers before comparing them with bound-identifier=?.

Even if a macro does not create a first-class definition context, some care is needed if a macro forces the expansion of subforms and moves pieces of the result into binding positions. Such a macro probably should not use syntax-local-identifier-as-binding, but it should first ensure that the macro use is in an expression context before forcing any subform expansions. Otherwise, the subform expansions could interact in unexpected ways with the use-site scopes of an enclosing definition context.

Use-site scopes associated with a first-class definition context are not stored directly in the compile-time environment frame for the definition context. Instead, they are stored in the closest frame that is not for a first-class definition context, so that the scopes are still tracked when the definition context is discarded (when the macro returns, typically). The scope for the definition context itself is similarly recorded in the closest such frame, so that quote-syntax can remove it, just like other binding scopes.

### 4.5 Modules and Phases

The module form creates a new scope for its body. More precisely, a module form creates an outside-edge scope and an inside-edge scope, like any other context that allows both definitions and macro expansion.

A (module* name #f ....) submodule form, where #f indicates that the enclosing module’s bindings should be visible, creates an additional scope in the obvious way. For other module* and module submodule forms, the macro expander prevents access to the enclosing module’s bindings by removing the two scopes of the enclosing module.

A module distinguishes bindings that have the same name but different phases. For example, lambda might have one meaning for run-time code within a module, but a different meaning for compile-time code within the same module. Furthermore, instantiating a module at a particular phase implies a phase shift in its syntax literals. Consider the module

```racket
(define x 1)
(define-for-syntax x 2)
(define id #'x)
(define-for-syntax id #'x)
(provide id (for-syntax id))
```

and suppose that the module is imported both normally and for compile time, the latter with a s: prefix. In a compile-time context within the importing module, both id and s:id will be bound to an identifier x that had the same scopes originally, but they should refer to different x bindings (in different module instances with different values).

Among the possibilities for distinguishing phases, having per-phase sets of scopes on an identifier makes the phase-shifting operation most natural. A local binding or macro expansion can add scopes at all phases, while module adds a distinct inside-edge scope to every phase (and the same outside-edge scope to all phases). Since every binding within a module is forced to have that module’s phase-specific inside-edge scopes, bindings at different scopes will be appropriately distinguished.

Having a distinct “root” scope for each phase makes most local bindings phase-specific. That is, in

```racket
(define-for-syntax x 10)
(let ([x 1])
  (let-syntax ([y x])
    ....))
```

the x on the right-hand side of let-syntax sees the top-level phase-1 x binding, not the phase-0 local binding. This is a change from Racket’s old approach to binding and phases, but the only programs that are affected are ones that would trigger an out-of-context error in the old system. Meanwhile, macros can construct identifiers that have no module scope, so out-of-context errors are still possible.

## 5. Implementation and Experience

Scope sets have an intuitive appeal as a model of binding, but a true test of the model is whether it can accommodate a Racket-scale use of macros—for constructing everything from simple syntactic abstractions to entirely new languages. Indeed, the set-of-scopes model was motivated in part by a fraying of Racket’s old macro expander at the frontiers of its implementation, e.g., for submodules (Flatt 2013).[^2]

<!-- Page 8 -->

We released the new macro expander as part of Racket version 6.3 (released November 2015), while Racket developers started using the expander about four months earlier. Compared to the previous release, build times, memory use, and bytecode footprint were essentially unchanged compared to the old expander. Getting performance on par with the previous system required about two weeks of performance tuning, which we consider promising in comparison to a system that has been tuned over the past 15 years.

### 5.1 Initial Compatibility Results

At the time that Racket developers switched to the new expander, packages in Racket’s main distribution had been adjusted to build without error (including all documentation), and most tests in the corresponding test suite passed; 43 out of 7501 modules failed. Correcting those failures before version 6.3 required small changes to accommodate the new macro expander.

Achieving the initial level of success required small changes to 15 out of about 200 packages in the distribution, plus several substantial macro rewrites in the core package:

- Changed macros in the core package include the unit, class, and define-generics macros, all of which manipulate scope in unusual ways.

- The Typed Racket implementation, which is generally sensitive to the details of macro expansion, required a handful of adjustments to deal with changed expansions of macros and the new scope-pruning behavior of quote-syntax.

- Most other package changes involve languages implementations that generate modules or submodules and rely on a non-composable treatment of module scopes by the old expander (which creates trouble for submodules in other contexts).

In about half of all cases, the adjustments for set-of-scopes expansion are compatible with the existing expander. In the other half, the macro adjustments were incompatible with the previous expander and the two separate implementations seem substantially easier to produce than one unified implementation.

Besides porting the main Racket distribution to a set-of-scopes expander, we tried building and testing all packages registered at http://pkgs.racket-lang.org/. There were 46 failures out of about 400 packages, as opposed to 21 failures for the same set of packages with the previous Racket release. All of those packages were repaired before the version 6.3 release.

### 5.2 Debugging Support

Although the macro debugger (Culpepper and Felleisen 2010) has proven to be a crucial tool for macro implementors, binding resolution in Racket’s old macro expander is completely opaque to macro implementers.

When something goes wrong, the expander or macro debugger can report little more than “unbound identifier” or “out of context”, because the process of replaying renamings and the encodings used for the renamings are difficult to unpack and relate to the programmer.

A set-of-scopes expander is more frequently in a position to report “unbound identifier, but here are the identifier’s scopes, and here are some bindings that are connected to those scopes.” In the case of ambiguous bindings, the expander can report the referencing identifier’s scopes and the scopes of the competing bindings. These details are reported in a way similar to stack traces: subject to optimization and representation choices, and underspecified as a result, but invaluable for debugging purposes.

[^2]: For an example of a bug report about submodules, see problem report 14521 at http://bugs.racket-lang.org/query/?debug=&database=default&cmd=view+audit-trail&cmd=view&pr=14521. The example program fails with the old expander, due to problems meshing mark-oriented module scope with renaming-oriented local scope, but the example works with the set-of-scopes expander.

### 5.3 Scope Sets for JavaScript

Although the set-of-scopes model of binding was developed with Racket as a target, it is also intended as a more understandable model of macros to facilitate the creation of macro systems for other languages. In fact, the Racket implementation was not the first implementation of the model to become available. Based on an early draft of this report, Tim Disney revised the Sweet.js macro implementation for JavaScript (Disney et al. 2014; Disney et al. 2015)[^3] to use scope sets even before the initial Racket prototype was complete. Disney reports that the implementation of hygiene for the macro expander is now “mostly understandable” and faster.

[^3]: See pull request 461 at https://github.com/mozilla/sweet.js/pull/461.

## 6. Model

We present a formal model of set-of-scope expansion following the style of Flatt et al. (2012). As a first step, we present a model where only run-time expressions are expanded, and implementations of macros are simply parsed. As a second step, we generalize the model to include phase-specific scope sets and macro expansion at all phases. The third step adds support for local expansion, and the fourth step adds first-class definition contexts. The model does not cover modules or top-level namespaces.

### 6.1 Single-Phase Expansion

Our macro-expansion model targets a language that includes with variables, function calls, functions, atomic constants, lists, and syntax objects:

```text
ast  ::= var | APP(ast, ast, ...) | val
var  ::= VAR(name)
val  ::= FUN(var, ast) | atom | LIST(val, ...) | stx
stx  ::= STX(atom, ctx) | STX(LIST(stx, ...), ctx)
id   ::= STX(sym, ctx)
atom ::= sym | prim | ...
sym  ::= 'name
name ::= a token such as x, egg, or lambda
```

Since the model is concerned with macro expansion and programmatic manipulation of program terms, we carefully distinguish among:

- names, which are abstract tokens;
- variables, which correspond to function arguments and references in an AST and are formed by wrapping a name as `VAR(name)`;
- symbols, which are values with respect to the evaluator and are formed by prefixing a name with a quote; and
- identifiers, which are also values, are formed by combining a symbol with a set of scopes, and are a subset of syntax objects.

For a further explanation of the distinctions among these different uses of names, see Flatt et al. (2012, section 3.2.1).

The model’s evaluator is standard and relies on a δ function to implement primitives:

<!-- Page 9 -->

```text
eval : ast -> val

eval⟦APP(ast_fun, ast_arg)⟧ = eval⟦ast_body[var←eval⟦ast_arg⟧]⟧
  subject to eval⟦ast_fun⟧ = FUN(var, ast_body)

eval⟦APP(prim, ast_arg, ...)⟧ = δ(prim, eval⟦ast_arg⟧, ...)

eval⟦val⟧ = val
```

Interesting primitives include the ones that manipulate syntax objects,

```text
prim ::= stx-e | mk-stx | ...
```

where `stx-e` extracts the content of a syntax object, and `mk-stx` creates a new syntax object with a given content and the scopes of a given existing syntax object:

```text
δ(stx-e, STX(val, ctx)) = val
δ(mk-stx, atom, STX(val, ctx)) = STX(atom, ctx)
δ(mk-stx, LIST(stx, ...), STX(val, ctx)) = STX(LIST(stx, ...), ctx)
```

Macro expansion takes a program that is represented as as a syntax object and produces a fully expanded syntax object. To evaluate the program, the syntax object must be parsed into an AST. The parser uses a `parse` metafunction that takes an identifier and a binding store, Σ. The names `lambda`, `quote`, and `syntax`, represent the core syntactic forms, along with the implicit forms of function calls and variable reference:

```text
parse : stx Σ -> ast

parse⟦STX(LIST(id_lam, id, stx_body), ctx), Σ⟧ = FUN(VAR(resolve⟦id, Σ⟧), parse⟦stx_body, Σ⟧)
  subject to resolve⟦id_lam, Σ⟧ = lambda

parse⟦STX(LIST(id_quote, stx), ctx), Σ⟧ = strip⟦stx⟧
  subject to resolve⟦id_quote, Σ⟧ = quote

parse⟦STX(LIST(id_syntax, stx), ctx), Σ⟧ = stx
  subject to resolve⟦id_syntax, Σ⟧ = syntax

parse⟦STX(LIST(stx_fun, stx_arg, ...), ctx), Σ⟧ = APP(parse⟦stx_fun, Σ⟧, parse⟦stx_arg, Σ⟧, ...)

parse⟦id, Σ⟧ = VAR(resolve⟦id, Σ⟧)
```

The `resolve` metafunction extracts an identifier’s name and its binding context. For now, we ignore phases and define a binding context as simply a set of scopes. A binding store maps a name to a mapping from scope sets to bindings, where bindings are represented by fresh names.

```text
ctx     ::= scp-set
scp-set ::= {scp, ...}
Σ       ::= binding store, name -> (scp-set -> name)
scp     ::= a token that represents a scope
```

The `resolve` metafunction uses these pieces along with a `biggest-subset` helper function to select a binding. If no binding is available in the store, the identifier’s symbol’s name is returned, which effectively allows access to the four primitive syntactic forms; the macro expander will reject any other unbound reference.

```text
resolve : id Σ -> name

resolve⟦STX('name, ctx), Σ⟧ = name_biggest
  subject to Σ(name) = {scp_bind ↦ name_bind, ...},
    biggest-subset⟦ctx, {scp_bind, ...}⟧ = scp_biggest,
    {scp_bind ↦ name_bind, ...}(scp_biggest) = name_biggest

resolve⟦STX('name, ctx), Σ⟧ = name

biggest-subset : scp-set {scp-set, ...} -> scp-set

biggest-subset⟦scp_ref, {scp_bind, ...}⟧ = scp_biggest
  subject to scp_biggest ⊆ scp_ref, scp_biggest ∈ {scp_bind, ...},
    scp_bind ⊆ scp_ref => scp_bind ⊆ scp_biggest
```

If multiple incomparable largest subsets apply, the reference is ambiguous.

Finally, we’re ready to define the `expand` metafunction. In addition to a syntax object (for a program to expand) and a binding store, the expander needs an environment, ξ, that maps bindings to compile-time meanings. The possible meanings of a binding are the three primitive syntactic forms recognized by parse, the `let-syntax` primitive form, a reference to a function argument, or a compile-time value—where a compile-time function represents a macro transformer.

```text
ξ ::= a mapping from name to transform
transform ::= lambda | let-syntax | quote | syntax | VAR(id) | val
```

The expansion metafunction can be read as:

```text
expand : stx ξ Σ -> ⟨stx, Σ⟩
```

The process of macro expansion creates new bindings, so the `expand` metafunction produces a tuple containing an updated binding store along with the expanded program. For example, the simplest case is for the `quote` form, which leaves the body of the form and the store as-is:

```text
expand⟦STX(LIST(id_quote, stx), ctx), ξ, Σ⟧ = ⟨STX(LIST(id_quote, stx), ctx), Σ⟩
  subject to resolve⟦id_quote, Σ⟧ = quote
```

Since we are not yet dealing with expansion of compile-time terms, no scope pruning is needed for syntax, and it can be essentially the same as quote.

```text
expand⟦STX(LIST(id_syntax, stx), ctx), ξ, Σ⟧ = ⟨STX(LIST(id_syntax, stx), ctx), Σ⟩
  subject to resolve⟦id_syntax, Σ⟧ = syntax
```

Expansion of a lambda form creates a fresh name and fresh scope for the argument binding. Adding the new scope to the formal argument (we define the `add` metafunction later) creates the binding identifier. The new binding is added to the store, Σ, and it is also recorded in the compile-time environment, ξ, as a variable binding. The body of the function is expanded with those extensions after receiving the new scope, and the pieces are reassembled into a lambda form.

```text
expand⟦STX(LIST(id_lam, id_arg, stx_body), ctx), ξ, Σ⟧
  = ⟨STX(LIST(id_lam, id_new, stx_body2), ctx), Σ4⟩
  subject to resolve⟦id_lam, Σ⟧ = lambda,
    alloc-name⟦Σ⟧ = ⟨name_new, Σ1⟩,
    alloc-scope⟦Σ1⟧ = ⟨scp_new, Σ2⟩, add⟦id_arg, scp_new⟧ = id_new,
    Σ2+{id_new→name_new} = Σ3,
    ξ+{name_new→VAR(id_new)} = ξ_new,
    expand⟦add⟦stx_body, scp_new⟧, ξ_new, Σ3⟧ = ⟨stx_body2, Σ4⟩
```

When the generated binding is referenced (i.e., when resolving an identifier produces a binding that is mapped as a variable), then the reference is replaced with the recorded binding, so that the reference is bound-identifier=? to the binding in the expansion result.

```text
expand⟦id, ξ, Σ⟧ = ⟨id_new, Σ⟩
  subject to ξ(resolve⟦id, Σ⟧) = VAR(id_new)
```

<!-- Page 10 -->

A local macro binding via `let-syntax` is similar to an argument binding, but the compile-time environment records a macro transformer instead of a variable. The transformer is produced by using `parse` and then `eval` on the compile-time expression for the transformer. After the body is expanded, the macro binding is no longer needed, so the body expansion is the result.

```text
expand⟦STX(LIST(id_ls, id, stx_rhs, stx_b), ctx), ξ, Σ⟧ = expand⟦stx_b2, ξ2, Σ3⟧
  subject to resolve⟦id_ls, Σ⟧ = let-syntax,
    alloc-name⟦Σ⟧ = ⟨name_new, Σ1⟩, alloc-scope⟦Σ1⟧ = ⟨scp_new, Σ2⟩,
    add⟦id, scp_new⟧ = id_new, Σ2+{id_new→name_new} = Σ3,
    ξ+{name_new→eval⟦parse⟦stx_rhs, Σ3⟧⟧} = ξ2,
    add⟦stx_b, scp_new⟧ = stx_b2
```

Finally, when the expander encounters an identifier that resolves to a binding mapped to a macro transformer, the transformer is applied to the macro use. Fresh scopes are generated to represent the use site, `scu`, and introduced syntax, `sci`, where the introduced-syntax scope is applied using `flip` to both the macro argument and result, where `flip` corresponds to an exclusive-or operation to leave the scope intact on syntax introduced by the macro (see below).

```text
expand⟦stx_macapp, ξ, Σ⟧ = expand⟦flip⟦stx_exp, scp_i⟧, ξ, Σ2⟧
  subject to stx_macapp = STX(LIST(id_mac, stx_arg, ...), ctx),
    ξ(resolve⟦id_mac, Σ⟧) = val, alloc-scope⟦Σ⟧ = ⟨scp_u, Σ1⟩,
    alloc-scope⟦Σ1⟧ = ⟨scp_i, Σ2⟩,
    eval⟦APP(val, flip⟦add⟦stx_macapp, scp_u⟧, scp_i⟧)⟧ = stx_exp
```

The only remaining case of `expand` is to recur for function-call forms, threading through the binding store using an accumulator-style `expand*` helper:

```text
expand⟦STX(LIST(stx_fun, stx_arg, ...), ctx), ξ, Σ⟧
  = ⟨STX(LIST(stx_fun2, stx_arg2, ...), ctx), Σ1⟩
  subject to expand*⟦(), (stx_fun stx_arg ...), ξ, Σ⟧ = ⟨(stx_fun2 stx_arg2 ...), Σ1⟩

expand* : (stx ...) (stx ...) ξ Σ -> ⟨(stx ...), Σ⟩
expand*⟦(stx_e ...), (), ξ, Σ⟧ = ⟨(stx_e ...), Σ⟩
expand*⟦(stx_e ...), (stx0 stx1 ...), ξ, Σ⟧ = expand*⟦(stx_e ... stx_e0), (stx1 ...), ξ, Σ1⟧
  subject to expand⟦stx0, ξ, Σ⟧ = ⟨stx_e0, Σ1⟩
```

For completeness, here are the `add` and `flip` metafunctions for propagating scopes to all parts of a syntax object, where `scp ⊕ ctx` adds `scp` to `ctx` if it is not already in `ctx` or removes it otherwise:

```text
add : stx scp -> stx
add⟦STX(atom, ctx), scp⟧ = STX(atom, {scp} ∪ ctx)
add⟦STX(LIST(stx, ...), ctx), scp⟧ = STX(LIST(add⟦stx, scp⟧, ...), {scp} ∪ ctx)

flip : stx scp -> stx
flip⟦STX(atom, ctx), scp⟧ = STX(atom, scp ⊕ ctx)
flip⟦STX(LIST(stx, ...), ctx), scp⟧ = STX(LIST(flip⟦stx, scp⟧, ...), scp ⊕ ctx)
```

To take a program from source to value, use `expand`, then `parse`, then `eval`.

### 6.2 Multi-Phase Expansion

To support phase-specific scope sets, we change the definition of context so that it is a mapping from phases to scope sets:

```text
ph  ::= integer
ctx ::= a mapping from ph to scp-set
```

With this change, many metafunctions must be indexed by the current phase of expansion. For example, the result of `resolve` depends on the current phase:

```text
resolve : ph id Σ -> name

resolve_ph⟦STX('name, ctx), Σ⟧ = name_biggest
  subject to Σ(name) = {scp_bind ↦ name_bind, ...},
    biggest-subset⟦ctx(ph), {scp_bind, ...}⟧ = scp_biggest,
    {scp_bind ↦ name_bind, ...}(scp_biggest) = name_biggest

resolve_ph⟦STX('name, ctx), Σ⟧ = name
```

Phase-specific expansion allows `let-syntax` to expand the compile-time expression for a macro implementation, instead of just parsing the expression. Note that the uses of `expand` and `parse` on the transformer expression are indexed by `ph+1`.

```text
expand : ph stx ξ scp-set Σ -> ⟨stx, Σ⟩

expand_ph⟦STX(LIST(id_ls, id, stx_rhs, stx_body), ctx), ξ, scp_p, Σ⟧
  = expand_ph⟦stx_body2, ξ2, scp_p2, Σ4⟧
  subject to resolve_ph⟦id_ls, Σ⟧ = let-syntax,
    alloc-name⟦Σ⟧ = ⟨name_new, Σ1⟩,
    alloc-scope⟦Σ1⟧ = ⟨scp_new, Σ2⟩,
    add_ph⟦id, scp_new⟧ = id_new,
    Σ2+{id_new→name_new} = Σ3,
    expand_{ph+1}⟦stx_rhs, ξ_primitives, ∅, Σ3⟧ = ⟨stx_exp, Σ4⟩,
    ξ+{name_new→eval⟦parse_{ph+1}⟦stx_exp, Σ4⟧⟧} = ξ2,
    add_ph⟦stx_body, scp_new⟧ = stx_body2,
    {scp_new} ∪ scp_p = scp_p2
```

In addition to carrying a phase index, the revised `expand` takes a set of scopes created for bindings. Those scopes are the ones to be pruned from quoted syntax by the revised `syntax` expansion.

```text
expand_ph⟦STX(LIST(id_syntax, stx), ctx), ξ, scp_p, Σ⟧
  = ⟨STX(LIST(id_syntax, stx_pruned), ctx), Σ⟩
  subject to resolve_ph⟦id_syntax, Σ⟧ = syntax,
    prune_ph⟦stx, scp_p⟧ = stx_pruned
```

The `prune` metafunction recurs through a syntax object to remove all of the given scopes at the indicated phase.

```text
prune : ph stx scp-set -> stx

prune_ph⟦STX(atom, ctx), scp_p⟧ = STX(atom, ctx+{ph→ctx(ph) \ scp_p})

prune_ph⟦STX(LIST(stx, ...), ctx), scp_p⟧
  = STX(LIST(stx_pruned, ...), ctx+{ph→ctx(ph) \ scp_p})
  subject to prune_ph⟦stx, scp_p⟧, ... = stx_pruned, ...
```

### 6.3 Local Expansion

Environment inspection via `syntax-local-value` and local expansion via `local-expand` are accommodated in the model essentially as in Flatt et al. (2012), but since local expansion can create bindings, the `eval` metafunction must consume and produce a binding store. The `eval` metafunction also must be indexed by the phase used for syntax operations.

Local expansion needs the current macro expansion’s introduction scope, if any. In addition, local expansions that move identifiers into binding positions need `syntax-local-identifier-as-binding`, which requires information about scopes in the current expansion context. Local expansion, meanwhile, can create new such scopes. To support those interactions, `eval` and `expand` must both consume and produce scope sets for the current use-site scopes, and binding scopes must also be available for local expansion of syntax forms. To facilitate threading through all of that information, we define `scp_i` as an optional current scope and `Σ̂` as an extended store:

```text
scp_i ::= scp | •
Σ̂     ::= ⟨Σ, scp_p, scp_u⟩
```

<!-- Page 11 -->

The second part of a `Σ̂` tuple is a set of scopes to be pruned at syntax forms. The third part is a subset of those scopes that are the current expansion context’s use-site scopes, which are pruned by `syntax-local-identifier-as-binding`. The different parts of a `Σ̂` tuple vary in different ways: the Σ part is consistently threaded through evaluation and expansion, while the scope-set parts are stack-like for expansion and threaded through evaluation. In the case of a macro-application step, the scope-set parts of the tuple are threaded through expansion, too, more like evaluation.

In the model, the `lvalue`, `lexpand`, and `lbinder` primitives represent `syntax-local-value`, `local-expand`, and `syntax-local-identifier-as-binding`, respectively:

```text
eval : ph ast scp_i ξ Σ̂ -> ⟨val, Σ̂⟩

eval_ph⟦APP(lvalue, ast_id), scp_i, ξ, Σ̂⟧ = ⟨ξ(resolve_ph⟦id_result, Σ2⟧), Σ̂2⟩
  subject to eval_ph⟦ast_id, scp_i, ξ, Σ̂⟧ = ⟨id_result, Σ̂2⟩, Σ̂2 = ⟨Σ2, _, _⟩

eval_ph⟦APP(lexpand, ast, ast_stops), scp_i, ξ, Σ̂⟧ = ⟨flip_ph⟦stx_exp, scp_i⟧, Σ̂4⟩
  subject to eval_ph⟦ast, scp_i, ξ, Σ̂⟧ = ⟨stx, Σ̂2⟩,
    eval_ph⟦ast_stops, scp_i, ξ, Σ̂2⟧ = ⟨LIST(id_stop, ...), Σ̂3⟩,
    {var↦unstop⟦ξ(var)⟧ | var ∈ dom(ξ)} = ξ_unstops,
    Σ̂3 = ⟨Σ3, _, _⟩, resolve_ph⟦id_stop, Σ3⟧, ... = name_stop, ...,
    ξ_unstops+{name_stop→STOP(ξ_unstops(name_stop))}, ... = ξ_stops,
    expand_ph⟦flip_ph⟦stx, scp_i⟧, ξ_stops, Σ̂3⟧ = ⟨stx_exp, Σ̂4⟩

eval_ph⟦APP(lbinder, ast_id), scp_i, ξ, Σ̂⟧ = ⟨prune_ph⟦id_result, scp_u2⟧, Σ̂2⟩
  subject to eval_ph⟦ast_id, scp_i, ξ, Σ̂⟧ = ⟨id_result, Σ̂2⟩, Σ̂2 = ⟨_, _, scp_u2⟩
```

The implementation of `lexpand` uses a new `STOP(transform)` transformer to make an identifier a stopping point for expansion while remembering the former `transform` mapping of the identifier. The `unstop` helper function strips away a `STOP` constructor:

```text
unstop : transform -> transform

unstop⟦STOP(transform)⟧ = transform
unstop⟦transform⟧       = transform
```

The expander must recognize `STOP` transformers to halt expansion at that point:

```text
expand : ph stx ξ Σ̂ -> ⟨stx, Σ̂⟩

expand_ph⟦STX(LIST(id, stx, ...), ctx), ξ, Σ̂⟧ = ⟨STX(LIST(id, stx, ...), ctx), Σ̂⟩
  subject to Σ̂ = ⟨Σ, _, _⟩, ξ(resolve_ph⟦id, Σ⟧) = STOP(_)
```

The revised macro-application rule for `expand` shows how the use-site scopes component of `Σ̂` is updated and how the current application’s macro-introduction scope is passed to `eval`:

```text
expand_ph⟦stx_macapp, ξ, ⟨Σ, scp_p, scp_u⟩⟧ = ⟨stx_result, Σ̂5⟩
  subject to stx_macapp = STX(LIST(id_mac, stx_arg, ...), ctx),
    ξ(resolve_ph⟦id_mac, Σ⟧) = val, alloc-scope⟦Σ⟧ = ⟨scp_u_new, Σ2⟩,
    alloc-scope⟦Σ2⟧ = ⟨scp_i, Σ3⟩,
    ⟨Σ3, {scp_u_new} ∪ scp_p, {scp_u_new} ∪ scp_u⟩ = Σ̂3,
    flip_ph⟦add_ph⟦stx_macapp, scp_u_new⟧, scp_i⟧ = stx_macapp2,
    eval_ph⟦APP(val, stx_macapp2), scp_i, ξ, Σ̂3⟧ = ⟨stx_exp, Σ̂4⟩,
    expand_ph⟦flip_ph⟦stx_exp, scp_i⟧, ξ, Σ̂4⟧ = ⟨stx_result, Σ̂5⟩
```

In contrast, the revised `lambda` rule shows how the pruning scope set is extended for expanding the body of the function, the use-site scope set is reset to empty, and all extensions are discarded in the expansion’s resulting store tuple.

```text
expand_ph⟦STX(LIST(id_lam, id_arg, stx_body), ctx), ξ, ⟨Σ, scp_p, scp_u⟩⟧
  = ⟨STX(LIST(id_lam, id_new, stx_body2), ctx), ⟨Σ4, scp_p, scp_u⟩⟩
  subject to resolve_ph⟦id_lam, Σ⟧ = lambda,
    alloc-name⟦Σ⟧ = ⟨name_new, Σ1⟩,
    alloc-scope⟦Σ1⟧ = ⟨scp_new, Σ2⟩, add_ph⟦id_arg, scp_new⟧ = id_new,
    Σ2+{id_new→name_new} = Σ3,
    ξ+{name_new→VAR(id_new)} = ξ_new,
    ⟨Σ3, {scp_new} ∪ scp_p, ∅⟩ = Σ̂3,
    expand_ph⟦add_ph⟦stx_body, scp_new⟧, ξ_new, Σ̂3⟧ = ⟨stx_body2, ⟨Σ4, _, _⟩⟩
```

### 6.4 First-Class Definition Contexts

Supporting first-class definition contexts requires no further changes to the `expand` metafunction, but the `eval` metafunction must be extended to implement the `new-defs` and `def-bind` primitives, which model the `syntax-local-make-definition-context` and `syntax-local-bind-syntaxes` functions. The `new-defs` primitive allocates a new scope to represent the definition context, and it also allocates a mutable reference to a compile-time environment that initially references the current environment. The two pieces are combined with a `defs` value constructor:

```text
eval_ph⟦APP(new-defs), scp_i, ξ, ⟨Σ, scp_p, scp_u⟩⟧ = ⟨DEFS(scp_defs, addr), Σ̂3⟩
  subject to alloc-scope⟦Σ⟧ = ⟨scp_defs, Σ2⟩, alloc-def-env⟦Σ2⟧ = ⟨addr, Σ3⟩,
    ⟨Σ2+{addr→ξ}, {scp_defs} ∪ scp_p, scp_u⟩ = Σ̂3
```

The `def-bind` primitive works in two modes. In the first mode, it is given only a definition context and an identifier, and it creates a new binding for the identifier that includes the definition context’s scope. The new binding is mapped to variable in an updated environment for definition context:

```text
eval_ph⟦APP(def-bind, ast_defs, ast_id), scp_i, ξ, Σ̂⟧ = ⟨(), ⟨Σ6, scp_p3, scp_u3⟩⟩
  subject to eval_ph⟦ast_defs, scp_i, ξ, Σ̂⟧ = ⟨DEFS(scp_defs, addr), Σ̂2⟩,
    eval_ph⟦ast_id, scp_i, ξ, Σ̂2⟧ = ⟨id_arg, Σ̂3⟩, Σ̂3 = ⟨Σ3, scp_p3, scp_u3⟩,
    add_ph⟦prune_ph⟦flip_ph⟦id_arg, scp_i⟧, scp_u3⟧, scp_defs⟧ = id_defs,
    alloc-name⟦Σ3⟧ = ⟨name_new, Σ4⟩, Σ4+{id_defs→name_new} = Σ5,
    Σ5(addr) = ξ_defs,
    Σ5+{addr→ξ_defs+{name_new→VAR(id_defs)}} = Σ6
```

When `def-bind` is given an additional syntax object, it expands and evaluates the additional syntax object as a compile-time expression, and it adds a macro binding to the definition context’s environment:

```text
eval_ph⟦APP(def-bind, ast_defs, ast_id, ast_stx), scp_i, ξ, Σ̂⟧ = ⟨(), Σ̂9⟩
  subject to eval_ph⟦ast_defs, scp_i, ξ, Σ̂⟧ = ⟨DEFS(scp_defs, addr), Σ̂2⟩,
    eval_ph⟦ast_id, scp_i, ξ, Σ̂2⟧ = ⟨id_arg, Σ̂3⟩,
    eval_ph⟦ast_stx, scp_i, ξ, Σ̂3⟧ = ⟨stx_arg, Σ̂4⟩,
    Σ̂4 = ⟨Σ4, scp_p4, scp_u4⟩,
    add_ph⟦flip_ph⟦stx_arg, scp_i⟧, scp_defs⟧ = stx_arg2,
    expand_{ph+1}⟦stx_arg2, ξ_primitives, ⟨Σ4, ∅, ∅⟩⟧ = ⟨stx_exp, ⟨Σ5, _, _⟩⟩,
    eval_ph⟦parse_{ph+1}⟦stx_exp, Σ5⟧, scp_i, ξ, ⟨Σ5, scp_p4, ∅⟩⟧ = ⟨val_exp, Σ̂6⟩,
    Σ̂6 = ⟨Σ6, _, _⟩, Σ6(addr) = ξ_defs,
    add_ph⟦prune_ph⟦flip_ph⟦id_arg, scp_i⟧, scp_u4⟧, scp_defs⟧ = id_defs,
    alloc-name⟦Σ6⟧ = ⟨name_new, Σ7⟩, Σ7+{id_defs→name_new} = Σ8,
    ⟨Σ8+{addr→ξ_defs+{name_new→val_exp}}, scp_p4, scp_u4⟩ = Σ̂9
```

Note that `def-bind` in this mode defines a potentially recursive macro, since the definition context’s scope is added to compile-time expression before expanding and parsing it.

<!-- Page 12 -->

Finally, a definition context is used to expand an expression by providing the definition context as an extra argument to `lexpand`. The implementation of the new case for `lexpand` is similar to the old one, but the definition context’s scope is applied to the given syntax object before expanding it, and the definition context’s environment is used for expansion.

```text
eval_ph⟦APP(lexpand, ast_expr, ast_stops, ast_defs), scp_i, ξ, Σ̂⟧ = ⟨stx_exp2, Σ̂5⟩
  subject to eval_ph⟦ast_expr, scp_i, ξ, Σ̂⟧ = ⟨stx, Σ̂2⟩,
    eval_ph⟦ast_stops, scp_i, ξ, Σ̂2⟧ = ⟨LIST(id_stop, ...), Σ̂3⟩,
    eval_ph⟦ast_defs, scp_i, ξ, Σ̂3⟧ = ⟨DEFS(scp_defs, addr), Σ̂4⟩,
    Σ̂4 = ⟨Σ4, _, _⟩, Σ4(addr) = ξ_defs,
    {var↦unstop⟦ξ_defs(var)⟧ | var ∈ dom(ξ_defs)} = ξ_unstops,
    resolve_ph⟦id_stop, Σ4⟧, ... = name_stop, ...,
    ξ_unstops+{name_stop→STOP(ξ_unstops(name_stop))}, ... = ξ_stops,
    expand_ph⟦add_ph⟦flip_ph⟦stx, scp_i⟧, scp_defs⟧, ξ_stops, Σ̂4⟧ = ⟨stx_exp, Σ̂5⟩,
    flip_ph⟦stx_exp, scp_i⟧ = stx_exp2
```

## 7. Defining Hygiene

Most previous work on hygiene has focused on expansion algorithms, but some work addresses the question of what hygiene means independent of a particular algorithm. In his dissertation, Herman (2008) addresses the question through a type system that constrains and exposes the binding structure of macro expansions, so that α-renaming can be applied to unexpanded programs. More recently, Adams (2015) defines hygienic macro-expansion steps as obeying invariants that are expressed in terms of renaming via nominal logic (Pitts 2003), and the concept of equivariance plays an important role in characterizing hygienic macro transformers.

Since our notion of binding is based on scope sets instead of renaming, previous work on defining hygiene via renaming does not map directly into our setting. A related obstacle is that our model transforms a syntax object to a syntax object, instead of directly producing an AST; that difference is necessary to support local and partial expansion, which in turn is needed for definition contexts. A more technical obstacle is that we have specified expansion in terms of a meta-function (i.e., a big-step semantics) instead of as a rewriting system (i.e., a small-step semantics).

Adams’s approach to defining hygiene nevertheless seems applicable to our notion of binding. We leave a full exploration for future work, but we can offer an informed guess about how that exploration will turn out.

Although our model of expansion does not incorporate renaming as a core concept, if we make assumptions similar to Adams (including omitting the quote form), then a renaming property seems useful and within reach. For a given set of scopes and a point during expansion (exclusive of macro invocations), the symbol can be swapped in every identifier that has a superset of the given set of scopes; such a swap matches the programmer’s intuition that any variable can be consistently renamed within a binding region, which corresponds to a set of scopes.

Hygienic expansion then means that the result of the continued expansion after swapping is α-equivalent to what it would be without swapping. An individual transformer could be classified as hygienic based on all introduced identifiers having a fresh scope, so that they cannot bind any non-introduced identifiers; the fresh scope ensures an analog to Adams’s equivariance with respect to binders.

Note that swapping x with y for the scope set {adef, bintro1} would not produce an equivalent program for the expansion in section 3.5, because it would convert an ambiguous reference x{adef, bintro1, cintro2} to an unambiguous y{adef, bintro1, cintro2}. This failure should not suggest that the pattern-matching macros in that example are non-hygienic in themselves, but that the (implicit) definition-context macro is potentially non-hygienic. That is, a macro in a definition context can introduce an identifier that is captured at the macro-use site, since the definition and use sites can be the same. That potential for non-hygienic expansion appears to be one of the trade-offs of providing a context that allows a mixture of mutually recursive macro and variable definitions.

If macro bindings are constrained to letrec-syntax, and if macro implementations are constrained to use syntax-case, free-identifier=?, and syntax->datum (not bound-identifier=? or datum->syntax), then we expect that all expansion steps will be provably hygienic and all macro transformers will be provably hygienic by the definitions sketched above.

## 8. Other Related Work

While our work shares certain goals with techniques for representing resolved bindings, such as de Bruijn indices, higher-order abstract syntax (Pfenning and Elliott 1988), and nominal sets (Pitts 2013), those techniques seem to be missing a dimension that is needed to incrementally resolve bindings as introduced and manipulated by macros. Adams (2015) demonstrates how pairs of conventional identifiers provide enough of an extra dimension for hygienic macro expansion, but supporting datum->syntax would require the further extension of reifying operations on identifiers (in the sense of marks and renamings). Scope sets provide the additional needed dimension in a simpler way.

Scope graphs (Neron et al. 2015) abstract over program syntax to represent binding relationships—including support for constructs, such as modules and class bodies, that create static scopes different than nested lexical scopes. Binding resolution with macro expansion seems more dynamic, in that a program and its binding structure evolve during expansion; up-front scope graphs are not clearly applicable. Scope sets, meanwhile, do not explicitly represent import relationships, relying on macros that implement modular constructs to create scopes and bindings that reflect the import structure. Further work on scope graphs and scope sets seems needed to reveal the connections.

Stansifer and Wand (2014) build on the direction of Herman (2008) with Romeo, which supports program manipulations that respect scope by requiring that every transformer’s type exposes its effect on binding. The resulting language is more general than Herman’s macros, but transformers are more constrained than hygienic macros in Scheme and Racket.

## 9. Conclusion

Hygienic macro expansion is a successful, decades-old technology in Racket and the broader Scheme community. Hygienic macros have also found a place in some other languages, but the difficulties of specifying hygiene, understanding macro scope, and implementing a macro expander have surely been an obstacle to the broader adoption and use of hygienic macros. Those obstacles, in turn, suggest that our models of macro expansion have not yet hit the mark. Scope sets are an attempt to move the search for a model of macro expansion to a substantially different space, and initial results with Racket and JavaScript show that this new space is promising.

## Acknowledgments

Thanks to Matthias Felleisen, Robby Findler, Sam Tobin-Hochstadt, Jay McCarthy, Ryan Culpepper, Michael Adams, Michael Ballantyne, Tim Disney, Simon Peyton Jones, Shriram Krishnamurthi, and Eelco Visser for discussions about set-of-scopes binding. Joshua Grams provided helpful editing suggestions. Anthony Carrico and Chris Brooks helpfully pointed out some typos. Thanks also to anonymous POPL reviewers for helping to improve the presentation. This work was supported by grants from the NSF.

<!-- Page 13 -->

## References

Michael D. Adams. Towards the Essence of Hygiene. In Proc. Principles of Programming Languages, 2015.

Alan Bawden and Jonathan Rees. Syntactic Closures. In Proc. Lisp and Functional Programming, 1988.

William Clinger and Jonathan Rees. Macros that Work. In Proc. Principles of Programming Languages, 1991.

William D. Clinger. Hygienic Macros Through Explicit Renaming. Lisp Pointers 4(4), 1991.

Ryan Culpepper and Matthias Felleisen. Debugging Hygienic Macros. Science of Computer Programming 75(7), 2010.

Tim Disney et al. Sweet.js. 2015. http://sweetjs.org/

Tim Disney, Nathan Faubion, David Herman, and Cormac Flanagan. Sweeten your JavaScript: Hygienic Macros for ES5. In Proc. Symposium on Dynamic Languages, 2014.

R. Kent Dybvig, Robert Hieb, and Carl Bruggeman. Syntactic Abstraction in Scheme. Lisp and Symbolic Computation 5(4), 1993.

Matthew Flatt. Compilable and Composable Macros, You Want it When? In Proc. International Conference on Functional Programming, 2002.

Matthew Flatt. Submodules in Racket: You Want it When, Again? In Proc. Generative Programming: Concepts and Experiences, 2013.

Matthew Flatt, Ryan Culpepper, Robert Bruce Findler, and David Darais. Macros that Work Together: Compile-Time Bindings, Partial Expansion, and Definition Contexts. Journal of Functional Programming 22(2), 2012.

David Herman. Dissertation. PhD dissertation, Northeastern University, 2008.

Eugene Kohlbecker, Daniel P. Friedman, Matthias Felleisen, and Bruce Duba. Hygienic Macro Expansion. In Proc. Lisp and Functional Programming, 1986.

Eugene E. Kohlbecker and Mitchell Wand. Macro-by-Example: Deriving Syntactic Transformations from their Specifications. In Proc. Principles of Programming Languages, 1987.

Pierre Neron, Andrew P. Tolmach, Eelco Visser, and Guido Wachsmuth. A Theory of Name Resolution. In Proc. European Symposium on Programming, 2015.

Frank Pfenning and Conal Elliott. Higher-order Abstract Syntax. In Proc. Programming Language Design and Implementation, 1988.

Andrew M. Pitts. Nominal Logic, a First Order Theory of Names and Binding. Information and Computation 186(2), 2003.

Andrew M. Pitts. Nominal Sets: Names and Symmetry in Computer Science. Cambridge University Press, 2013.

Paul Stansifer and Mitch Wand. Romeo: a System for More Flexible Binding-Safe Programming. In Proc. International Conference on Functional Programming, 2014.
