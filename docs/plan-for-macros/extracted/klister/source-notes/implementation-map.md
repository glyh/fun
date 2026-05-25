# Klister implementation map

This directory intentionally does not preserve the full Klister source tree. It keeps only the design commentary, representative examples, and this map of implementation ideas worth revisiting.

## Syntax objects

Original source: `klister/src/Syntax.hs`

Important ideas:

- `Syntax` is the user-facing syntax produced by parsing or macros and consumed by expansion.
- Syntax nodes carry source location and scope-set information.
- Scope operations are phase-indexed:
  - `addScope : Phase -> Scope -> a -> a`
  - `removeScope : Phase -> Scope -> a -> a`
  - `flipScope : Phase -> Scope -> a -> a`
  - universal variants apply across phases.
- Quoted/generated syntax must preserve or adjust scopes deliberately.

Relevant excerpt:

```haskell
addScope :: HasScopes a => Phase -> Scope -> a -> a
addScope p = adjustScope (ScopeSet.insertAtPhase p)

removeScope :: HasScopes a => Phase -> Scope -> a -> a
removeScope p = adjustScope (ScopeSet.deleteAtPhase p)

flipScope :: HasScopes a => Phase -> Scope -> a -> a
flipScope p = adjustScope go
  where
    go sc scs
      | ScopeSet.member p sc scs = ScopeSet.deleteAtPhase p sc scs
      | otherwise                = ScopeSet.insertAtPhase p sc scs
```

## Scope sets

Original source: `klister/src/ScopeSet.hs`

Important ideas:

- A scope set has universal scopes and phase-specific scopes.
- Identifier resolution is based on subset checks at the current phase.
- Scopes can be shifted when modules are visited at another phase.
- There is a traversal for updating every scope set inside a larger syntax/core structure.

Relevant excerpt:

```haskell
data ScopeSet = ScopeSet
  { _universalScopes :: Set Scope
  , _phaseScopes     :: Store Phase (Set Scope)
  }

scopes :: Phase -> ScopeSet -> Set Scope
scopes p scs = view universalScopes scs `Set.union`
               view (phaseScopes . at p . non Set.empty) scs

isSubsetOf :: Phase -> ScopeSet -> ScopeSet -> Bool
isSubsetOf p scs1 scs2 =
  Set.isSubsetOf (scopes p scs1) (scopes p scs2)

instance Phased ScopeSet where
  shift j = over phaseScopes $ St.mapKeys (+ Phase j)
```

## Phases

Original source: `klister/src/Phase.hs`

Important ideas:

- Runtime is phase 0.
- Compile-time/import-for-syntax behavior is modeled by shifting phases.
- Phase shifting applies recursively to phase-bearing structures.

Relevant excerpt:

```haskell
newtype Phase = Phase { phaseNum :: Natural }

runtime :: Phase
runtime = Phase 0

prior :: Phase -> Phase
prior (Phase i) = Phase (i + 1)

class Phased a where
  shift :: Natural -> a -> a
```

## Expansion entrypoints and module visiting

Original source: `klister/src/Expander.hs`

Important ideas:

- `expandExpr` creates both a core expansion destination and a type metavariable, then forks expansion tasks and drains the task queue.
- Module expansion resets module-local binding/type state, initializes the language, adds module scopes, expands declarations, then caches expanded output.
- Visiting a module loads it if needed, shifts it to the current phase, evaluates it at that phase, and installs exported bindings.
- Imports/exports can be shifted by phase.

Relevant excerpt:

```haskell
expandExpr :: Syntax -> Expand SplitCore
expandExpr stx = do
  dest <- liftIO $ newSplitCorePtr
  t <- tMetaVar <$> freshMeta (Just KStar)
  forkExpandSyntax (ExprDest t dest) stx
  expandTasks
  children <- view expanderCompletedCore <$> getState
  patterns <- view expanderCompletedPatterns <$> getState
  typePatterns <- view expanderCompletedTypePatterns <$> getState
  return $ SplitCore { _splitCoreRoot = dest
                     , _splitCoreDescendants = children
                     , _splitCorePatterns = patterns
                     , _splitCoreTypePatterns = typePatterns
                     }
```

```haskell
visit :: ModuleName -> Expand Exports
visit modName = do
  ...
  p <- currentPhase
  let i = phaseNum p
  ...
  let m' = shift i m
  sc <- freshScope ...
  let m'' = over ScopeSet.allScopeSets (ScopeSet.insertUniversally sc) m'
  evalResults <- inPhase p $ evalMod m''
  ...
  return (shift i es)
```

## Extracted examples to keep

- `examples/which-problem.kl` — one macro expands differently as declaration/type/expression/pattern; includes expected-type-driven expansion.
- `examples/type-eq.kl` — type-case over type structure and macro-level type equality.
- `examples/datatype-macro.kl` — datatype definition as macro-level abstraction.
- `examples/hygiene.kl` — captures hygiene expectations.
- `examples/bound-identifier.kl` — bound-identifier comparison.
- `examples/free-identifier-case.kl` — free-identifier comparison and identifier-sensitive syntax dispatch.
- `examples/implicit-conversion.kl` — type-directed code generation.
- `examples/custom-module.kl` — macro-defined/custom module behavior.
- `examples/meta-macro.kl` — macros that produce macro-related bindings.
- `examples/quasiquote-syntax-test.kl` — syntax construction and splicing.
