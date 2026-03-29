# Marmoset vNext Syntax Spec

This document has two layers:
- Part A is a reader-friendly language guide.
- Part B is a strict, parser-oriented normative reference.

## Quick Navigation
- Guide start: [Part A. Language Guide](#part-a-language-guide-non-normative)
- Normative start: [Part B. Compiler Reference](#part-b-compiler-reference-normative)
- Parser grammar: [13.2 Concrete Grammar (EBNF)](#132-concrete-grammar-ebnf)
- Desugaring pipeline: [13.5 Desugaring Order (Normative)](#135-desugaring-order-normative)

## Part A. Language Guide (Non-Normative)

## 1. Scope And Status
This is the language-level syntax and static-semantics spec for Marmoset vNext.

It specifies the target vNext language surface only.
The compiler accepts only the syntax described here. Historical rollout notes live in `docs/plans/done/language/02_syntax-rework.md`.

It defines:
- Surface syntax.
- Canonical casing conventions.
- Parsing/desugaring rules.
- Trait/impl/derive behavior.
- Match and lambda shorthand behavior.

It does not fully define:
- Runtime model.
- Module/import system.
- Full operator precedence table beyond what is needed here.

## 2. Lexical Conventions
- Keywords are lowercase: `type`, `enum`, `trait`, `impl`, `fn`, `let`, `derive`, `override`, `if`, `else`, `match`, `case`.
- Type/trait/enum names use `UpperCamel`: `User`, `Result`, `Named`, `Eq`.
- Primitive types follow the same convention: `Int`, `Str`, `Bool` (and other primitives should follow this style).
- Built-in generic type constructors follow the same convention: `List`, `Map`, `Option`, `Result`.
- Generic type parameters are lowercase: `a`, `e`, `t0`.
- Value/function identifiers are typically lowercase/snake_case.
- `?` and `!` are allowed in identifiers (for example `admin?`, `panic!`).
- Comments start with `#` and run to end of line.
- Semicolons are optional separators accepted by the language surface, but canonical vNext formatting omits them.

## 3. Top-Level Shape (EBNF-Lite)
```ebnf
Program        ::= { Decl }

Decl           ::= TypeDecl | AliasDecl | ShapeDecl | EnumDecl | TraitDecl | ImplDecl | FnDecl | LetDecl

TypeDecl       ::= "type" TypeName [TypeParams] "=" TypeExpr [DeriveClause]
AliasDecl      ::= "alias" TypeName [TypeParams] "=" TypeExpr
ShapeDecl      ::= "shape" TypeName [TypeParams] "=" "{" {RecordTypeField} "}"
EnumDecl       ::= "enum" TypeName [TypeParams] "=" "{" [EnumVariants] "}" [DeriveClause]
TraitDecl      ::= "trait" TraitName "[" TypeParam "]" [":" ConstraintExpr] "=" "{" {TraitMember} "}"
ImplDecl       ::= "impl" [GenericParams] (TraitName "[" TypeExpr "]" | TypeExpr) "=" "{" {ImplMember} "}"
FnDecl         ::= "fn" Ident [GenericParams] "(" [FnParams] ")" [FnSigSuffix] "=" ExprOrBlock
LetDecl        ::= "let" Ident [":" TypeExpr] "=" Expr

TypeParams     ::= "[" TypeParam {"," TypeParam} "]"
TypeParam      ::= LowerIdent
GenericParams  ::= "[" GenericParam {"," GenericParam} "]"
GenericParam   ::= LowerIdent [":" ConstraintExpr]

DeriveClause   ::= "derive" TraitName {"," TraitName}
FnSigSuffix    ::= PurityArrow TypeExpr

ConstraintExpr ::= ConstraintRef {"&" ConstraintRef}
ConstraintRef  ::= TraitName | TypeName

PurityArrow    ::= "->" | "=>"
ExprOrBlock    ::= Expr | Block
```

## 4. Types And Declarations
- Uniform declaration form is `name = value/form`.
- Function types use shorthand:
  - Pure: `(a, b) -> c`
  - Effectful: `(a, b) => c`
- Top-level `fn` declarations and impl methods may omit the purity/return suffix. When omitted, the compiler infers both from the body.
- `alias` is transparent and behaviorless.
- `type` is nominal and requires explicit construction for products and wrappers.
- `shape` introduces structural field constraints.
- Union types use `|`: `Int | Str`.
- Intersection types use `&`: `A & B`.
- `&` binds tighter than `|` in type expressions.
- In parameter position, an unparenthesized bare trait-name chain such as `Named & Aged` still means constrained-param shorthand, not a general intersection. Parenthesize to force a real intersection there.
- Built-in collection type constructors follow the same casing convention: `List[Int]`, `Map[Str, Int]`.
- Record type fields are comma-separated, trailing comma allowed.
- Enum variants are comma-separated, trailing comma allowed.
- Derive is postfix on `type`/`enum` declarations only.

Examples:
```mr
alias UserId = Int
alias Reducer[a] = (a, a) -> a
shape Named = { name: Str }

enum Result[a, e] = {
  Success(a),
  Failure(e),
} derive Eq, Show
```

## 5. Traits, Constraints, And Impl
### 5.1 Trait Declarations
- Trait constraints compose with `&`: `trait Greeter[a]: Show & Named = { ... }`.
- Trait bodies are method-only.
- Structural field requirements belong in `shape` declarations.
- Required methods have signatures only.
- Default methods include a body.
- In signature-only trait methods, positional typed params are allowed (for example `fn eq(a, a) -> Bool`).
- In default methods, named params are used when referenced (for example `self: a`).

### 5.2 Impl Forms
- Trait impl core form: `impl[a: Show] Show[List[a]] = { ... }`
- Inherent impl core form: `impl[a] List[a] = { ... }`
- Unconstrained impl binders may be omitted when they can be inferred from free type variables in the impl target:
  - `impl Show[List[a]] = { ... }`
  - `impl Result[a, e] = { ... }`
- Impl methods use explicit `self`.

### 5.3 Override Rule
- Replacing a trait default method requires `override`.
- Using `override` on a required (non-default) method is allowed but should emit a warning.
- Omitting `override` while replacing a default method is an error.

## 6. Derive Semantics
- Built-in derivable traits include `Eq`, `Show`, `Debug`, `Ord`, `Hash` (project policy may expand).
- Built-in derive remains part of the vNext surface syntax and uses specialized builtin synthesis.
- User-trait derive v1 is default-backed only:
  - every trait method must have a default body after type substitution,
  - traits with any required method are not derivable in this version unless they are one of the builtin derivable traits above,
  - shapes are not derivable.
- Supertraits are not auto-derived implicitly from trait definitions alone. A derive request succeeds only if every supertrait is already implemented for the target type or appears in the same derive clause.
- Derive failure cases are deterministic:
  - undefined trait,
  - trait not derivable,
  - shape derive is redundant/unsupported,
  - missing supertrait implementation or derive closure,
  - required method without default body,
  - duplicate explicit or duplicate derived impl,
  - default body typecheck failure after substitution.

## 7. Function Parameters And Constraint Shorthand
### 7.1 Explicit Generic Constraint
```mr
shape Named = { name: Str }

fn display_name[a: Named](x: a) -> Str = x.name
```

### 7.2 Constrained-Param Shorthand
```mr
fn display_name2(x: Named) -> Str = x.name
```
desugars to:
```mr
fn display_name2[t0: Named](x: t0) -> Str = x.name
```

### 7.3 Multiple Shorthand Params Are Independent
```mr
fn always_true(x: Eq, y: Eq) -> Bool = true
```
desugars to:
```mr
fn always_true[t0: Eq, t1: Eq](x: t0, y: t1) -> Bool = true
```

### 7.4 Same-Type Tie Requires Explicit Generic
```mr
fn same_eq[t: Eq](x: t, y: t) -> Bool = x.eq(y)
```

### 7.5 Bare Constraint Names
- In function parameter position, bare trait or shape names are constrained-param shorthand.
- Bare constraint names are not general type expressions in vNext.
- Use `Dyn[...]` for trait-object types. Bare constraint names never mean trait-object types.

### 7.6 Explicit Trait Objects
```mr
let value: Dyn[Show] = 42
let items: List[Dyn[Show]] = [42, "hello", true]
```

- `Dyn[ConstraintExpr]` is the explicit trait-object type form.
- `Dyn[...]` accepts trait constraints only. Shapes are rejected inside `Dyn[...]`.
- Concrete values coerce to `Dyn[...]` only at type-directed sites:
  - assignment,
  - argument passing,
  - return position.
- Method dispatch on `Dyn[...]` values is dynamic and requires no manual wrapping or unwrapping.
- `Dyn[...]` does not replace structural field projection; shapes remain compile-time structural constraints outside trait objects.

## 8. Expressions And Blocks
- Blocks are braced: `{ ... }`.
- A block evaluates to its last expression value.
- `let` introduces bindings in block scope.
- `if` is an expression and both branches must type-check to a compatible type.

## 9. Match
Canonical form:
```mr
match <expr> {
  case <pattern>: <expr-or-block>
  ...
}
```

- `match` is an expression.
- Arms are written with `case`.
- Arm body can be single expression or block.
- No `match x = { ... }` form in vNext.

Examples:
```mr
fn role_to_string(r: Role) -> Str = match r {
  case Role.User: "user"
  case Role.Admin: "admin"
}

fn classify_role(r: Role) -> Str = match r {
  case Role.User: "regular"
  case Role.Admin: {
    let label = "privileged"
    label
  }
}
```

## 10. Pattern Syntax
Supported pattern classes in this spec:
- Wildcard: `_`
- Constructor pattern: `Option.Some(x)`
- Record pattern with punning: `{ name:, ...rest }`
- Or-patterns: `P1 | P2`

Notes:
- `name:` binds field `name` into local variable `name`.
- `...rest` binds the remaining fields (record pattern contexts only).

## 11. Lambda Syntax
### 11.1 Explicit Lambdas
- Pure: `(x) -> expr`, `(a, b) -> expr`
- Effectful: `(x) => expr`, `(a, b) => expr`

### 11.2 Placeholder Shorthand
- Single-arg pure shorthand only.
- `_` can appear in nested expression positions.

Examples:
```mr
ids.map(_ * 2)                    # x -> x * 2
users.map(_.name)                 # x -> x.name
ids.map(do_something(_))          # x -> do_something(x)
names.map(trim(lowercase(_)))     # x -> trim(lowercase(x))
users.filter(is_admin?(_))        # x -> is_admin?(x)
```

Restrictions:
- Expression must contain exactly one placeholder `_`.
- Expressions with zero placeholders remain ordinary expressions and are not rewritten.
- Multi-placeholder forms like `_ + _` are rejected.
- Effectful callbacks require explicit `=>` lambda (no placeholder effectful form).

## 12. Canonical Example (Condensed)
```mr
alias UserId = Int
alias Reducer[a] = (a, a) -> a

enum Role = {
  User,
  Admin,
}

enum Result[a, e] = {
  Success(a),
  Failure(e),
}

trait Eq[a] = {
  fn eq(a, a) -> Bool
}

shape Named = {
  name: Str,
}

trait Account[a]: Eq & Named = {
  fn id(a) -> UserId
  fn full_name(self: a) -> Str = self.name
}

type User = {
  id: UserId,
  name: Str,
  admin: Bool,
} derive Eq

impl Account[User] = {
  fn id(self: User) -> UserId = self.id
  override fn full_name(self: User) -> Str = self.name
}

fn label_user(x: Account) -> Str = x.full_name()

fn role_to_string(r: Role) -> Str = match r {
  case Role.User: "user"
  case Role.Admin: "admin"
}
```

## Part B. Compiler Reference (Normative)
Parser/typechecker implementation should treat this part as source-of-truth when it differs from examples in Part A.

## 13. Strict Parser-Ready Addendum
This section is normative for parser implementation.

### 13.1 Tokens And Lexemes
- `TypeName`, `TraitName`, `EnumName`, `CtorName`, and built-in generic type constructors are `UpperCamel` identifier tokens.
- Primitive types are also `UpperCamel` (`Int`, `Str`, `Bool`).
- `LowerIdent` is a lowercase identifier token used for generic parameters and inferred binders (`a`, `e`, `t0`).
- `Ident` is a lowercase/snake_case value identifier token and may include suffix `?` or `!`.
- `Literal` covers integer, float, string, and boolean literals.
- `StringLit` is the string-literal subset of `Literal`.
- Keywords: `type`, `enum`, `trait`, `impl`, `fn`, `let`, `derive`, `override`, `if`, `else`, `match`, `case`, `true`, `false`.
- Punctuation/operators used here: `=`, `:`, `,`, `;`, `.`, `...`, `(`, `)`, `{`, `}`, `[`, `]`, `->`, `=>`, `|`, `&`.
- `_` has context-sensitive meaning:
  - pattern wildcard in pattern position,
  - lambda placeholder in expression position.
- Optional semicolons may appear after top-level declarations and block statements/expressions; they are non-semantic separators and are omitted from the EBNF below for brevity.

### 13.2 Concrete Grammar (EBNF)
Lexical token classes from 13.1 appear as terminals in the EBNF below.

```ebnf
Program              ::= { TopDecl }

TopDecl              ::= TypeDecl
                       | AliasDecl
                       | ShapeDecl
                       | EnumDecl
                       | TraitDecl
                       | ImplDecl
                       | FnDecl
                       | LetDecl
                       | ExprStmt

TypeDecl             ::= "type" TypeName [TypeParams] "=" TypeExpr [DeriveClause]
AliasDecl            ::= "alias" TypeName [TypeParams] "=" TypeExpr
ShapeDecl            ::= "shape" TypeName [TypeParams] "=" "{" [RecordTypeFieldList] "}"

EnumDecl             ::= "enum" TypeName [TypeParams] "=" "{" [EnumVariantList] "}" [DeriveClause]
EnumVariantList      ::= EnumVariant { "," EnumVariant } [","]
EnumVariant          ::= CtorName [ "(" [TypeExprList] ")" ]

TraitDecl            ::= "trait" TraitName "[" TypeParam "]" [":" ConstraintExpr] "=" "{" { TraitMember } "}"
TraitMember          ::= TraitReqMethod | TraitDefaultMethod
TraitReqMethod       ::= "fn" Ident "(" [TraitSigParamList] ")" PurityArrow TypeExpr
TraitDefaultMethod   ::= "fn" Ident "(" [FnParamList] ")" PurityArrow TypeExpr "=" ExprOrBlock

ImplDecl             ::= "impl" [GenericParams] ImplTarget "=" "{" { ImplMember } "}"
ImplTarget           ::= TraitName "[" TypeExpr "]" | TypeExpr
ImplMember           ::= [ "override" ] "fn" Ident "(" [FnParamList] ")" [FnSigSuffix] "=" ExprOrBlock

FnDecl               ::= "fn" Ident [GenericParams] "(" [FnParamList] ")" [FnSigSuffix] "=" ExprOrBlock
LetDecl              ::= "let" Ident [ ":" TypeExpr ] "=" Expr

DeriveClause         ::= "derive" TraitName { "," TraitName }

TypeParams           ::= "[" TypeParam { "," TypeParam } "]"
TypeParam            ::= LowerIdent
GenericParams        ::= "[" GenericParam { "," GenericParam } "]"
GenericParam         ::= LowerIdent [ ":" ConstraintExpr ]
FnSigSuffix          ::= PurityArrow TypeExpr

FnParamList          ::= FnParam { "," FnParam }
FnParam              ::= Ident ":" TypeExpr

TraitSigParamList    ::= TraitSigParam { "," TraitSigParam }
TraitSigParam        ::= TypeExpr | Ident ":" TypeExpr

RecordType           ::= "{" [RecordFieldList] [","] "}"
RecordFieldList      ::= RecordField { "," RecordField }
RecordField          ::= Ident ":" TypeExpr

TypeExprList         ::= TypeExpr { "," TypeExpr }
ArgList              ::= Expr { "," Expr }
ExprList             ::= Expr { "," Expr }
PatternList          ::= Pattern { "," Pattern }

PurityArrow          ::= "->" | "=>"
ExprOrBlock          ::= Expr | Block

TypeExpr             ::= TypeUnion
TypeUnion            ::= TypeIntersection { "|" TypeIntersection }
TypeIntersection     ::= TypePrimary { "&" TypePrimary }
TypePrimary          ::= TypeName
                       | TypeName "[" TypeExprList "]"
                       | "Dyn" "[" ConstraintExpr "]"
                       | LowerIdent
                       | RecordType
                       | "(" [TypeExprList] ")" PurityArrow TypeExpr
                       | "(" TypeExpr ")"

ConstraintExpr       ::= ConstraintRef { "&" ConstraintRef }
ConstraintRef        ::= TraitName | TypeName

Block                ::= "{" { Stmt } [Expr] "}"
Stmt                 ::= LetDecl
                       | ExprStmt
ExprStmt             ::= Expr

Expr                 ::= MatchExpr | IfExpr | ExplicitLambda | OrExpr

IfExpr               ::= "if" "(" Expr ")" ExprOrBlock "else" ExprOrBlock
MatchExpr            ::= "match" Expr "{" { CaseArm } "}"
CaseArm              ::= "case" Pattern ":" ExprOrBlock

ExplicitLambda       ::= "(" [LambdaParamList] ")" PurityArrow ExprOrBlock
LambdaParamList      ::= LambdaParam { "," LambdaParam }
LambdaParam          ::= Ident [ ":" TypeExpr ]

OrExpr               ::= AndExpr { "||" AndExpr }
AndExpr              ::= EqExpr { "&&" EqExpr }
EqExpr               ::= RelExpr { ("==" | "!=") RelExpr }
RelExpr              ::= AddExpr { ("<" | "<=" | ">" | ">=" | "is") AddExpr }
AddExpr              ::= MulExpr { ("+" | "-") MulExpr }
MulExpr              ::= UnaryExpr { ("*" | "/" | "%") UnaryExpr }
UnaryExpr            ::= [("!" | "-")] PostfixExpr
PostfixExpr          ::= PrimaryExpr { CallSuffix | MemberSuffix | IndexSuffix }
CallSuffix           ::= "(" [ArgList] ")"
MemberSuffix         ::= "." Ident
IndexSuffix          ::= "[" Expr "]"

PrimaryExpr          ::= Literal
                       | Ident
                       | "_"
                       | "(" Expr ")"
                       | ArrayExpr
                       | MapOrRecordExpr
                       | Block

ArrayExpr            ::= "[" [ExprList] "]"
MapOrRecordExpr      ::= "{" MapOrRecordItems [","] "}"
MapOrRecordItems     ::= MapOrRecordItem { "," MapOrRecordItem }
MapOrRecordItem      ::= "..." Expr
                       | Ident ":"
                       | Ident ":" Expr
                       | Expr ":" Expr

Pattern              ::= OrPattern
OrPattern            ::= PrimaryPattern { "|" PrimaryPattern }
PrimaryPattern       ::= "_"
                       | Literal
                       | CtorPattern
                       | RecordPattern
                       | "(" Pattern ")"
CtorPattern          ::= TypeName "." CtorName [ "(" [PatternList] ")" ]
RecordPattern        ::= "{" [RecordPatternItems] [","] "}"
RecordPatternItems   ::= RecordPatternItem { "," RecordPatternItem }
RecordPatternItem    ::= Ident ":"
                       | Ident ":" Pattern
                       | "..." Ident
```

### 13.3 Disambiguation Rules
- `x: Named` in function parameter position is parsed as a type annotation. Whether `Named` refers to a trait (constraint shorthand) or a concrete type is resolved during lowering/typechecking, not at parse time. The parser remains syntax-only.
- `x: Show & Eq` is always a constrained-param shorthand (the `&` operator is unambiguous).
- Bare trait names are not valid general type expressions in vNext.
- Field-only trait-as-type forms from previous syntax are not part of vNext.
- Record/map literal split uses key form:
  - `{"k": v}`, `{1: v}`, `{true: v}`, and `{false: v}` are map-like.
  - `{name: expr}` or `{name:}` or `{...base}` is record-like.
- Empty braces in expression position parse as an empty block, not as a map/record literal.
- `_` in expression position is parsed as a placeholder-lambda candidate. Validation occurs in desugaring.

### 13.4 Operator Precedence And Associativity
From highest to lowest:
1. Postfix: call `()`, member `.`, index `[]` (left-associative)
2. Prefix: unary `!`, unary `-`
3. Multiplicative: `* / %` (left-associative)
4. Additive: `+ -` (left-associative)
5. Relational: `< <= > >= is` (non-associative by default; parser may chain then reject/typecheck)
6. Equality: `== !=` (left-associative)
7. Logical and: `&&` (left-associative)
8. Logical or: `||` (left-associative)
9. Match/if/lambda forms bind at expression level and are parsed by keyword lead-in.

### 13.5 Desugaring Order (Normative)
The frontend should apply these rewrites in this order:
1. Resolve symbols (distinguish type vs trait names for shorthand).
2. Infer unconstrained impl binders from free type variables in impl targets.
3. Expand constrained-param shorthand in function declarations.
4. Expand placeholder lambdas.
5. Validate purity arrows (`->` pure, `=>` effectful).
6. Lower built-in `derive` into synthesized impl obligations.
7. Enforce `override` rules.

### 13.6 Impl Binder Inference
- If an impl declaration omits an explicit binder list, every free lowercase type variable appearing in the impl target is lifted into an unconstrained binder in first-occurrence order.

Examples:
```mr
impl Show[List[a]] = { ... }
```
becomes:
```mr
impl[a] Show[List[a]] = { ... }
```

```mr
impl Result[a, e] = { ... }
```
becomes:
```mr
impl[a, e] Result[a, e] = { ... }
```

Rules:
- Constraint-bearing binders must be explicit.
- Inference applies only to unconstrained binders.

### 13.7 Constrained-Param Shorthand Rewrite
- Input:
```mr
fn f(x: Named) -> Str = x.full_name()
```
- Output:
```mr
fn f[t0: Named](x: t0) -> Str = x.full_name()
```

Rules:
- Each shorthand parameter gets a fresh generic variable.
- Multiple shorthand params are independent unless explicitly tied by user-written generic.
- Composition is preserved:
```mr
fn g(x: Show & Eq) -> Str = ...
```
becomes:
```mr
fn g[t0: Show & Eq](x: t0) -> Str = ...
```

### 13.8 Placeholder Lambda Rewrite
Rule:
- If expression `E` contains exactly one placeholder `_` and no explicit lambda arrow, rewrite to `(it) -> E[it/_]` with fresh `it`.

Examples:
```mr
_ * 2                 => (it) -> it * 2
_.name                => (it) -> it.name
do_something(_)       => (it) -> do_something(it)
trim(lowercase(_))    => (it) -> trim(lowercase(it))
```

Zero-placeholder expressions are left unchanged.

Errors:
- Any expression containing more than one placeholder is rejected.
- Effectful intent with placeholder is not inferred. Use explicit `=>` lambda.

### 13.9 Trait/Impl/Derive Validation Rules
- Trait impl target shape is `impl [GenericParams] Trait[Type] = { ... }`.
- Inherent impl target shape is `impl [GenericParams] Type = { ... }`.
- Unconstrained impl binders may be inferred from free type variables in the impl target.
- If impl method replaces a trait default method, `override` is mandatory.
- If `override` is used on required/non-default method, emit warning.
- Built-in `derive` synthesizes builtin trait behavior per project policy.
- User-trait derive v1 is default-backed only:
  - every trait method must have a default body after substitution,
  - shapes are rejected,
  - duplicate explicit/derived impls are rejected,
  - supertraits must already be implemented for the target type or appear in the same derive clause,
  - derive planning is graph-driven rather than source-order-sensitive.

### 13.10 Trait-Object Validation Rules
- `Dyn[ConstraintExpr]` is the explicit trait-object type form.
- The enclosed `ConstraintExpr` uses the same trait-bound grammar as generic constraints.
- Every member inside `Dyn[...]` must be a trait.
- `Dyn[Named]` where `Named` is a shape is an error.
- `Dyn[Show & Named]` is also an error.
- Coercion from a concrete value to `Dyn[...]` happens only at type-directed sites:
  - assignment,
  - argument passing,
  - return position.
- Method dispatch on `Dyn[...]` values is dynamic.

### 13.11 Match Validation Rules
- `match` is always an expression.
- Each arm is `case <pattern>: <expr-or-block>`.
- Arm result types must unify.
- Exhaustiveness checking is required for closed enums and should warn/error for non-exhaustive matches per project policy.

### 13.12 Intersection Validation Rules
- `TypeExpr & TypeExpr` is a compile-time-only type form. It does not introduce a runtime wrapper.
- Canonical normalization flattens nested intersections, sorts members deterministically, and removes duplicates.
- If an intersection contains multiple `Dyn[...]` members, they merge into one `Dyn[...]` whose trait set is the union of those members.
- `Dyn[...]` members may not be mixed with non-`Dyn[...]` members inside the same intersection in v1.
- Subtyping follows meet-style rules:
  - `X <: A & B` iff `X <: A` and `X <: B`.
  - `A & B <: X` iff the intersection is usable anywhere one of its members is usable.
- Field access on an intersection-typed value is valid only when every member guarantees that field.
- General callable intersections are rejected in v1 unless normalization leaves a single callable type.
- Constraint grammar does not change:
  - generic constraints,
  - trait supertraits,
  - constrained-param shorthand,
  - `Dyn[...]`
  still use `ConstraintExpr`, not general type intersections.
