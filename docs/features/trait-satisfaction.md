Below is a detailed spec you can hand to Codex. It’s written to be implementable and reviewable: it defines what “trait satisfaction” means, the exact rules for field traits vs method traits vs mixed traits, how constraints are checked, how method calls resolve, how dynamic “trait as type” works, and what to forbid to avoid TS-style structural method madness.

⸻

Marmoset Trait Satisfaction Spec (v1)

Goal: Provide Rust/Scala-with-GC semantics:
	•	structural records + row-like field constraints are allowed,
	•	ad-hoc polymorphism for methods/operators is nominal and explicit,
	•	no TS-style “if it has the right methods it implements the interface” inference.

This spec defines:
	1.	Trait kinds (Field-only, Method-only, Mixed)
	2.	Satisfaction rules (structural vs nominal)
	3.	Coherence / collisions
	4.	Constraint checking
	5.	Method resolution rules
	6.	Trait objects (trait as a type) semantics and codegen contracts
	7.	Implementation plan / checks for Codex

⸻

0. Terminology
	•	Record type: structural product type { f1: T1, f2: T2, ... } (may be named alias).
	•	Enum/ADT type: nominal sum type enum ....
	•	Trait: a named set of requirements, which may include:
	•	field requirements: name: string
	•	method requirements: fn show(x: a) -> string (trait-param-based)
	•	Impl: a declared implementation impl Trait[...] for Type { ... } providing method bodies (and optionally field confirmation).
	•	Canonical shape: deterministic representation of a record type used for hashing/equality.
	•	Trait satisfaction: “Type T satisfies trait Tr” (for constraints, method calls, operator desugaring, and trait objects).
	•	Constraint context: a generic parameter constraint like [t: Tr1 + Tr2].
	•	Trait object: a value whose static type is a trait (e.g., named used as a type, not a constraint).

⸻

1. Trait kinds

A trait definition is classified by its members:

1.1 Field-only trait (STRUCTURAL)

Contains one or more fields and no methods.

trait named { name: string }

1.2 Method-only trait (NOMINAL)

Contains one or more methods and no fields.

trait show[a] { fn show(x: a) -> string }

1.3 Mixed trait (HYBRID)

Contains both fields and methods.

trait printable[a] {
  name: string
  fn format(x: a) -> string
}

1.4 Supertraits

A trait may declare supertraits:

trait ord[a]: eq { fn compare(x:a, y:a)->ordering }
trait foo[a]: bar + baz { ... }

Supertrait requirements are included in satisfaction checks.

⸻

2. Satisfaction model overview (the big rule)

2.1 Fields are structural

Field requirements are satisfied by record shape, not by impls.

2.2 Methods are nominal

Method requirements are satisfied only by an explicit impl (or compiler-provided builtin impl), never by structural matching of existing methods/fields.

No TS-style structural method satisfaction. Ever.

2.3 Mixed traits are satisfied if:
	•	all required fields are present structurally, AND
	•	method requirements are met via a nominal impl.

⸻

3. Trait satisfaction rules in detail

3.1 Field satisfaction: T ⊨ fields(Tr)

Let Tr have required fields { fi : Ti }.

A type T satisfies Tr’s field requirements iff:
	1.	T is a record type (including aliases that resolve to a record), and
	2.	for every required field fi, T contains a field named fi, and
	3.	the field’s type is equal to (or unifies with) required type Ti under current type substitutions (for polymorphic cases).

Notes:
	•	Field satisfaction is structural: any record with the right fields qualifies.
	•	Non-record types do not satisfy field traits by default.
	•	(Optional future: allow “field projection traits” for non-records via explicit impl + getters. Not v1.)

3.2 Method satisfaction: T ⊨ methods(Tr)

Let Tr have method requirements m1..mk.

T satisfies method requirements iff:
	•	There exists exactly one applicable impl of Tr for T in the trait registry, after considering conditional impl constraints.
	•	Builtin impls count as impls.

Structural matching is not allowed. A record having a field show or inherent method show does not imply show trait satisfaction.

3.3 Full trait satisfaction T ⊨ Tr

Depends on trait kind:
	•	Field-only trait: T ⊨ Tr iff T ⊨ fields(Tr) and T ⊨ supertraits(Tr)
	•	Method-only trait: T ⊨ Tr iff T ⊨ methods(Tr) and T ⊨ supertraits(Tr)
	•	Mixed trait: T ⊨ Tr iff T ⊨ fields(Tr) AND T ⊨ methods(Tr) AND T ⊨ supertraits(Tr)

3.4 Supertrait satisfaction

If Tr : S1 + S2 + ..., then T ⊨ Tr requires T ⊨ Si for all i.

Important: supertraits can be field-only, method-only, or mixed. Apply the corresponding rule recursively.

⸻

4. Orphan policy (coherence & ambiguity)

You currently allow orphan impls with warnings. This interacts with structural records.

4.1 Impl uniqueness rule (must hold)

For any (TraitName, ConcreteType) pair, there must be:
	•	0 or 1 most specific applicable impl after solver filtering, or
	•	the program is rejected as ambiguous.

4.2 Ambiguity is a hard error

If multiple impls match and neither is strictly more specific, reject:
	•	error includes both impl sites.

4.3 Field-only traits do not have impls

Because field-only traits are structural, there is no global registry entry required. This avoids coherence complexity for fields.

4.4 Mixed traits still need impls for methods

Mixed traits still participate in impl coherence checks (for their method part).

⸻

5. Constraint checking

Constraint syntax:

fn f[t: named + show](x: t) -> string { ... }

5.1 When constraints are checked

Constraints are checked at:
	•	generic function instantiation / call sites
	•	(optionally) at definition sites if you have polymorphic let-generalization checks, but call-site is required.

5.2 What it means to satisfy a constraint

For each required trait Tr in t: Tr1 + Tr2 + ...:
	•	the concrete inferred type Tc for t must satisfy Tc ⊨ Tr (Section 3).

5.3 Errors

If any constraint fails:
	•	error: Type <Tc> does not satisfy trait <Tr>
	•	include reason:
	•	missing field(s) for field traits,
	•	missing impl for method traits,
	•	missing supertrait satisfaction,
	•	ambiguous impl.

⸻

6. Method call resolution (no TS structural methods)

A method call expression:

receiver.method(arg1, arg2)

Resolution is not “find any field/method with that name”. It is deterministic.

6.1 Candidate sources

Candidates can come from:
	1.	Enum constructor / variant syntax (existing behavior, if any)
	2.	Inherent methods (if you implement them)
	3.	Trait methods (nominal, via impl registry)

Field access receiver.field is separate and uses record typing rules, not method resolution.

6.2 Trait method candidates

A trait method candidate exists only if:
	•	there is an applicable impl for receiver’s type for the trait that defines method.

No structural “receiver has method method” probing.

6.3 Collisions

If inherent and trait methods share (receiver_type, method_name):
	•	v1 policy (as you wrote): hard error ambiguity.
	•	future: allow qualified call syntax. (If you add now, document it as reserved.)

6.4 Dispatch mode interaction

If receiver is statically a concrete type (or generic constrained type), trait calls are statically resolved via solver + monomorphization.
If receiver has static type equal to a trait type (“trait object”), calls dispatch via interface (Section 7).

⸻

7. Trait objects (trait used as a type)

Example:

let xs: list[named] = [person, company]
puts(xs[0].name)

Trait object semantics depend on trait kind.

7.1 Field-only trait objects

Allowed. A value of static type named is a trait object requiring the presence of name: string.

Representation / codegen contract:
	•	trait object type becomes a Go interface with getter methods for required fields:
	•	GetName() string for name: string
	•	any concrete record type R used as named must have an auto-generated getter implementing that interface.

Satisfaction rule for coercion to trait object:
To coerce a value of type T to trait type Tr (field-only):
	•	T ⊨ fields(Tr) must hold.
	•	If not, type error.

7.2 Method-only trait objects (v1 decision)

You have two options; pick explicitly:

Option 7.2.A (recommended v1): disallow method-only trait objects
	•	You avoid Go-interface method dispatch for now except where needed for hetero field access.
	•	This keeps the system simpler and avoids needing “dictionary objects” for methods.

Option 7.2.B: allow method-only trait objects
	•	Then the trait object interface includes method signatures.
	•	You must generate wrapper types or pass method implementations in the object.
	•	This becomes full dynamic dispatch/vtable semantics. You said non-goal for v1.

So: v1 should disallow method-only trait objects.

7.3 Mixed trait objects (v1 decision)

Same as method-only: mixed trait object includes methods → dynamic dispatch required.

Given your stated non-goals:
	•	Disallow mixed trait objects in v1.
	•	Allow only field-only trait objects.

(Your earlier doc mentions dynamic dispatch for trait objects generally, but this spec recommends tightening: dynamic trait objects for methods is a huge semantic commitment. You can still keep hetero collections via field-only traits.)

7.4 Trait used as type in list[Tr]

In v1, this is allowed only if Tr is field-only.

⸻

8. Operator overloading via traits (nominal)

Operators desugar to trait functions (like your doc), but satisfaction is method-based:
	•	== requires eq[T] impl for T
	•	< requires ord[T] impl for T
	•	+ requires num[T] impl for T
etc.

No structural operator matching.

⸻

9. Interaction with records + row polymorphism

You prefer named constraints. Therefore:

9.1 Field traits as named row constraints (desugaring)

A field-only trait:

trait named { name: string }

is equivalent to a row constraint requiring that field:
	•	t: named means t’s type must be a record containing name: string.

9.2 Users do not need to write {name: string, ...r} in constraints

Row variables exist internally and in patterns, but not required in function signatures.

9.3 Pattern matching can still use open record patterns

Patterns can use { x:, ...rest } regardless of whether you expose row vars in types.

⸻

10. Implementation plan (for Codex)

This is “how to implement” at a systems level (not coding details, but exact module responsibilities).

10.1 Data structures

Trait registry

Store trait definitions:
	•	name
	•	type params
	•	required fields (name, type)
	•	required methods (name, polytype)
	•	supertraits list
	•	classification: field-only / method-only / mixed

Impl registry (methods only)

Store impls for traits that have methods:
	•	trait name
	•	for_type pattern (mono type with vars)
	•	constraints on vars
	•	methods bodies

Field-only traits do not store impls.

10.2 Core functions to implement

trait_kind(trait_def) -> kind

Compute kind by members.

satisfies_fields(T, trait) -> ok | missing_fields | mismatch
	•	Normalize T by expanding aliases and record canonicalization.
	•	Require T is record for v1.
	•	Check each required field exists and its type unifies.

solve_impl(T, trait) -> Solved impl | NotImplemented | Ambiguous

Existing solver logic applies.

satisfies_trait(T, trait) -> ok | error

Pseudo:
	1.	check all supertraits satisfied recursively
	2.	if trait has fields: satisfies_fields(T, trait) must succeed
	3.	if trait has methods: solve_impl(T, trait) must yield single impl
	4.	return ok else error

10.3 Where this is invoked

Constraint checking

During generic instantiation:
	•	for each t: Tr constraint, call satisfies_trait(concrete_type_of_t, Tr)

Method call resolution

When typing recv.method(...):
	1.	determine static type of recv
	2.	check inherent registry (if implemented)
	3.	else find trait(s) that define method and for which solve_impl(recv_type, trait) succeeds
	4.	if exactly one candidate -> resolve to that impl method
	5.	else if none -> error
	6.	else -> ambiguity error

Trait object coercion (field-only only)

When you assign / pass value v:T to context expecting Tr:
	•	if Tr is field-only: require satisfies_fields(T, Tr)
	•	else: reject in v1

10.4 Codegen requirements

For field-only trait objects
	•	Emit Go interface for each field-only trait used as a type:
	•	each field f: Tf becomes GetF() <go(Tf)>
	•	For each record type R that is ever coerced to that trait object:
	•	emit trivial getters

For method traits
	•	Static dispatch only (monomorphization).
	•	Do not emit Go interfaces for them in v1.

⸻

11. Compliance checklist for Codex review

Codex should verify the project matches these invariants:

Parser/AST
	•	trait member types include both fields and methods
	•	trait kind can be derived
	•	impl blocks exist only for traits (and later inherent impl)

Typechecker
	•	field trait satisfaction is structural and does not require impl
	•	method trait satisfaction requires explicit impl or builtin impl
	•	no code path that treats “has method named X” as trait satisfaction
	•	supertraits enforced for both field-only and method traits

Trait objects
	•	only field-only traits can appear as concrete types in v1
	•	list[named] style is rejected for method/mixed traits

Resolution
	•	x.name is purely record typing (field access), never solver-driven
	•	x.show() is method resolution (inherent/trait), never structural
	•	collisions are handled as hard errors

Codegen
	•	field-only trait object uses Go interface with getters
	•	no Go interface dispatch for method traits in v1

⸻

12. Open decisions you must lock (so Codex doesn’t guess)
	1.	Are field-only traits satisfied by non-record types?
	•	Spec says: no (v1).
	2.	Are method/mixed trait objects allowed?
	•	Spec says: no (v1).
	3.	Do mixed traits require explicit impl for methods even if fields match?
	•	Spec says: yes.
	4.	Do inherent methods participate in trait satisfaction?
	•	Spec says: no (ever). Inherent methods are separate.

⸻

If you want, paste your current trait codegen strategy you implemented (esp. around dynamic dispatch), and I can point out exactly where it deviates from this spec (e.g., if you currently allow method trait objects implicitly via Go interfaces).
