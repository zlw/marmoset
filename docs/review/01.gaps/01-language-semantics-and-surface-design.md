A. Language semantics & surface design

A1) Dot syntax ambiguity (field vs method vs trait-property)

You have all of these converging:
	•	record field access: r.x
	•	trait “field” access: x.name
	•	method call sugar: x.show() (dot-syntax static dispatch)

Right now the docs imply different Go lowerings (.Field vs .GetField()), and also mix “property” with “method”.

Future issue: once you add records + trait fields + method dot syntax, x.f needs a single coherent rule set or you’ll fight confusing edge cases (especially with inference + modules).

Fix: define a single semantic notion: property access.
	•	e.f is property access
	•	resolution order (suggested):
	1.	if e is a record type that has field f, resolve to field
	2.	else if e is constrained by trait(s) that require property f, resolve to property
	3.	else if e has a method f (trait method or normal fn-in-namespace), resolve as method call sugar when used with ()
	4.	otherwise error

Make “method call sugar” require parentheses (so x.show can be value later or not exist at all).

⸻

A2) “Trait fields are same as row constraints” needs an explicit rule

You said you don’t want two mechanisms. Good.
But you must decide: does property-only trait conformance require impl?

Future issue: anonymous records { name: "a" } cannot have explicit impl. If you require impls for field-only traits, you either:
	•	ban list[named] for anonymous records, or
	•	auto-generate impls implicitly anyway (which becomes “structural conformance” in disguise).

Fix (recommended hybrid):
	•	property-only traits are structural (no impl needed)
	•	traits with methods require impl (unless all methods have defaults and you allow “auto-impl marker”, but keep it simple first)

Also: once modules exist, structural conformance reduces “orphan rule” pressure.

⸻

A3) Orphan rule “allowed with warnings” will explode with modules

In single-file mode it’s fine. With modules/packages it becomes a coherence nightmare.

Future issue: two dependencies define impl show for int or impl show for Foo in different modules → ambiguity, different builds behave differently.

Fix options:
	•	(Preferred) keep “anyone for everyone” only in single-file / app root; once modules exist introduce:
	•	coherence per build: exactly one impl must be visible for (trait, type)
	•	define visibility/import rules for impls (explicit use impl Foo style), or
	•	adopt a real orphan rule: impl allowed only if trait or type is defined in current package
	•	If you keep Haskell-like open instances, you’ll need explicit “instance import” and very good error messages, plus LSP support.

⸻

A4) Dynamic dispatch performance claim (Go inliner) is shaky

Your doc says Go will inline trivial getters and “dynamic dispatch approaches static”.

Future issue: interface method dispatch cannot be inlined across interface calls; Go can inline the getter body, but not remove interface dispatch overhead. It’s still a virtual call. Also it can inhibit escape analysis and allocations.

Fix: keep the story honest:
	•	dynamic dispatch is “small overhead” but not “approaches static”
	•	provide escape hatch: all static dispatch via generics ([t: trait]) for performance paths
	•	later: add “sealed trait” or “enum wrapper” strategy for hot paths (optional)

⸻

A5) Structural records + Go nominal types is an actual hard problem

Your record doc claims structural type equality:

type point = { x: int, y: int }
type vec2d = { x: int, y: int }
let v: vec2d = p

But your codegen plan emits Go structs per alias or anonymous structs. In Go, identical structs are still different types if named, and anonymous struct literals are unique types too.

Future issue: this feature literally won’t work unless you canonicalize shapes.

Fix: you need a shape interner:
	•	compute a canonical “record shape key” from sorted fields + their types
	•	emit exactly one Go struct type per unique shape
	•	make aliases just type synonyms to that canonical type name (or just map alias→canonical name in emitter)
	•	this also helps derive and equality.

Also decide canonical field order. You already flagged it — it’s not optional if you want structural equality and stable codegen.

⸻

A6) Row polymorphism unification plan is under-specified

Your unify sketch is not correct enough for real usage (open-open unification with “fresh row” as a list element is not a thing). Row polymorphism needs a concrete representation: either
	•	row as a map + row var
	•	or row as a “tail” variable with difference constraints

Future issue: you’ll get unsoundness or impossible-to-solve constraints quickly (especially with spread + preserving fields).

Fix: pick a known approach:
	•	represent record type as (sorted field map, optional rowvar)
	•	unification:
	•	unify common fields
	•	leftover fields from closed side must be empty
	•	leftover fields from open side are pushed into row var substitution as a record fragment
	•	open-open introduces fresh row var for remainder
You’ll want an occurs check / cycle prevention for row vars too.

⸻

B. Frontend (lexer/parser/AST)

B1) Spread token lexer code is buggy

Your lexer snippet for ... does multiple read_char/peek_char in a way that likely consumes wrong characters.

Future issue: subtle lexing bugs will make parser miserable.

Fix: implement a proper lookahead for .:
	•	if next two chars are . then emit Spread and consume 3
	•	else emit Dot

Also: you need consistent position tracking across multi-char tokens for LSP diagnostics.
