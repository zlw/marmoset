3) Targeted “improvements to current design/codegen” you should do early

These are high leverage and prevent rewrites:
	1.	Record shape interning (mandatory)
	2.	Monomorphization instantiation cache keyed by (fn_id, type_subst) (mandatory)
	3.	Explicit obligation list in type inference (traits/operators/constraints unify)
	4.	Unified property access semantics (e.f), and stop mixing field/method terminology
	5.	Coherence plan for impls when modules arrive (even if you postpone enforcement)
	6.	One Go package per build (strongly recommended; makes trait objects + impl emission feasible)
