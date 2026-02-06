2) Best order plan from now (detailed, with improvements)

I’ll assume: you’re mid-records implementation, traits milestone is planned/partial, everything is still single-file.

Phase 1: Stabilize core data model

1) Finish records properly (with two critical upgrades)
	•	Implement record literals, field access, spread, patterns
	•	Implement row polymorphism unification (pick a correct representation)
	•	Add record shape interning + canonical ordering
	•	this is mandatory for structural equality and good codegen
	•	Add spans/node IDs everywhere (for errors + future LSP)

Deliverable: records that actually satisfy the “structural type equality” test.

⸻

Phase 2: Lock down trait/property/operator semantics before modules

2) Implement operator desugaring now (even if codegen is naive)
	•	== etc desugar to trait calls in a lowering phase
	•	same for arithmetic ops
	•	make sure it creates trait obligations cleanly

Also fix the trait doc consistency:
	•	define property access semantics
	•	decide structural conformance for property-only traits (recommended)

Deliverable: surface syntax feels normal; no more a.eq(b).

⸻

3) Implement minimal task concurrency runtime (optional here, but good)
	•	task[a,e], spawn, await, all, race, cancel
	•	purity: these are impure
	•	make it work end-to-end with Go codegen

Why here: shakes out your => inference early without involving modules/FFI.

Deliverable: real concurrency usable in single file.

(Streams can wait.)

⸻

Phase 3: Ecosystem plumbing (this is the big unlock)

4) Modules + build/package system together

Decide:
	•	module syntax: import foo.bar
	•	package root discovery
	•	file layout rules
	•	visibility (pub vs private)
	•	how to avoid cycles (or allow SCC)

Implementation:
	•	module graph
	•	name resolution across files
	•	typecheck whole workspace
	•	emit a single Go package per build

Deliverable: multi-file Marmoset projects compile.

⸻

5) LSP v0 immediately after modules/build

Implement minimal:
	•	diagnostics
	•	hover type
	•	definition
	•	document symbols

Back it with:
	•	the same module graph + typecheck pipeline
	•	simple caching per file

Deliverable: editing becomes enjoyable; you stop flying blind.

⸻

Phase 4: Interop + standard library growth

6) Go FFI (extern declarations)

Design goals:
	•	no runtime reflection
	•	compile-time checked signatures
	•	clear mapping of result and option

Implement:
	•	extern syntax + importable extern modules
	•	wrapper generation in Go emitter
	•	basic types first: int, float, bool, string
	•	then records/enums (with clear representation)
	•	decide collection mapping (slice/map vs persistent collection wrappers later)

Deliverable: you can call Go libs and build real programs.

⸻

7) Persistent collections (recommended as stdlib via FFI first)

Now that you have modules + FFI:
	•	ship std.pvec, std.pmap
	•	either wrap a Go persistent lib or implement in Go runtime package inside your repo
	•	keep API stable and FP:
	•	get/set/update/insert/remove/iter/fold
	•	plan for later “transient” builders (impure) but don’t need now

Deliverable: FP-friendly performance + ergonomics without deep compiler optimizations.

⸻

8) Streams (library built on Task + channels internally)

Once you have IO/FFI, streams become valuable.
	•	stream pipelines for IO, concurrency, event-ish flows
	•	eval_map(concurrency=n) pattern

Deliverable: modern FP concurrency story.

⸻

Phase 5: Advanced types

9) Existentials (formalize trait objects, and add explicit existentials if desired)

Do this when modules/FFI are stable, because representation matters across boundaries.

Deliverable: coherent story for hetero collections + abstraction barriers.

⸻

10) HKT (last big type-system expansion)

Only after your core language is stable and you’re ready for the complexity.
This will touch:
	•	type representation
	•	kind checking
	•	trait solver
	•	stdlib design

Deliverable: functor/monad style abstractions etc.

⸻

Phase 6: LSP v1 (upgrade)

Once language stabilizes:
	•	incremental typecheck caching
	•	better diagnostics for trait solving / row errors / HKT
	•	code actions (import fix, derive suggestions)
