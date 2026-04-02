import { readFileSync } from "node:fs";

function loadJson(path) {
  return JSON.parse(readFileSync(new URL(path, import.meta.url)));
}

function fail(message) {
  throw new Error(message);
}

function assert(condition, message) {
  if (!condition) {
    fail(message);
  }
}

function regexFor(entry, field, label) {
  const pattern = entry?.[field];
  assert(typeof pattern === "string", `${label} is missing ${field}`);
  return new RegExp(pattern);
}

function assertRegexMatches(entry, field, sample, label) {
  const regex = regexFor(entry, field, label);
  assert(regex.test(sample), `${label} does not match ${JSON.stringify(sample)}`);
}

function assertRegexDoesNotMatch(entry, field, sample, label) {
  const regex = regexFor(entry, field, label);
  assert(!regex.test(sample), `${label} unexpectedly matches ${JSON.stringify(sample)}`);
}

function assertAnyMatch(entries, sample, label) {
  assert(Array.isArray(entries), `${label} is not a pattern list`);
  const matches = entries.some((entry) => {
    if (typeof entry?.match !== "string") {
      return false;
    }
    return new RegExp(entry.match).test(sample);
  });
  assert(matches, `${label} does not match ${JSON.stringify(sample)}`);
}

function assertNoMatch(entries, sample, label) {
  assert(Array.isArray(entries), `${label} is not a pattern list`);
  const matches = entries.some((entry) => {
    if (typeof entry?.match !== "string") {
      return false;
    }
    return new RegExp(entry.match).test(sample);
  });
  assert(!matches, `${label} unexpectedly matches ${JSON.stringify(sample)}`);
}

function findByName(entries, name) {
  return entries.find((entry) => entry?.name === name);
}

const grammar = loadJson("../syntaxes/marmoset.tmLanguage.json");
const languageConfig = loadJson("../language-configuration.json");
const repo = grammar.repository;

assertAnyMatch(repo.keywords.patterns, "case", "keyword patterns");
assertAnyMatch(repo.keywords.patterns, "override", "keyword patterns");
assertAnyMatch(repo.keywords.patterns, "fn", "keyword patterns");
assertAnyMatch(repo.keywords.patterns, "shape", "keyword patterns");
assertNoMatch(repo.keywords.patterns, "for", "keyword patterns");

assertRegexMatches(repo["builtin-types"], "match", "Int", "builtin type pattern");
assertRegexMatches(repo["builtin-types"], "match", "Str", "builtin type pattern");
assertRegexMatches(repo["builtin-types"], "match", "Unit", "builtin type pattern");
assertRegexDoesNotMatch(repo["builtin-types"], "match", "int", "builtin type pattern");

assertAnyMatch(repo.operators.patterns, "=>", "operator patterns");
assertAnyMatch(repo.operators.patterns, "|>", "operator patterns");
assertAnyMatch(repo.operators.patterns, "&&", "operator patterns");
assertAnyMatch(repo.operators.patterns, "||", "operator patterns");
assertAnyMatch(repo.operators.patterns, "%", "operator patterns");
assertAnyMatch(repo.operators.patterns, "<=", "operator patterns");
assertAnyMatch(repo.operators.patterns, ">=", "operator patterns");
assertAnyMatch(repo.operators.patterns, "&", "operator patterns");

assert(repo["fn-declaration"], "fn-declaration repository entry is missing");
assertRegexMatches(repo["fn-declaration"], "begin", "fn add(x: Int) -> Int = x", "fn-declaration begin");

assert(repo["lambda-expression"], "lambda-expression repository entry is missing");
assertRegexMatches(
  repo["lambda-expression"],
  "begin",
  "(x: Int) => x + 1",
  "lambda-expression begin",
);

assertRegexMatches(repo["function-type"], "begin", "(Int, Int) => Int", "function-type begin");
assertRegexMatches(repo["function-type"], "end", ") => Int", "function-type end");
assertRegexDoesNotMatch(
  repo["function-type"],
  "begin",
  "Int -> Int",
  "function-type begin",
);
assert(repo["trait-object-type"], "trait-object-type repository entry is missing");
assertRegexMatches(repo["trait-object-type"], "begin", "Dyn[Show & Eq]", "trait-object-type begin");
assertRegexMatches(repo["trait-object-type"], "end", "]", "trait-object-type end");
assert(repo["shape-definition"], "shape-definition repository entry is missing");
assertRegexMatches(repo["shape-definition"], "begin", "shape Named = {", "shape-definition begin");
assert(repo["shape-field"], "shape-field repository entry is missing");
assertRegexMatches(repo["shape-field"], "match", "name: Str", "shape-field match");
assert(repo["intersection-type"], "intersection-type repository entry is missing");
assertRegexMatches(repo["intersection-type"], "match", "Named & Aged", "intersection-type match");
assertRegexMatches(repo["intersection-type"], "match", "List[Int] & Named", "intersection-type match");
assertRegexDoesNotMatch(repo["intersection-type"], "match", "Dyn[Show & Eq]", "intersection-type match");

assertRegexMatches(repo["impl-block"], "begin", "impl[a: Show] Show[List[a]] = {", "impl-block begin");
assert(
  !JSON.stringify(repo["impl-block"]).includes("\\\\b(for)"),
  "impl-block should not advertise removed header spellings",
);
assert(repo["derive-clause"], "derive-clause repository entry is missing");
assertRegexMatches(repo["derive-clause"], "match", "derive Eq, Show", "derive-clause match");
assert(!repo["derive-statement"], "removed detached-derive entry should stay absent");
assert(!repo["function-literal"], "removed block-lambda entry should stay absent");
assert(!repo["trait-field"], "removed trait-field entry should stay absent");
assert(
  !grammar.patterns.some((entry) => entry?.include === "#derive-statement"),
  "top-level patterns should not include the removed detached-derive entry",
);
assert(
  !grammar.patterns.some((entry) => entry?.include === "#function-literal"),
  "top-level patterns should not include the removed block-lambda entry",
);
assert(
  grammar.patterns.some((entry) => entry?.include === "#shape-definition"),
  "top-level patterns should include shape-definition",
);
assert(
  repo["type-definition"].patterns.some((entry) => entry?.include === "#constructor-type-body"),
  "type-definition should highlight canonical constructor bodies",
);
assert(
  repo["type-definition"].patterns.some((entry) => entry?.include === "#wrapper-type"),
  "type-definition should highlight wrapper bodies",
);
assert(repo.interpolation, "interpolation repository entry is missing");
assertRegexMatches(repo.interpolation, "begin", "#{name}", "interpolation begin");
assertRegexMatches(repo.interpolation, "end", "}", "interpolation end");
assert(
  repo.string.patterns.some((entry) => entry?.include === "#interpolation"),
  "string patterns should include interpolation",
);
assertRegexMatches(repo["field-access"], "match", ".ring!", "field-access match");
assertRegexMatches(repo["field-access"], "match", ".hungry?", "field-access match");
assert(
  repo.expression.patterns.some((entry) => entry?.include === "#object-literal"),
  "expression patterns should include object literals",
);
assert(
  repo.expression.patterns.some((entry) => entry?.include === "#match-expression"),
  "expression patterns should include match expressions",
);
assert(
  repo.expression.patterns.some((entry) => entry?.include === "#if-expression"),
  "expression patterns should include if expressions",
);

assertRegexMatches(
  repo["type-parameter-list"].patterns[0].captures["3"].patterns[1],
  "match",
  "&",
  "constraint separator",
);
assertRegexDoesNotMatch(
  repo["type-parameter-list"].patterns[0].captures["3"].patterns[1],
  "match",
  "+",
  "constraint separator",
);
assertRegexMatches(
  repo["trait-definition"].patterns[1],
  "begin",
  ": Show & Eq = {",
  "trait supertrait begin",
);
assertRegexDoesNotMatch(
  repo["trait-definition"].patterns[1].patterns[1],
  "match",
  "+",
  "trait supertrait separator",
);
assert(
  repo["method-definition"].patterns.some((entry) => entry?.include === "#expression"),
  "method-definition should highlight expression bodies",
);

assert(
  /\[\!\?\]/.test(languageConfig.wordPattern) || /\[!\?\]\?/.test(languageConfig.wordPattern),
  "language wordPattern should include identifier suffixes",
);

console.log("TextMate grammar smoke checks passed.");
