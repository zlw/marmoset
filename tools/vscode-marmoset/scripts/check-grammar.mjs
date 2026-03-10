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
assertNoMatch(repo.keywords.patterns, "for", "keyword patterns");

assertRegexMatches(repo["builtin-types"], "match", "Int", "builtin type pattern");
assertRegexMatches(repo["builtin-types"], "match", "Str", "builtin type pattern");
assertRegexMatches(repo["builtin-types"], "match", "Unit", "builtin type pattern");
assertRegexDoesNotMatch(repo["builtin-types"], "match", "int", "builtin type pattern");

assertAnyMatch(repo.operators.patterns, "=>", "operator patterns");
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
  "fn(Int, Int) -> Int",
  "function-type begin",
);
assert(repo["trait-object-type"], "trait-object-type repository entry is missing");
assertRegexMatches(repo["trait-object-type"], "begin", "Dyn[Show & Eq]", "trait-object-type begin");
assertRegexMatches(repo["trait-object-type"], "end", "]", "trait-object-type end");

assertRegexMatches(repo["impl-block"], "begin", "impl[a: Show] Show[List[a]] = {", "impl-block begin");
assert(
  !JSON.stringify(repo["impl-block"]).includes("\\\\b(for)"),
  "impl-block should not advertise legacy for-syntax",
);
assert(repo["derive-clause"], "derive-clause repository entry is missing");
assertRegexMatches(repo["derive-clause"], "match", "derive Eq, Show", "derive-clause match");
assert(!repo["derive-statement"], "derive-statement repository entry should be removed");
assert(!repo["function-literal"], "function-literal repository entry should be removed");
assert(
  !grammar.patterns.some((entry) => entry?.include === "#derive-statement"),
  "top-level patterns should not include derive-statement",
);
assert(
  !grammar.patterns.some((entry) => entry?.include === "#function-literal"),
  "top-level patterns should not include function-literal",
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
