const assert = require("node:assert/strict");
const ChildPrologEngine = require("../child_prolog_engine");

const engine = new ChildPrologEngine();
const program = `
parent(alice,bob).
parent(bob,charlie).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).
`;

function runTests() {
  const ancestorResult = engine.run(program, "ancestor(alice,charlie).");
  assert.equal(ancestorResult.answer, "Yes.");
  assert.equal(ancestorResult.visual.type, "graph");
  assert.deepEqual(ancestorResult.visual.data.path, ["alice", "bob", "charlie"]);
  assert.match(ancestorResult.steps.join(" "), /Alice is Bob's parent/);
  assert.match(ancestorResult.steps.join(" "), /Bob is Charlie's parent/);

  const bindingResult = engine.run(program, "ancestor(alice,Y).");
  assert.equal(bindingResult.answer, "Yes, with bindings.");
  assert.equal(bindingResult.bindings.Y, "bob");
  assert.equal(bindingResult.solutions.length >= 2, true);
  assert.deepEqual(bindingResult.solutions.map((item) => item.Y), ["bob", "charlie"]);

  const noResult = engine.run(program, "ancestor(charlie,alice).");
  assert.equal(noResult.answer, "No proof found.");
}

runTests();
console.log("test_child_prolog_engine.js: all tests passed");
