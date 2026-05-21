(function () {
  const defaultProgram = `parent(alice,bob).
parent(bob,charlie).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).`;

  const defaultQuery = "ancestor(alice,charlie).";

  function setDefaultValues() {
    const programBox = document.getElementById("program");
    const queryBox = document.getElementById("query");
    if (programBox && !programBox.value.trim()) {
      programBox.value = defaultProgram;
    }
    if (queryBox && !queryBox.value.trim()) {
      queryBox.value = defaultQuery;
    }
  }

  function renderResult(result) {
    const answer = document.getElementById("answer");
    const steps = document.getElementById("steps");
    const bindings = document.getElementById("bindings");
    const json = document.getElementById("json-output");
    const visual = document.getElementById("visual-output");

    answer.textContent = result.answer;
    bindings.innerHTML =
      Object.keys(result.bindings).length === 0
        ? "<em>No variable bindings for this query.</em>"
        : Object.entries(result.bindings)
            .map(([name, value]) => `<div><strong>${escapeHtml(name)}</strong> = ${escapeHtml(value)}</div>`)
            .join("");
    steps.innerHTML = result.steps.map((step) => `<li>${escapeHtml(step)}</li>`).join("");
    json.textContent = JSON.stringify(result, null, 2);
    window.ChildPrologVisualizer.renderVisual(visual, result.visual);
  }

  function escapeHtml(value) {
    return String(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;");
  }

  async function loadExample() {
    try {
      const response = await fetch("examples/stage1_ancestor.pl");
      if (!response.ok) {
        throw new Error("Example not found");
      }
      const program = await response.text();
      const programBox = document.getElementById("program");
      if (programBox) {
        programBox.value = program.trim();
      }
    } catch (_error) {
      const programBox = document.getElementById("program");
      if (programBox) {
        programBox.value = defaultProgram;
      }
    }
  }

  function runExample() {
    const engine = new window.ChildPrologEngine();
    const program = document.getElementById("program").value;
    const query = document.getElementById("query").value;
    const result = engine.run(program, query);
    renderResult(result);
  }

  window.runExample = runExample;

  window.addEventListener("DOMContentLoaded", async () => {
    setDefaultValues();
    await loadExample();
    runExample();
  });
})();
