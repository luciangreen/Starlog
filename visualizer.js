(function (root, factory) {
  if (typeof module === "object" && module.exports) {
    module.exports = factory();
  } else {
    root.ChildPrologVisualizer = factory();
  }
})(typeof globalThis !== "undefined" ? globalThis : this, function () {
  function renderVisual(container, visual) {
    if (!container) {
      return;
    }

    if (!visual || !visual.type) {
      container.innerHTML = "";
      return;
    }

    if (visual.type === "graph") {
      container.innerHTML = renderGraph(visual.data);
      return;
    }

    container.innerHTML = renderTree(visual.data);
  }

  function renderGraph(data) {
    const nodes = data.nodes || [];
    const width = Math.max(220, nodes.length * 140);
    const height = 140;

    const circles = nodes
      .map((node, index) => {
        const x = 70 + index * 120;
        return `
          <circle cx="${x}" cy="55" r="24" fill="#fff7d6" stroke="#c97b00" stroke-width="2"></circle>
          <text x="${x}" y="60" text-anchor="middle" font-size="14">${escapeHtml(node.label)}</text>
        `;
      })
      .join("");

    const edges = (data.edges || [])
      .map((edge, index) => {
        const start = 70 + index * 120;
        const end = 70 + (index + 1) * 120;
        return `
          <line x1="${start + 24}" y1="55" x2="${end - 24}" y2="55" stroke="#2d6cdf" stroke-width="3" marker-end="url(#arrow)"></line>
          <text x="${(start + end) / 2}" y="40" text-anchor="middle" font-size="12">${escapeHtml(edge.label || "")}</text>
        `;
      })
      .join("");

    return `
      <svg viewBox="0 0 ${width} ${height}" width="100%" height="${height}">
        <defs>
          <marker id="arrow" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse">
            <path d="M 0 0 L 10 5 L 0 10 z" fill="#2d6cdf"></path>
          </marker>
        </defs>
        ${edges}
        ${circles}
      </svg>
    `;
  }

  function renderTree(data) {
    const children = (data.children || [])
      .map(
        (child) =>
          `<li><strong>${escapeHtml(child.label)}</strong><div>${escapeHtml(child.clause || "")}</div></li>`
      )
      .join("");
    return `
      <div class="trace-tree">
        <strong>${escapeHtml(data.root || "Query")}</strong>
        <ul>${children}</ul>
      </div>
    `;
  }

  function escapeHtml(value) {
    return String(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;");
  }

  return { renderVisual };
});
