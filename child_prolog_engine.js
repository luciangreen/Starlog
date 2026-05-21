(function (root, factory) {
  if (typeof module === "object" && module.exports) {
    module.exports = factory();
  } else {
    root.ChildPrologEngine = factory();
  }
})(typeof globalThis !== "undefined" ? globalThis : this, function () {
  class ChildPrologEngine {
    constructor(options = {}) {
      this.maxDepth = options.maxDepth || 30;
      this.maxSolutions = options.maxSolutions || 10;
    }

    run(programText, queryText) {
      const clauses = this.parseProgram(programText);
      const query = this.parseQuery(queryText);
      const queryVariables = this.collectVariables(query);
      const solutions = this.solveGoals([query], {}, clauses, [], 0, []);
      if (solutions.length === 0) {
        return {
          answer: "No proof found.",
          query: this.termToString(query),
          bindings: {},
          solutions: [],
          steps: [`I couldn't prove ${this.termToString(query)} with the rules I know yet.`],
          trace: [],
          visual: {
            type: "tree",
            data: {
              root: this.termToString(query),
              children: []
            }
          }
        };
      }

      const first = solutions[0];
      const formattedSolutions = solutions.map((solution) =>
        this.extractBindings(queryVariables, solution.env)
      );
      const bindings = this.extractBindings(queryVariables, first.env);
      const steps = this.buildExplanation(query, first.trace);

      return {
        answer: Object.keys(bindings).length > 0 ? "Yes, with bindings." : "Yes.",
        query: this.termToString(query),
        bindings,
        solutions: formattedSolutions,
        steps,
        trace: first.trace,
        visual: this.buildVisual(query, first.trace)
      };
    }

    parseProgram(text) {
      const parser = new Parser(text);
      return parser.parseProgram();
    }

    parseQuery(text) {
      const cleaned = String(text)
        .trim()
        .replace(/^query\s*:\s*/i, "")
        .replace(/^\?-\s*/, "");
      const parser = new Parser(cleaned.endsWith(".") ? cleaned : `${cleaned}.`);
      return parser.parseSingleTerm();
    }

    solveGoals(goals, env, clauses, trace, depth, ancestry) {
      if (depth > this.maxDepth) {
        return [];
      }
      if (goals.length === 0) {
        return [{ env, trace, ancestry }];
      }

      const [goal, ...restGoals] = goals;
      const resolvedGoal = this.substitute(goal, env);

      if (resolvedGoal.type === "atom" && resolvedGoal.value === "true") {
        return this.solveGoals(restGoals, env, clauses, trace, depth, ancestry);
      }

      const matches = [];
      for (let clauseIndex = 0; clauseIndex < clauses.length; clauseIndex += 1) {
        const clause = clauses[clauseIndex];
        if (!this.samePredicate(resolvedGoal, clause.head)) {
          continue;
        }

        const freshClause = this.freshenClause(clause, `${depth}_${clauseIndex}_${trace.length}`);
        const nextEnv = this.unify(resolvedGoal, freshClause.head, env);
        if (!nextEnv) {
          continue;
        }

        const groundedGoal = this.substitute(resolvedGoal, nextEnv);
        const traceEntry = {
          depth,
          goal: this.termToString(groundedGoal),
          functor: groundedGoal.functor,
          args: groundedGoal.args ? groundedGoal.args.map((arg) => this.termToString(arg)) : [],
          clause: clause.source,
          type: freshClause.body.length === 0 ? "fact" : "rule"
        };

        const childSolutions = this.solveGoals(
          [...freshClause.body, ...restGoals],
          nextEnv,
          clauses,
          [...trace, traceEntry],
          depth + 1,
          [...ancestry, traceEntry]
        );

        for (const solution of childSolutions) {
          matches.push(solution);
          if (matches.length >= this.maxSolutions) {
            return matches;
          }
        }
      }

      return matches;
    }

    samePredicate(a, b) {
      return (
        a &&
        b &&
        a.type === "compound" &&
        b.type === "compound" &&
        a.functor === b.functor &&
        a.args.length === b.args.length
      );
    }

    freshenClause(clause, suffix) {
      const varMap = new Map();
      const freshen = (term) => {
        if (term.type === "var") {
          if (!varMap.has(term.name)) {
            varMap.set(term.name, { type: "var", name: `${term.name}_${suffix}` });
          }
          return varMap.get(term.name);
        }
        if (term.type === "compound") {
          return {
            type: "compound",
            functor: term.functor,
            args: term.args.map(freshen)
          };
        }
        return term;
      };

      return {
        head: freshen(clause.head),
        body: clause.body.map(freshen),
        source: clause.source
      };
    }

    unify(left, right, env) {
      const nextEnv = { ...env };
      return this.unifyInto(left, right, nextEnv);
    }

    unifyInto(left, right, env) {
      const a = this.deref(left, env);
      const b = this.deref(right, env);

      if (a.type === "var") {
        env[a.name] = b;
        return env;
      }
      if (b.type === "var") {
        env[b.name] = a;
        return env;
      }
      if (a.type !== b.type) {
        return null;
      }
      if (a.type === "atom") {
        return a.value === b.value ? env : null;
      }
      if (a.type === "number") {
        return a.value === b.value ? env : null;
      }
      if (a.functor !== b.functor || a.args.length !== b.args.length) {
        return null;
      }

      let currentEnv = env;
      for (let index = 0; index < a.args.length; index += 1) {
        currentEnv = this.unifyInto(a.args[index], b.args[index], currentEnv);
        if (!currentEnv) {
          return null;
        }
      }
      return currentEnv;
    }

    deref(term, env) {
      let current = term;
      const seen = new Set();
      while (current && current.type === "var" && env[current.name] && !seen.has(current.name)) {
        seen.add(current.name);
        current = env[current.name];
      }
      return current;
    }

    substitute(term, env) {
      const resolved = this.deref(term, env);
      if (resolved.type === "compound") {
        return {
          type: "compound",
          functor: resolved.functor,
          args: resolved.args.map((arg) => this.substitute(arg, env))
        };
      }
      return resolved;
    }

    collectVariables(term, names = new Set()) {
      if (term.type === "var") {
        names.add(term.name);
        return names;
      }
      if (term.type === "compound") {
        term.args.forEach((arg) => this.collectVariables(arg, names));
      }
      return names;
    }

    extractBindings(queryVariables, env) {
      const bindings = {};
      for (const name of queryVariables) {
        const resolved = this.deref({ type: "var", name }, env);
        if (resolved.type !== "var") {
          bindings[name] = this.termToString(this.substitute(resolved, env));
        }
      }
      return bindings;
    }

    buildExplanation(query, trace) {
      if (
        query.type === "compound" &&
        query.functor === "ancestor" &&
        query.args.length === 2 &&
        trace.some((entry) => entry.functor === "parent")
      ) {
        const pathEdges = trace
          .filter((entry) => entry.functor === "parent" && entry.args.length === 2)
          .map((entry) => ({ from: entry.args[0], to: entry.args[1] }));
        const start = this.prettyName(this.termToString(query.args[0]));
        const end = this.prettyName(this.termToString(query.args[1]));
        const via = pathEdges.slice(0, -1).map((edge) => this.prettyName(edge.to));
        const steps = pathEdges.map(
          (edge, index) =>
            `${index + 1}. ${this.prettyName(edge.from)} is ${this.prettyName(edge.to)}'s parent.`
        );
        const conclusion =
          via.length > 0
            ? `So ${start} is connected to ${end} through ${via.join(" and ")}.`
            : `So ${start} is directly connected to ${end}.`;
        return steps.concat(conclusion);
      }

      return trace.map(
        (entry, index) => `${index + 1}. To prove ${entry.goal}, I used ${entry.clause}`
      );
    }

    buildVisual(query, trace) {
      const parentEdges = trace
        .filter((entry) => entry.functor === "parent" && entry.args.length === 2)
        .map((entry) => ({ from: entry.args[0], to: entry.args[1], label: "parent" }));

      if (parentEdges.length > 0) {
        const names = [];
        parentEdges.forEach((edge) => {
          if (!names.includes(edge.from)) {
            names.push(edge.from);
          }
          if (!names.includes(edge.to)) {
            names.push(edge.to);
          }
        });
        return {
          type: "graph",
          data: {
            query: this.termToString(query),
            nodes: names.map((id) => ({ id, label: this.prettyName(id) })),
            edges: parentEdges,
            path: names
          }
        };
      }

      return {
        type: "tree",
        data: {
          root: this.termToString(query),
          children: trace.map((entry) => ({
            label: entry.goal,
            clause: entry.clause
          }))
        }
      };
    }

    prettyName(value) {
      if (!value) {
        return value;
      }
      return value.charAt(0).toUpperCase() + value.slice(1);
    }

    termToString(term) {
      if (!term) {
        return "";
      }
      if (term.type === "var") {
        return term.name;
      }
      if (term.type === "atom") {
        return term.value;
      }
      if (term.type === "number") {
        return String(term.value);
      }
      return `${term.functor}(${term.args.map((arg) => this.termToString(arg)).join(",")})`;
    }
  }

  class Parser {
    constructor(text) {
      this.tokens = tokenize(stripComments(text));
      this.position = 0;
    }

    parseProgram() {
      const clauses = [];
      while (!this.isAtEnd()) {
        const start = this.position;
        const head = this.parseTerm();
        const body = this.match(":-") ? this.parseGoalList() : [];
        this.consume(".");
        clauses.push({
          head,
          body,
          source: this.tokensToSource(start, this.position - 1)
        });
      }
      return clauses;
    }

    parseSingleTerm() {
      const term = this.parseTerm();
      this.consume(".");
      return term;
    }

    parseGoalList() {
      const goals = [this.parseTerm()];
      while (this.match(",")) {
        goals.push(this.parseTerm());
      }
      return goals;
    }

    parseTerm() {
      const token = this.peek();
      if (!token) {
        throw new Error("Unexpected end of input.");
      }

      if (token.type === "number") {
        this.advance();
        return { type: "number", value: Number(token.value) };
      }

      if (token.type === "identifier" || token.type === "string") {
        this.advance();
        const raw = token.value;
        if (token.type === "identifier" && /^[A-Z_]/.test(raw)) {
          return { type: "var", name: raw };
        }
        if (this.match("(")) {
          const args = [];
          if (!this.check(")")) {
            args.push(this.parseTerm());
            while (this.match(",")) {
              args.push(this.parseTerm());
            }
          }
          this.consume(")");
          return { type: "compound", functor: raw, args };
        }
        return { type: "atom", value: raw };
      }

      throw new Error(`Unexpected token: ${token.value}`);
    }

    tokensToSource(start, end) {
      return this.tokens
        .slice(start, end + 1)
        .map((token) => token.value)
        .join("")
        .replace(/\s*([(),.:-])\s*/g, "$1")
        .replace(/:-/g, " :- ")
        .replace(/,/g, ", ");
    }

    match(value) {
      if (this.check(value)) {
        this.advance();
        return true;
      }
      return false;
    }

    consume(value) {
      if (!this.check(value)) {
        const token = this.peek();
        throw new Error(`Expected '${value}' but found '${token ? token.value : "EOF"}'.`);
      }
      return this.advance();
    }

    check(value) {
      if (this.isAtEnd()) {
        return false;
      }
      return this.peek().value === value;
    }

    advance() {
      return this.tokens[this.position++];
    }

    peek() {
      return this.tokens[this.position];
    }

    isAtEnd() {
      return this.position >= this.tokens.length;
    }
  }

  function stripComments(text) {
    return String(text)
      .replace(/\/\*[\s\S]*?\*\//g, "")
      .replace(/%.*$/gm, "");
  }

  function tokenize(text) {
    const tokens = [];
    let index = 0;

    while (index < text.length) {
      const char = text[index];

      if (/\s/.test(char)) {
        index += 1;
        continue;
      }

      if (text.startsWith(":-", index)) {
        tokens.push({ type: "symbol", value: ":-" });
        index += 2;
        continue;
      }

      if (/[(),.]/.test(char)) {
        tokens.push({ type: "symbol", value: char });
        index += 1;
        continue;
      }

      if (char === '"' || char === "'") {
        let value = "";
        const quote = char;
        index += 1;
        while (index < text.length && text[index] !== quote) {
          value += text[index];
          index += 1;
        }
        index += 1;
        tokens.push({ type: "string", value });
        continue;
      }

      const numberMatch = /^[0-9]+(?:\.[0-9]+)?/.exec(text.slice(index));
      if (numberMatch) {
        tokens.push({ type: "number", value: numberMatch[0] });
        index += numberMatch[0].length;
        continue;
      }

      const identifierMatch = /^[A-Za-z_][A-Za-z0-9_]*/.exec(text.slice(index));
      if (identifierMatch) {
        tokens.push({ type: "identifier", value: identifierMatch[0] });
        index += identifierMatch[0].length;
        continue;
      }

      throw new Error(`Unexpected character: ${char}`);
    }

    return tokens;
  }

  return ChildPrologEngine;
});
