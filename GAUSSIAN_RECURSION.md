# GAUSSIAN_RECURSION

## Stage 11 documentation rule

Polynomial discovery is required to use Gaussian elimination as the mandatory coefficient solver.

Finite differences may be used only to estimate a plausible degree. They do not replace Gaussian elimination for solving coefficients.

## Worked example: triangular numbers

For samples `(1,1)`, `(2,3)`, `(3,6)`, construct the polynomial system:

- `a0 + a1*1 + a2*1^2 = 1`
- `a0 + a1*2 + a2*2^2 = 3`
- `a0 + a1*3 + a2*3^2 = 6`

Solve by Gaussian elimination to obtain coefficients equivalent to:

- `a0 = 0`
- `a1 = 1/2`
- `a2 = 1/2`

Reconstruct:

- `0.5*N^2 + 0.5*N`
- optionally simplified to `N*(N+1)/2`

## Indexed-variable polynomial discovery

When traced indexed-variable lineage yields polynomial relations, coefficients are also solved with Gaussian elimination.

Examples:

- affine indexed formulas: `x_i = 4+i-1`, `y_i = 5+i-1`
- polynomial indexed formulas: `u_i = i^2+i`

The reconstruction is performed from traced correspondences and sample equations, not from hardcoded templates.

## Unsupported cases

No polynomial rewrite should be applied when:

- purity or reducibility requirements fail,
- the relation is non-polynomial,
- the sampled system is insufficient or inconsistent,
- validation fails against observed behaviour.

In such cases, behaviour remains unchanged.
