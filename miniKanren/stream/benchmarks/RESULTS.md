# Runtime comparison: predicate correction in progress

The previous TRS1/TRS2 timing comparison was confounded by `define-predicate`
under no-check. On Racket 9.3 CS it expands to a predicate accepting only #f,
not the requested Data type. The original checked predicate and the no-check
predicate therefore took different paths through ext-s. Matching query answers
did not detect this because the occurs-check fallback still produces the same
substitutions. The previous figures are preserved as superseded raw data in
`results-20261107-superseded.json`; they must not be used to conclude that
ordinary checked TRS code is 5–10 times slower than equivalent no-check code.

`no-check-predicate-repro.rkt` reduces the issue to an Integer predicate:

```text
(checked (#f #t))
(no-check (#t #f))
```

The stream variants now spell Data's predicate explicitly, retaining its type
annotation. The driver also uses that same explicit predicate for a checked
original control and for the no-check original, and checks predicate behavior
before timing. The original checked/generated-predicate case remains a separate
reference. Corrected measurements will follow below.
