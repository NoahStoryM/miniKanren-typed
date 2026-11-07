# Racket stream research baseline

```racket
(require private/racket/stream)
```

This branch copies the public `racket/stream` module and its native primitive
module from the official Racket **v9.3** tag. `UPSTREAM.json` records upstream
blob IDs, hashes, and the exact relocation map. The primitive file is unchanged;
the public file changes only three relative imports to the installed shared
stream/sequence/generic protocol modules. Its stream-cons dependency remains
local, so derived functions and macros use the copied constructors.

The initial copy is its own byte-for-byte commit. Package-layout preparation and
import relocation are separate commits. The baseline contains no peek/step
extensions or Kanren experiments. It has no require/expose or RackUnit runtime
dependency. The fixed baseline tag is `stream-baseline-racket-9.3`; the baseline
PR also records its immutable commit SHA.

## Verify before extending

With Racket 9.3 and this repository available as a multi-collection package:

```sh
python private/racket/tests/verify-baseline.py
raco test private/racket/tests/compatibility.rkt
```

The hash verifier intentionally fails once the sources are extended; run it on
the fixed baseline, not on extension branches. Compatibility checks run the same
fixtures against stock and copied streams, covering constructors, derived
operators/macros, multiple values, contracts, custom streams, errors, and sharing.

`benchmarks/compare.py` compiles a shared workload against stock, a fixed baseline
checkout, and a candidate checkout. It reports raw CPU/GC samples and retained
heap measurements; use it to distinguish copy overhead from extension overhead.
Neither the baseline nor the extension is an upstream Racket proposal yet.

```sh
python private/racket/benchmarks/compare.py /path/to/racket \
  --baseline /path/to/fixed-baseline-checkout \
  --candidate /path/to/extension-checkout \
  --output /tmp/stream-comparison.json
```

The three backends use the same compiled workload and fresh processes per case;
seven samples follow warmups and full GC. Compare baseline/candidate first when
estimating extension overhead. Shared-machine timings are not stable guarantees.
