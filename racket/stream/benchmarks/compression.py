#!/usr/bin/env python3
"""Compare compression with the pre-compression commit in fresh processes."""
import hashlib
import json
from pathlib import Path
import statistics
import subprocess
import sys
import tempfile

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[2]
RACKET = Path(sys.argv[1]).resolve()
BASE = "87cf037987bb7c234871f455543e6003b5ea9276"
source = subprocess.check_output(
    ["git", "show", BASE + ":racket/stream/private/two-state-cons.rkt"], cwd=REPO)
report = {"racket": str(RACKET), "baseline_commit": BASE,
          "baseline_sha256": hashlib.sha256(source).hexdigest(),
          "candidate_sha256": hashlib.sha256((HERE.parent / "private/two-state-cons.rkt").read_bytes()).hexdigest(),
          "rounds": 7,
          "method": "Sequential fresh processes; compile first; small and full warmups; seven CPU-time samples including GC with full GC between samples; root and cursor both fully force the root at the end.",
          "results": []}
with tempfile.TemporaryDirectory(prefix="stream-compression-") as directory:
    tmp = Path(directory)
    (tmp / "baseline-cons.rkt").write_bytes(source)
    facade = (HERE.parent / "two-state.rkt").read_text().replace(
        '"private/two-state-cons.rkt"', '(file ' + json.dumps(str(tmp / "baseline-cons.rkt")) + ')')
    (tmp / "baseline.rkt").write_text(facade)
    harness = f'''#lang racket/base
(module before racket/base
  (require racket/include (file {json.dumps(str(tmp / "baseline.rkt"))}))
  (include (file {json.dumps(str(HERE / "compression-workloads.rktl"))})))
(module after racket/base
  (require racket/include racket/stream/two-state)
  (include (file {json.dumps(str(HERE / "compression-workloads.rktl"))})))
(require json racket/match
         (prefix-in before: (submod "." before))
         (prefix-in after: (submod "." after)))
(match-define (list backend mode count) (vector->list (current-command-line-arguments)))
(define exercise (if (equal? backend "before") before:exercise after:exercise))
(define kind (string->symbol mode))
(define n (string->number count))
(void (exercise kind (max 1 (quotient n 10))))
(void (exercise kind n))
(define samples
  (for/list ([i (in-range 7)])
    (collect-garbage) (collect-garbage) (collect-garbage)
    (let-values ([(result cpu real gc) (time-apply exercise (list kind n))])
      (hasheq 'cpu_ms cpu 'real_ms real 'gc_ms gc 'checksum (car result)))))
(write-json (hasheq 'backend backend 'mode mode 'n n 'version (version) 'samples samples))
(newline)
'''
    entry = tmp / "main.rkt"
    entry.write_text(harness)
    subprocess.run([str(RACKET), "-l", "compiler/commands/make", "--", str(entry)], check=True, cwd=REPO)
    cases = [("cursor", 300000), ("cursor", 1000000),
             ("root", 5000), ("root", 10000), ("root", 20000),
             ("root", 300000), ("root", 600000)]
    for i, (mode, n) in enumerate(cases):
        medians = {}
        backends = (["before", "after"] if i % 2 == 0 else ["after", "before"])
        if mode == "root" and n > 20000:
            backends = ["after"]
        for backend in backends:
            output = subprocess.check_output([str(RACKET), str(entry), backend, mode, str(n)], cwd=REPO, text=True, timeout=120)
            item = json.loads(output)
            assert all(s["checksum"] == n for s in item["samples"])
            report["results"].append(item)
            medians[backend] = statistics.median(s["cpu_ms"] for s in item["samples"])
        print(mode, n, medians, flush=True)
        (HERE / "compression-results-20261107.json").write_text(json.dumps(report, indent=2) + "\n")
