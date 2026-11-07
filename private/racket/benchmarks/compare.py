#!/usr/bin/env python3
"""Compile one workload for stock, a fixed baseline, and an extension checkout."""
import argparse
import hashlib
import json
from pathlib import Path
import statistics
import subprocess
import tempfile

parser = argparse.ArgumentParser()
parser.add_argument("racket", type=Path)
parser.add_argument("--baseline", required=True, type=Path)
parser.add_argument("--candidate", required=True, type=Path)
parser.add_argument("--output", required=True, type=Path)
parser.add_argument("--rounds", type=int, default=7)
args = parser.parse_args()
racket = args.racket.resolve()
roots = {name: getattr(args, name).resolve() for name in ("baseline", "candidate")}
here = Path(__file__).resolve().parent

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def describe(root):
    return {"commit": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root, text=True).strip(),
            "sources": {name: digest(root / "private/racket" / name)
                        for name in ("stream.rkt", "private/stream-cons.rkt")}}

report = {"method": "Three backends, sequential fresh processes, shared compiled workload, small/full warmups, full GC between samples, CPU time includes GC; retained managed heap is not peak memory.",
          "rounds": args.rounds, **{name: describe(root) for name, root in roots.items()},
          "results": []}
with tempfile.TemporaryDirectory(prefix="stream-compare-") as directory:
    entry = Path(directory) / "main.rkt"
    modules = []
    for name in ("stock", "baseline", "candidate"):
        module = "racket/stream" if name == "stock" else "(file " + json.dumps(str(roots[name] / "private/racket/stream.rkt")) + ")"
        modules.append(f'''(module {name} racket/base
  (require racket/include {module})
  (include (file {json.dumps(str(here / "workloads.rktl"))})))''')
    entry.write_text('''#lang racket/base
''' + "\n".join(modules) + '''
(require json racket/match
         (prefix-in stock: (submod "." stock))
         (prefix-in baseline: (submod "." baseline))
         (prefix-in candidate: (submod "." candidate)))
(match-define (list backend workload count rounds)
  (vector->list (current-command-line-arguments)))
(define n (string->number count))
(define kind (string->symbol workload))
(define memory? (memq kind '(memory-chain memory-tail)))
(define actual-kind
  (case kind [(memory-chain) 'delay-chain] [(memory-tail) 'lazy-tail] [else kind]))
(define-values (exercise memory)
  (case (string->symbol backend)
    [(stock) (values stock:exercise stock:retained-memory)]
    [(baseline) (values baseline:exercise baseline:retained-memory)]
    [(candidate) (values candidate:exercise candidate:retained-memory)]))
(void (exercise actual-kind (max 1 (quotient n 10))))
(void (exercise actual-kind n))
(when memory? (void (memory actual-kind n)))
(define samples
  (for/list ([i (in-range (string->number rounds))])
    (collect-garbage) (collect-garbage) (collect-garbage)
    (if memory?
        (hasheq 'retained_bytes (memory actual-kind n))
        (let-values ([(result cpu real gc) (time-apply exercise (list kind n))])
          (hasheq 'cpu_ms cpu 'real_ms real 'gc_ms gc 'checksum (car result))))))
(write-json (hasheq 'backend backend 'workload workload 'n n
                    'version (version) 'samples samples))
(newline)
''')
    subprocess.run([str(racket), "-l", "compiler/commands/make", "--", str(entry)], check=True)
    cases = [("eager", 1000000), ("lazy-tail", 1000000), ("lazy-both", 1000000),
             ("delay-chain", 2000000), ("cached-head", 5000000),
             ("memory-chain", 300000), ("memory-tail", 300000)]
    for i, (workload, count) in enumerate(cases):
        order = ["stock", "baseline", "candidate"]
        order = order[i % 3:] + order[:i % 3]
        medians = {}
        results = []
        for backend in order:
            result = json.loads(subprocess.check_output(
                [str(racket), str(entry), backend, workload, str(count), str(args.rounds)],
                text=True, timeout=180))
            results.append(result)
            report["results"].append(result)
            metric = "retained_bytes" if workload.startswith("memory") else "cpu_ms"
            medians[backend] = statistics.median(x[metric] for x in result["samples"])
        if not workload.startswith("memory"):
            assert len({x["checksum"] for r in results for x in r["samples"]}) == 1
        print(workload, count, medians, flush=True)
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n")
