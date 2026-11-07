#!/usr/bin/env python3
"""Run sequential fresh-process comparisons; write metadata and raw samples.

From repository root:
  python experiments/stream-benchmarks/run.py /path/to/racket
Compile benchmarks/main.rkt first to exclude source compilation.
"""
import hashlib
import json
import platform
import statistics
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
RACKET = Path(sys.argv[1]).resolve()
CASES = [
    ("eager", 1_000_000),
    ("lazy-tail", 1_000_000),
    ("lazy-both", 1_000_000),
    ("delay-chain", 2_000_000),
    ("cached-head", 5_000_000),
    ("memory-chain", 100_000),
    ("memory-chain", 300_000),
    ("memory-tail", 300_000),
]
report = {
    "source_commit": subprocess.check_output(
        ["git", "rev-parse", "HEAD"], cwd=REPO, text=True
    ).strip(),
    "machine": platform.machine(),
    "rounds": 7,
    "method": "Sequential fresh processes; small and full warmups; medians of seven samples; full GC between samples. Memory is retained managed-heap delta after full GC with source retained, not peak RSS.",
    "fork_sha256": hashlib.sha256(
        (HERE / "private/two-state-cons.rkt").read_bytes()
    ).hexdigest(),
    "stock_source_sha256": hashlib.sha256(
        (RACKET.parent.parent / "collects/racket/private/stream-cons.rkt").read_bytes()
    ).hexdigest(),
    "results": [],
}
for index, (workload, count) in enumerate(CASES):
    pair = {}
    for backend in (["stock", "fork"] if index % 2 == 0 else ["fork", "stock"]):
        completed = subprocess.run(
            [str(RACKET), str(HERE / "main.rkt"), backend, workload, str(count), "7"],
            cwd=REPO, capture_output=True, text=True, check=True, timeout=120,
        )
        result = json.loads(completed.stdout)
        report["results"].append(result)
        metric = "retained_bytes" if workload.startswith("memory") else "cpu_ms"
        pair[backend] = statistics.median(sample[metric] for sample in result["samples"])
    if not workload.startswith("memory"):
        results = report["results"][-2:]
        checksums = {sample["checksum"] for result in results for sample in result["samples"]}
        assert len(checksums) == 1, (workload, checksums)
    print(workload, count, pair, flush=True)
    (HERE / "two-state-results-20261107.json").write_text(json.dumps(report, indent=2) + "\n")
