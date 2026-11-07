#!/usr/bin/env python3
"""Diagnostic ablations of native-stream Kanren; variants are not public APIs."""
import argparse
import hashlib
import json
from pathlib import Path
import platform
import statistics
import subprocess
import tempfile

parser = argparse.ArgumentParser()
parser.add_argument('racket', type=Path)
parser.add_argument('--rounds', type=int, default=7)
parser.add_argument('--output', type=Path, required=True)
parser.add_argument('--editions', nargs='+', choices=['trs1', 'trs2'], default=['trs1', 'trs2'])
parser.add_argument('--workloads', nargs='+', choices=['answers', 'splits', 'branching'], default=['answers', 'splits', 'branching'])
parser.add_argument('--variants', nargs='+', choices=['stream', 'stream-once', 'stream-no-cache', 'stream-no-guards', 'stream-no-cycle', 'stream-no-result', 'stream-no-reentrance'], default=['stream', 'stream-once', 'stream-no-cache', 'stream-no-guards'])
args = parser.parse_args()
racket = args.racket.resolve()
root = Path(__file__).resolve().parents[3]
report = {
    'commit': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=root, text=True).strip(),
    'platform': platform.platform(), 'machine': platform.machine(),
    'rounds': args.rounds,
    'method': 'Compile before timing. Each edition/backend/workload runs in a fresh process. Both small and full warmups precede samples. Three full collections precede each sample. Timers run inside the implementation module; no typed/untyped contracts are crossed inside the timed region. Every sample builds a fresh goal/result; no stream is reused between samples. CPU includes GC. Source module loading and compilation are excluded. Backend order rotates across workloads. Ablations: stream-once reads first only once in ready clauses; stream-no-cache restores the thunk rather than saving its result, retaining safety checks; stream-no-guards keeps caching but omits reentrancy/result/cycle guards in single-step forcing.',
    'sources': {}, 'results': [],
    'warning': 'Diagnostic variants intentionally change invalid-input or memoization behavior. Only cold benchmark queries are compared; timings are not an additive cost decomposition.',
    'variants': args.variants, 'editions': args.editions, 'workloads': args.workloads,
    'predicate_control': 'Every backend uses the same explicit Data predicate. typed-manual enables type checking; no-check uses the original pair/thunk representation without checking.',
}

with tempfile.TemporaryDirectory(prefix='kanren-compare-') as directory:
    directory = Path(directory)
    def end_form(text, start):
        depth = 0
        quoted = False
        escaped = False
        for index in range(start, len(text)):
            c = text[index]
            if quoted:
                if escaped: escaped = False
                elif c == chr(92): escaped = True
                elif c == '"': quoted = False
            elif c == '"': quoted = True
            elif c in '([': depth += 1
            elif c in ')]':
                depth -= 1
                if depth == 0: return index + 1
        raise ValueError('unbalanced form')

    def peek_once(source):
        marker = '[(stream-peek-first s-inf)'
        offset = 0
        while True:
            start = source.find(marker, offset)
            if start < 0: return source
            end = end_form(source, start)
            branch = source[start:end]
            if '(car (stream-peek-first s-inf))' in branch:
                body = branch[len(marker):-1].strip().replace('(car (stream-peek-first s-inf))', '(car vs)')
                replacement = marker + '\n     => (λ (vs) ' + body + ')]'
                source = source[:start] + replacement + source[end:]
                offset = start + len(replacement)
            else:
                offset = end

    variants = args.variants
    for variant in variants:
        lib = directory / variant
        (lib / 'private').mkdir(parents=True)
        primitive = (root / 'private/racket/private/stream-cons.rkt').read_text()
        start = primitive.index('(define (stream-force-once source)')
        prefix, force = primitive[:start], primitive[start:]
        if variant == 'stream-no-cache':
            force = force.replace('(set-lazily-created-stream-thunk-or-stream! s result)',
                                  '(set-lazily-created-stream-thunk-or-stream! s thunk)')
            force = force.replace('     (set-lazily-created-stream-creator-if-unforced! s #f)\n', '')
            force = force.replace('     (compress-cached-prefix! source s result)\n', '')
            force = force.replace('     (compress-cached-prefix! source s s)\n', '')
        if variant in ['stream-no-guards', 'stream-no-reentrance']:
            force = force.replace('     (set-lazily-created-stream-thunk-or-stream! s reentrant-error)\n', '')
        markers = []
        if variant in ['stream-no-guards', 'stream-no-result']:
            markers.append('(unless (for:stream? result)')
        if variant in ['stream-no-guards', 'stream-no-cycle']:
            markers.append('(let check ([target result])')
        for marker in markers:
            a = force.index(marker)
            b = end_form(force, a)
            force = force[:a] + '(void)' + force[b:]
        (lib / 'private/stream-cons.rkt').write_text(prefix + force)
        (lib / 'stream.rkt').write_text((root / 'private/racket/stream.rkt').read_text())

    for edition in args.editions:
        original = root / ('typed/microKanren/main.rkt' if edition == 'micro' else f'typed/miniKanren/{edition}/main.rkt')
        candidate = root / ('microKanren/stream/main.rkt' if edition == 'micro' else f'miniKanren/stream/{edition}/main.rkt')
        report['sources'][edition] = {label: hashlib.sha256(path.read_bytes()).hexdigest()
                                      for label, path in [('original', original), ('stream', candidate)]}
        backends = ['typed-manual', 'no-check', *variants]
        for backend in backends:
            source = (candidate if backend.startswith('stream') else original).read_text()
            if backend.startswith('stream'):
                lib = directory / backend / 'stream.rkt'
                source = source.replace('private/racket/stream', '(file ' + json.dumps(str(lib)) + ')')
                if backend == 'stream-once': source = peek_once(source)
            if edition != 'micro' and backend in ['typed-manual', 'no-check']:
                source = source.replace('(define-predicate data? Data)', '''(: data? (→ Any Boolean : Data))
(define (data? v)
  (or (boolean? v) (number? v) (char? v) (bytes? v) (string? v)
      (keyword? v) (null? v) (symbol? v)
      (and (pair? v) (data? (car v)) (data? (cdr v)))))''')
            if backend == 'no-check':
                source = source.replace('#lang typed/racket/base', '#lang typed/racket/no-check', 1)
            if edition == 'trs2':
                repeat = '(defrel (repeat-o x) (conde [(== x \'a)] [(repeat-o x)]))'
                appendo_head = '(defrel (append-o l t out)'
                bits_head = '(defrel (bits-o [n : Natural] xs)'
                run_answers = "(run n q (repeat-o q))"
                run_splits = "(run* q (fresh (l t) (== (list l t) q) (append-o l t '(a b c d e f g h i j k l))))"
                run_bits = '(run* q (bits-o 7 q))'
            else:
                repeat = "(: repeat-o (→ Term Goal))\n(define (repeat-o x) (conde [(== x 'a)] [(repeat-o x)]))"
                appendo_head = '(: append-o (→ Term Term Term Goal))\n(define (append-o l t out)'
                bits_head = '(: bits-o (→ Natural Term Goal))\n(define (bits-o n xs)'
                run_answers = '(run n (q) (repeat-o q))'
                run_splits = "(run* (q) (fresh (l t) (== (list l t) q) (append-o l t '(a b c d e f g h i j k l))))"
                run_bits = '(run* (q) (bits-o 7 q))'
            predicate_check = '' if edition == 'micro' else "(unless (and (data? #t) (data? 1) (data? '()) (data? '(a b)) (not (data? (var 'x)))) (error 'preview \"Data predicate differs\"))"
            fixtures = f'''
;; Benchmark relations execute within the implementation's own module.
{repeat}
{appendo_head}
  (conde
    [(== '() l) (== t out)]
    [(fresh (a d res)
       (== (cons a d) l)
       (== (cons a res) out)
       (append-o d t res))]))
{bits_head}
  (if (zero? n)
      (== xs '())
      (fresh (a d)
        (== (cons a d) xs)
        (conde [(== a 0)] [(== a 1)])
        (bits-o (sub1 n) d))))

(: exercise (→ Symbol Natural Integer))
(define (exercise kind n)
  (case kind
    [(answers) (length {run_answers})]
    [(splits)
     (for/sum : Integer ([i (in-range n)]) (length {run_splits}))]
    [(branching)
     (for/sum : Integer ([i (in-range n)]) (length {run_bits}))]
    [else (error 'exercise "unknown workload")]))

(: preview (→ Symbol String))
(define (preview kind)
  {predicate_check}
  (define n 12)
  (format "~s"
          (case kind
            [(answers) {run_answers}]
            [(splits) {run_splits}]
            [(branching) {run_bits}])))

(: benchmark (→ Symbol Natural Natural (Listof (Listof Real))))
(define (benchmark kind n rounds)
  (void (exercise kind (max 1 (quotient n 10))))
  (void (exercise kind n))
  (for/list : (Listof (Listof Real)) ([i (in-range rounds)])
    (collect-garbage) (collect-garbage) (collect-garbage)
    (define cpu (current-process-milliseconds))
    (define real (current-inexact-milliseconds))
    (define gc (current-gc-milliseconds))
    (define checksum (exercise kind n))
    (list (- (current-process-milliseconds) cpu)
          (- (current-inexact-milliseconds) real)
          (- (current-gc-milliseconds) gc)
          checksum)))
'''
            module_path = directory / f'{edition}-{backend}.rkt'
            module_path.write_text(source + '\n' + fixtures)
            runner = directory / f'run-{edition}-{backend}.rkt'
            runner.write_text(f'''#lang racket/base
(require json (only-in (file {json.dumps(str(module_path))}) benchmark preview))
(define args (current-command-line-arguments))
(define kind (string->symbol (vector-ref args 0)))
(define n (string->number (vector-ref args 1)))
(define rounds (string->number (vector-ref args 2)))
(define expected (preview kind))
(define samples (benchmark kind n rounds))
(write-json (hasheq 'version (version) 'preview expected 'samples samples))
(newline)
''')
            subprocess.run([str(racket), '-l', 'compiler/commands/make', '--', str(runner)], check=True)
    cases = [('answers', 1000000), ('splits', 5000), ('branching', 1000)]
    cases = [(name, count) for name, count in cases if name in args.workloads]
    for edition in args.editions:
        for index, (workload, count) in enumerate(cases):
            order = ['typed-manual', 'no-check', *variants]
            order = order[index:] + order[:index]
            previews = set()
            for backend in order:
                runner = directory / f'run-{edition}-{backend}.rkt'
                data = json.loads(subprocess.check_output([str(racket), str(runner), workload,
                                                          str(count), str(args.rounds)], text=True))
                preview = data.pop('preview')
                previews.add(preview)
                samples = data.pop('samples')
                result = dict(edition=edition, backend=backend, workload=workload, n=count,
                              preview_sha256=hashlib.sha256(preview.encode()).hexdigest(),
                              **data, samples=[dict(zip(['cpu_ms', 'real_ms', 'gc_ms', 'checksum'], row))
                                               for row in samples])
                for metric in ['cpu_ms', 'real_ms', 'gc_ms']:
                    result['median_' + metric] = statistics.median(row[metric] for row in result['samples'])
                report['results'].append(result)
                args.output.write_text(json.dumps(report, indent=2) + '\n')
                print(f"{edition:5} {workload:9} {backend:8} CPU {result['median_cpu_ms']:6} ms, GC {result['median_gc_ms']:5} ms", flush=True)
            if len(previews) != 1:
                raise RuntimeError(f'Preview answers differ for {edition}/{workload}')
            checksums = {sample['checksum'] for r in report['results']
                         if r['edition'] == edition and r['workload'] == workload
                         for sample in r['samples']}
            if len(checksums) != 1:
                raise RuntimeError(f'Answer counts differ for {edition}/{workload}')
    report['answers_verified'] = True
    args.output.write_text(json.dumps(report, indent=2) + '\n')
