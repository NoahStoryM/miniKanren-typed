"""Verify the relocated baseline against the recorded upstream source hashes."""
import hashlib
import json
from pathlib import Path
root = Path(__file__).resolve().parents[1]
manifest = json.loads((root / "UPSTREAM.json").read_text())
for item in manifest["files"]:
    path = root / item["local_path"].removeprefix("private/racket/")
    data = path.read_bytes()
    assert hashlib.sha256(data).hexdigest() == item["baseline_sha256"], path
    if path == root / "stream.rkt":
        text = data.decode()
        for before, after in manifest["relocations"].items():
            assert text.count(after) == 1
            text = text.replace(after, before)
        data = text.encode()
    assert hashlib.sha256(data).hexdigest() == item["upstream_sha256"], path
    print(f"verified {item['local_path']}")
