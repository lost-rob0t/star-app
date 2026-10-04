"""Check the exact source release consumed by both compiled CL dependencies."""
import hashlib
import json
from pathlib import Path
import sys
root=Path(__file__).resolve().parents[1]
lock=json.loads((root/'schema/starintel-schema.lock.json').read_text())
for dependency in map(Path,sys.argv[1:]):
    assert lock==json.loads((dependency/'schema/starintel-schema.lock.json').read_text()),str(dependency)
    for local,entry in lock['vendored_files'].items():
        assert hashlib.sha256((dependency/local).read_bytes()).hexdigest()==entry['sha256'],local
nodes=json.loads((root/'flake.lock').read_text())['nodes']
assert nodes['star-cl']['locked']['rev']=='a18a6cddcbcf4d989495a5de8abcedc4d0294d1c'
assert nodes['starintel-server']['locked']['rev']=='117057d6f5bbf2e2b40d5cfd89eb7adcca5e28fd'
assert nodes['starintel-server']['inputs']['star-cl']==['star-cl']
print('application, compiled CL runtime and server client agree on every release artifact')
