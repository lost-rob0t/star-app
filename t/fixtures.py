"""Representative documents derived solely from the vendored source schema."""
import copy
import json
from pathlib import Path

root=Path(__file__).resolve().parents[1]
schema=json.loads((root/'schemas/starintel-0.10.1/generated/schema.json').read_text())
manifest=json.loads((root/'schemas/starintel-0.10.1/generated/portable-manifest.json').read_text())
def sample(node):
    if '$ref' in node:return sample(schema['$defs'][node['$ref'].split('/')[-1]])
    if 'enum' in node:return node['enum'][0]
    if 'anyOf' in node:return sample(node['anyOf'][0])
    kind=node.get('type')
    if kind=='object':return {key:sample(node['properties'][key]) for key in node.get('required',[])}
    if kind=='array':return []
    if kind in ('integer','number'):return node.get('minimum',0)
    if kind=='boolean':return False
    if node.get('format')=='date-time':return '2026-10-03T12:00:00Z'
    if node.get('format')=='date':return '2026-10-03'
    if node.get('format')=='uri':return 'https://example.test/'
    if 'pattern' in node:
        if '@' in node['pattern']:return 'fixture@example.test'
        if '0-9().' in node['pattern']:return '+123456789'
        return '0'
    return 'fixture'
documents=[]
for contract in manifest['types']:
    if contract['kind']!='document':continue
    dtype=contract['name'].split('/')[-1]
    definition=''.join(part.capitalize() for part in dtype.split('-'))
    wire=sample(schema['$defs'][definition])
    wire.update(id='fixture:'+dtype,dataset='conformance',dtype=dtype,schemaVersion='0.10.1',deleted=False,
                extensions={'ui.fixture':{'flag':False,'nil':None,'items':[],'largeInteger':9007199254740993}})
    documents.append(wire)
assert len(documents)==60
person=next(wire for wire in documents if wire['dtype']=='person')
invalid=[]
for key,value in [('schemaVersion','0.10.2'),('dtype','unknown'),('_id','legacy'),('data',{}),
                  ('id','invalid space'),('createdAt',-1),('confidence','0.12345'),('confidence','1.1'),
                  ('sources',[{'id':'source'}]),('dob','2026-02-30')]:
    invalid.append({**copy.deepcopy(person),key:value})
missing=copy.deepcopy(person);del missing['dataset'];invalid.append(missing)
