"""Full application codec/forms and actual maintained HTTP-client transport."""
import copy
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import threading

root=Path(__file__).resolve().parents[1]
from fixtures import documents, invalid
requests=[];errors=[]
class Handler(BaseHTTPRequestHandler):
    def log_message(self,*args):pass
    def do_POST(self):
        value=json.loads(self.rfile.read(int(self.headers['Content-Length'])))
        requests.append((self.path,value))
        expected=next((wire for wire in documents if '/new/document/'+wire['dtype']==self.path),None)
        if value!=expected:errors.append((self.path,value,expected))
        body=json.dumps({'ok':True,'id':value['id']}).encode()
        self.send_response(201);self.send_header('Content-Type','application/json');self.send_header('Content-Length',str(len(body)));self.end_headers();self.wfile.write(body)
server=ThreadingHTTPServer(('127.0.0.1',0),Handler)
thread=threading.Thread(target=server.serve_forever,daemon=True);thread.start()
try:
    with tempfile.TemporaryDirectory() as directory:
        path=Path(directory)/'fixtures.json';path.write_text(json.dumps({'documents':documents,'invalid':invalid}))
        env=os.environ.copy();env.update(STAR_APP_FIXTURES=str(path),STAR_APP_TEST_API=f'http://127.0.0.1:{server.server_port}')
        subprocess.run([sys.argv[1],'--script',str(root/'t/contract-tests.lisp')],env=env,check=True,timeout=120)
    assert not errors,errors
    assert len(requests)==60,(len(requests),requests)
    assert {value['dtype'] for _,value in requests}=={wire['dtype'] for wire in documents}
    print('real UI codec/forms -> maintained CL client -> actual HTTP: all 60 source document types, no network submission of invalid documents PASS')
finally:
    server.shutdown();server.server_close();thread.join(timeout=5)
