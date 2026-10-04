"""Actual installed CLOG UI in headless Chromium, using local HTTP fixtures."""
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import threading
import time
from urllib.parse import parse_qs,urlparse
from urllib.request import urlopen
from playwright.sync_api import sync_playwright, expect
from fixtures import documents, schema

binary,chromium=sys.argv[1:3]
message=next(wire.copy() for wire in documents if wire['dtype']=='message')
message.update(message='canonical browser message',group='fixture-group',channel='fixture-channel',
               user={'schema':'org.starintel/core@1/user','id':'fixture:author'},updatedAt=1780000000)
requests=[]
class Handler(BaseHTTPRequestHandler):
    def log_message(self,*args):pass
    def do_GET(self):
        parsed=urlparse(self.path);query=parse_qs(parsed.query);requests.append((parsed.path,query))
        if parsed.path=='/documents/messages/groups':
            value=[] if query.get('skip')==['100'] else [{'key':'fixture-group','value':['fixture-channel']}]
        elif parsed.path=='/documents/messages/by-channel':value=[message]
        elif parsed.path=='/search':value={'rows':[{'doc':message}],'bookmark':'fixture-bookmark'}
        else:self.send_error(404);return
        body=json.dumps(value).encode();self.send_response(200)
        self.send_header('Content-Type','application/json');self.send_header('Content-Length',str(len(body)))
        self.end_headers();self.wfile.write(body)
server=ThreadingHTTPServer(('127.0.0.1',0),Handler)
thread=threading.Thread(target=server.serve_forever,daemon=True);thread.start()
with socket.socket() as available:available.bind(('127.0.0.1',0));port=available.getsockname()[1]
env=os.environ.copy();env.update(STAR_APP_PORT=str(port),STARINTEL_API_URL=f'http://127.0.0.1:{server.server_port}')
env.pop('STAR_APP_OPEN_BROWSER',None)
diagnostics={"console":[],"requestFailures":[],"pageErrors":[],"websocketFrames":[],"websocketSent":[]}
page=None
with tempfile.TemporaryDirectory() as directory:
    log=Path(directory)/'application.log'
    with log.open('wb') as output:
        process=subprocess.Popen([binary],env=env,stdout=output,stderr=subprocess.STDOUT,cwd=directory)
        try:
            deadline=time.monotonic()+20
            while True:
                assert process.poll() is None,log.read_text(errors='replace')[-3000:]
                try:
                    with urlopen(f'http://127.0.0.1:{port}/editor',timeout=1) as response:
                        assert b'/js/boot.js' in response.read();break
                except OSError:
                    assert time.monotonic()<deadline,log.read_text(errors='replace')[-3000:]
                    time.sleep(.1)
            with sync_playwright() as playwright:
                browser=playwright.chromium.launch(executable_path=chromium,headless=True,
                          args=['--no-sandbox','--disable-dev-shm-usage'])
                page=browser.new_page();errors=[]
                page.on('pageerror',lambda error:(errors.append(str(error)),diagnostics['pageErrors'].append(str(error))))
                page.on('console',lambda event:diagnostics['console'].append(event.text) if event.type=='error' else None)
                page.on('requestfailed',lambda request:diagnostics['requestFailures'].append({'url':request.url,'failure':request.failure}))
                def websocket_observer(websocket):
                    def record_frame(key,frame):
                        diagnostics[key].append(str(frame)[:300])
                        del diagnostics[key][:-20]
                    websocket.on('framereceived',lambda frame:record_frame('websocketFrames',frame))
                    websocket.on('framesent',lambda frame:record_frame('websocketSent',frame))
                page.on('websocket',websocket_observer)
                # Keep the fixture offline; presentation-only CDN styles aren't needed
                # to validate CLOG transport, form controls or document semantics.
                page.route('**/*',lambda route:route.continue_() if route.request.url.startswith(f'http://127.0.0.1:{port}/') else route.abort())
                page.goto(f'http://127.0.0.1:{port}/editor')
                selector=page.locator('select.form-select')
                try:
                    expect(selector).to_be_visible(timeout=15000)
                except Exception:
                    diagnostics['dom']=page.evaluate("""() => ({
                        url: location.href,
                        html: document.documentElement.outerHTML.slice(0,4000),
                        selects: document.querySelectorAll('select.form-select').length,
                        bodies: document.querySelectorAll('body').length
                    })""")
                    raise
                # CLOG creates controls before registering their server callbacks.
                # Wait for the actual change handler instead of racing page startup.
                page.wait_for_function("""() => {
                    const select = document.querySelector('select.form-select');
                    return select && window.jQuery && jQuery._data(select, 'events')?.change?.length;
                }""")
                assert set(selector.locator('option').evaluate_all('(items)=>items.map(x=>x.value)'))=={wire['dtype'] for wire in documents}
                for wire in documents:
                    dtype=wire['dtype'];selector.select_option(dtype)
                    expect(page.locator('input[name="dtype"]').first).to_have_value(dtype)
                    definition=''.join(part.capitalize() for part in dtype.split('-'))
                    fields=set(schema['$defs'][definition]['properties'])
                    expect(page.locator('form [name]')).to_have_count(len(fields))
                    assert set(page.locator('form [name]').evaluate_all('(items)=>items.map(x=>x.name)'))==fields,dtype
                selector.select_option('person')
                expect(page.locator('input[name="dtype"]').first).to_have_value('person')
                form=page.locator('form.form-horizontal').first
                form.locator('[name="id"]').fill('browser:person')
                form.locator('[name="dataset"]').fill('browser')
                form.locator('[name="deleted"]').fill('false')
                opaque={'browser.fixture':{'flag':False,'nil':None,'items':[]}}
                form.locator('[name="extensions"]').fill(json.dumps(opaque))
                form.locator('[name="confidence"]').fill('1.1')
                form.get_by_role('button',name='Add Document').click()
                expect(page.locator('.toast').first).to_contain_text('maximum')
                assert page.locator('.card').count()==0
                form.locator('[name="confidence"]').fill('0.1234')
                form.get_by_role('button',name='Add Document').click()
                expect(page.locator('.toast').first).to_have_text('Document validated')
                card=page.locator('.card').first;expect(card).to_be_visible()
                expect(card.locator('[name="deleted"]')).to_have_value('false')
                assert json.loads(card.locator('[name="extensions"]').input_value())==opaque
                expect(card.locator('[name="confidence"]')).to_have_value('0.1234')
                page.goto(f'http://127.0.0.1:{port}/chat')
                page.get_by_text('fixture-group',exact=True).click(timeout=15000)
                page.get_by_text('fixture-channel',exact=True).click()
                expect(page.get_by_text('canonical browser message',exact=True)).to_be_visible()
                expect(page.get_by_text('fixture:author',exact=False)).to_be_visible()
                page.goto(f'http://127.0.0.1:{port}/search')
                search=page.locator('#search-bar');expect(search).to_be_visible(timeout=15000)
                search.fill('message:canonical');search.press('Enter')
                expect(page.locator('.results-container')).to_contain_text('canonical browser message')
                assert not errors,errors
                browser.close()
            assert any(path=='/documents/messages/by-channel' for path,_ in requests),requests
            assert any(path=='/search' for path,_ in requests),requests
            print('installed executable + actual Chromium/CLOG WebSocket: all 60 source form fields; invalid/valid editor submission with false/null/empty evidence; canonical chat references/message and search; configured real HTTP client PASS')
        except BaseException:
            if page is not None:
                try:diagnostics['body']=page.locator('body').inner_text(timeout=1000)[:1500]
                except Exception as error:diagnostics['bodyError']=str(error)[:500]
            print(json.dumps(diagnostics),file=sys.stderr)
            print(log.read_text(errors='replace')[-6000:],file=sys.stderr);raise
        finally:
            if process.poll() is None:
                process.terminate()
                try:process.wait(timeout=5)
                except subprocess.TimeoutExpired:process.kill();process.wait(timeout=5)
            server.shutdown();server.server_close();thread.join(timeout=5)
