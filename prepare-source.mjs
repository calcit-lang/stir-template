import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {mkdirSync,readFileSync,writeFileSync} from 'node:fs';
const run=(...args)=>execFileSync(process.env.STIR_QUERY_CLI||'calcit',args,{encoding:'utf8',maxBuffer:20*1024*1024,stdio:['ignore','pipe','ignore']});
const q=(...args)=>JSON.parse(run('query',...args,'--format','json')).data;
mkdirSync('acceptance',{recursive:true});
function inventory(testCount=2){
  const ds=[];
  for(const ns of q('ns').namespaces){
    const names=run('query','defs',ns).split('\n').filter(l=>/^  \S/.test(l)).map(l=>l.trim().split(' ')[0]);
    assert.equal(names.length,q('ns',ns).definition_count);
    for(const n of names)ds.push(q('def',`${ns}/${n}`));
  }
  assert.equal(ds.length,66);assert.equal(ds.reduce((n,d)=>n+d.tests.length,0),testCount);
  assert.equal(q('ns').namespaces.length,6);return ds;
}
if(process.argv[2]==='inventory'){
  const ds=inventory();writeFileSync('acceptance/reviewed.json',JSON.stringify(ds,null,2));
  console.log(JSON.stringify({definitions:ds.length,tests:ds.reduce((n,d)=>n+d.tests.length,0),namespaces:q('ns').namespaces}));
  process.exit(0);
}
assert.equal(run('-v').trim(),'0.30.1');run('docs','agents','--contract');
const config=q('config'),before=inventory();assert.deepEqual(before,JSON.parse(readFileSync('acceptance/reviewed.json','utf8')));
const expected=structuredClone(before),counts={map:0,count:0,merge:0};
function normalize(n){
  if(!Array.isArray(n))return n;
  const a=n.map(normalize);
  if(a[0]==='map'||a[0]==='merge'){counts[a[0]]++;a[0]=`.${a[0]}`;}
  else if(a[0]==='count'){assert.deepEqual(a,['count','props-in-string']);counts.count++;a[0]='.len';}
  return a;
}
for(const d of expected)d.code=normalize(d.code);
assert.deepEqual(counts,{map:7,count:1,merge:1});
const hint=(args,result)=>['hint-fn',['{}',[':args',['[]',...args]],[':return',result]]];
const element=['::',"'Map","'Tag","'Dynamic"];
const page=expected.find(d=>d.id==='stir-template.alias/make-page');
const callbacks=[];
function visit(n){if(!Array.isArray(n))return;if(n[0]==='.map'&&n.at(-1)?.[0]==='fn')callbacks.push(n.at(-1));n.forEach(visit);}
visit(page.code);assert.equal(callbacks.length,3);
assert.equal(callbacks[0][1][0],'path');assert.equal(callbacks[1][1][0],'content');
callbacks[0].splice(2,0,hint(["'String"],element));
callbacks[1].splice(2,0,hint(["'String"],element));
const style=expected.find(d=>d.id==='stir-template.core/style->string');
assert.equal(style.code[3][2][0],'.map');assert.equal(style.code[3][2][1][0],'fn');
style.code[3][2][1].splice(2,0,hint([['::',"'MapEntry","'Tag","'Dynamic"]],"'String"));
const operations=expected.filter((d,i)=>JSON.stringify(d.code)!==JSON.stringify(before[i].code)).map(d=>['edit','def',d.id,'--overwrite','--input-format','json-ast','--code',JSON.stringify(d.code)]);
assert.equal(operations.length,7);
for(const name of ['StirPageResources','StirScriptResource']){
  const d=expected.find(d=>d.id===`stir-template.schema/${name}`);assert.equal(d.code[0],'defstruct');assert.equal(d.schema,"'Struct");
  d.schema="'StructDef";operations.push(['edit','schema',d.id,'--input-format','json-ast','--code',JSON.stringify(d.schema)]);
}
const resources=['{}',[':content','|CONTENT'],[':styles',['[]','|/a.css']],[':inline-styles',['[]','|.a{color:red;}']],[':scripts',['[]','|/a.js',['{}',[':src','|/m.js'],[':type',':module'],[':defer?','true']]]]];
const testCode=['let',[['page',['make-page',resources]]],...['|/a.css','|.a{color:red;}','|/a.js','|/m.js','|CONTENT'].map(text=>['assert=','2',['.len',['split','page',text]]])];
assert.deepEqual(page.tests,[]);page.tests.push({code:testCode,name:'renders-page-resources',tags:['unit']});
operations.push(['edit','add-test',page.id,'renders-page-resources','--tags','unit','--input-format','json-ast','--code',JSON.stringify(testCode)]);
writeFileSync('acceptance/before.json',JSON.stringify(before,null,2));writeFileSync('acceptance/operations.json',JSON.stringify(operations,null,2));
const args=['edit','transaction','--file','acceptance/operations.json','--format','json'];
const preview=JSON.parse(run(...args,'--dry-run'));assert.ok(preview.scoped_revision);
writeFileSync('acceptance/preview.json',JSON.stringify(preview,null,2));writeFileSync('acceptance/applied.json',run(...args,'--expect-revision',preview.scoped_revision));
assert.deepEqual(inventory(3),expected,'Whole views:9heads/3concretecallbacks/2definition-kinds/1resource-case;originaltests/publiccontracts/variadic/escaping/HTML/config unchanged');assert.deepEqual(q('config'),config);
writeFileSync('acceptance/after.json',JSON.stringify(expected,null,2));writeFileSync('acceptance/snapshot-blob.txt',execFileSync('git',['hash-object','calcit.cirru'],{encoding:'utf8'}));
