var H=class c{static read_bytes(t,r){let s=new c;return s.buf=t.getUint32(r,!0),s.buf_len=t.getUint32(r+4,!0),s}static read_bytes_array(t,r,s){let f=[];for(let e=0;e<s;e++)f.push(c.read_bytes(t,r+8*e));return f}},Y=class c{static read_bytes(t,r){let s=new c;return s.buf=t.getUint32(r,!0),s.buf_len=t.getUint32(r+4,!0),s}static read_bytes_array(t,r,s){let f=[];for(let e=0;e<s;e++)f.push(c.read_bytes(t,r+8*e));return f}},et=0,rt=1,v=2;var nt=2,S=3,F=4;var U=class{head_length(){return 24}name_length(){return this.dir_name.byteLength}write_head_bytes(t,r){t.setBigUint64(r,this.d_next,!0),t.setBigUint64(r+8,this.d_ino,!0),t.setUint32(r+16,this.dir_name.length,!0),t.setUint8(r+20,this.d_type)}write_name_bytes(t,r,s){t.set(this.dir_name.slice(0,Math.min(this.dir_name.byteLength,s)),r)}constructor(t,r,s,f){let e=new TextEncoder().encode(s);this.d_next=t,this.d_ino=r,this.d_namlen=e.byteLength,this.d_type=f,this.dir_name=e}};var st=1;var A=class{write_bytes(t,r){t.setUint8(r,this.fs_filetype),t.setUint16(r+2,this.fs_flags,!0),t.setBigUint64(r+8,this.fs_rights_base,!0),t.setBigUint64(r+16,this.fs_rights_inherited,!0)}constructor(t,r){this.fs_rights_base=0n,this.fs_rights_inherited=0n,this.fs_filetype=t,this.fs_flags=r}};var q=1,P=2,it=4,X=8,y=class{write_bytes(t,r){t.setBigUint64(r,this.dev,!0),t.setBigUint64(r+8,this.ino,!0),t.setUint8(r+16,this.filetype),t.setBigUint64(r+24,this.nlink,!0),t.setBigUint64(r+32,this.size,!0),t.setBigUint64(r+38,this.atim,!0),t.setBigUint64(r+46,this.mtim,!0),t.setBigUint64(r+52,this.ctim,!0)}constructor(t,r,s){this.dev=0n,this.nlink=0n,this.atim=0n,this.mtim=0n,this.ctim=0n,this.ino=t,this.filetype=r,this.size=s}},dt=0;var Rt=1,Z=class c{static read_bytes(t,r){return new c(t.getBigUint64(r,!0),t.getUint8(r+8),t.getUint32(r+16,!0),t.getBigUint64(r+24,!0),t.getUint16(r+36,!0))}constructor(t,r,s,f,e){this.userdata=t,this.eventtype=r,this.clockid=s,this.timeout=f,this.flags=e}},j=class{write_bytes(t,r){t.setBigUint64(r,this.userdata,!0),t.setUint16(r+8,this.error,!0),t.setUint8(r+10,this.eventtype)}constructor(t,r,s){this.userdata=t,this.error=r,this.eventtype=s}};var Tt=0,tt=class{write_bytes(t,r){t.setUint32(r,this.pr_name.byteLength,!0)}constructor(t){this.pr_name=new TextEncoder().encode(t)}},K=class c{static dir(t){let r=new c;return r.tag=Tt,r.inner=new tt(t),r}write_bytes(t,r){t.setUint32(r,this.tag,!0),this.inner.write_bytes(t,r+4)}};var xt=class{enable(t){this.log=It(t===void 0?!0:t,this.prefix)}get enabled(){return this.isEnabled}constructor(t){this.isEnabled=t,this.prefix="wasi:",this.enable(t)}};function It(c,t){return c?console.log.bind(console,"%c%s","color: #265BA0",t):()=>{}}var m=new xt(!1);var V=class extends Error{constructor(t){super("exit with exit code "+t),this.code=t}},_t=class{start(t){this.inst=t;try{return t.exports._start(),0}catch(r){if(r instanceof V)return r.code;throw r}}initialize(t){this.inst=t,t.exports._initialize&&t.exports._initialize()}constructor(t,r,s,f={}){this.args=[],this.env=[],this.fds=[],m.enable(f.debug),this.args=t,this.env=r,this.fds=s;let e=this;this.wasiImport={args_sizes_get(n,i){let o=new DataView(e.inst.exports.memory.buffer);o.setUint32(n,e.args.length,!0);let a=0;for(let _ of e.args)a+=_.length+1;return o.setUint32(i,a,!0),m.log(o.getUint32(n,!0),o.getUint32(i,!0)),0},args_get(n,i){let o=new DataView(e.inst.exports.memory.buffer),a=new Uint8Array(e.inst.exports.memory.buffer),_=i;for(let l=0;l<e.args.length;l++){o.setUint32(n,i,!0),n+=4;let d=new TextEncoder().encode(e.args[l]);a.set(d,i),o.setUint8(i+d.length,0),i+=d.length+1}return m.enabled&&m.log(new TextDecoder("utf-8").decode(a.slice(_,i))),0},environ_sizes_get(n,i){let o=new DataView(e.inst.exports.memory.buffer);o.setUint32(n,e.env.length,!0);let a=0;for(let _ of e.env)a+=new TextEncoder().encode(_).length+1;return o.setUint32(i,a,!0),m.log(o.getUint32(n,!0),o.getUint32(i,!0)),0},environ_get(n,i){let o=new DataView(e.inst.exports.memory.buffer),a=new Uint8Array(e.inst.exports.memory.buffer),_=i;for(let l=0;l<e.env.length;l++){o.setUint32(n,i,!0),n+=4;let d=new TextEncoder().encode(e.env[l]);a.set(d,i),o.setUint8(i+d.length,0),i+=d.length+1}return m.enabled&&m.log(new TextDecoder("utf-8").decode(a.slice(_,i))),0},clock_res_get(n,i){let o;switch(n){case 1:{o=5000n;break}case 0:{o=1000000n;break}default:return 52}return new DataView(e.inst.exports.memory.buffer).setBigUint64(i,o,!0),0},clock_time_get(n,i,o){let a=new DataView(e.inst.exports.memory.buffer);if(n===0)a.setBigUint64(o,BigInt(new Date().getTime())*1000000n,!0);else if(n==1){let _;try{_=BigInt(Math.round(performance.now()*1e6))}catch{_=0n}a.setBigUint64(o,_,!0)}else a.setBigUint64(o,0n,!0);return 0},fd_advise(n,i,o,a){return e.fds[n]!=null?0:8},fd_allocate(n,i,o){return e.fds[n]!=null?e.fds[n].fd_allocate(i,o):8},fd_close(n){if(e.fds[n]!=null){let i=e.fds[n].fd_close();return e.fds[n]=void 0,i}else return 8},fd_datasync(n){return e.fds[n]!=null?e.fds[n].fd_sync():8},fd_fdstat_get(n,i){if(e.fds[n]!=null){let{ret:o,fdstat:a}=e.fds[n].fd_fdstat_get();return a?.write_bytes(new DataView(e.inst.exports.memory.buffer),i),o}else return 8},fd_fdstat_set_flags(n,i){return e.fds[n]!=null?e.fds[n].fd_fdstat_set_flags(i):8},fd_fdstat_set_rights(n,i,o){return e.fds[n]!=null?e.fds[n].fd_fdstat_set_rights(i,o):8},fd_filestat_get(n,i){if(e.fds[n]!=null){let{ret:o,filestat:a}=e.fds[n].fd_filestat_get();return a?.write_bytes(new DataView(e.inst.exports.memory.buffer),i),o}else return 8},fd_filestat_set_size(n,i){return e.fds[n]!=null?e.fds[n].fd_filestat_set_size(i):8},fd_filestat_set_times(n,i,o,a){return e.fds[n]!=null?e.fds[n].fd_filestat_set_times(i,o,a):8},fd_pread(n,i,o,a,_){let l=new DataView(e.inst.exports.memory.buffer),d=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let R=H.read_bytes_array(l,i,o),E=0;for(let p of R){let{ret:N,data:O}=e.fds[n].fd_pread(p.buf_len,a);if(N!=0)return l.setUint32(_,E,!0),N;if(d.set(O,p.buf),E+=O.length,a+=BigInt(O.length),O.length!=p.buf_len)break}return l.setUint32(_,E,!0),0}else return 8},fd_prestat_get(n,i){let o=new DataView(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let{ret:a,prestat:_}=e.fds[n].fd_prestat_get();return _?.write_bytes(o,i),a}else return 8},fd_prestat_dir_name(n,i,o){if(e.fds[n]!=null){let{ret:a,prestat:_}=e.fds[n].fd_prestat_get();if(_==null)return a;let l=_.inner.pr_name;return new Uint8Array(e.inst.exports.memory.buffer).set(l.slice(0,o),i),l.byteLength>o?37:0}else return 8},fd_pwrite(n,i,o,a,_){let l=new DataView(e.inst.exports.memory.buffer),d=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let R=Y.read_bytes_array(l,i,o),E=0;for(let p of R){let N=d.slice(p.buf,p.buf+p.buf_len),{ret:O,nwritten:L}=e.fds[n].fd_pwrite(N,a);if(O!=0)return l.setUint32(_,E,!0),O;if(E+=L,a+=BigInt(L),L!=N.byteLength)break}return l.setUint32(_,E,!0),0}else return 8},fd_read(n,i,o,a){let _=new DataView(e.inst.exports.memory.buffer),l=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let d=H.read_bytes_array(_,i,o),R=0;for(let E of d){let{ret:p,data:N}=e.fds[n].fd_read(E.buf_len);if(p!=0)return _.setUint32(a,R,!0),p;if(l.set(N,E.buf),R+=N.length,N.length!=E.buf_len)break}return _.setUint32(a,R,!0),0}else return 8},fd_readdir(n,i,o,a,_){let l=new DataView(e.inst.exports.memory.buffer),d=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let R=0;for(;;){let{ret:E,dirent:p}=e.fds[n].fd_readdir_single(a);if(E!=0)return l.setUint32(_,R,!0),E;if(p==null)break;if(o-R<p.head_length()){R=o;break}let N=new ArrayBuffer(p.head_length());if(p.write_head_bytes(new DataView(N),0),d.set(new Uint8Array(N).slice(0,Math.min(N.byteLength,o-R)),i),i+=p.head_length(),R+=p.head_length(),o-R<p.name_length()){R=o;break}p.write_name_bytes(d,i,o-R),i+=p.name_length(),R+=p.name_length(),a=p.d_next}return l.setUint32(_,R,!0),0}else return 8},fd_renumber(n,i){if(e.fds[n]!=null&&e.fds[i]!=null){let o=e.fds[i].fd_close();return o!=0?o:(e.fds[i]=e.fds[n],e.fds[n]=void 0,0)}else return 8},fd_seek(n,i,o,a){let _=new DataView(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let{ret:l,offset:d}=e.fds[n].fd_seek(i,o);return _.setBigInt64(a,d,!0),l}else return 8},fd_sync(n){return e.fds[n]!=null?e.fds[n].fd_sync():8},fd_tell(n,i){let o=new DataView(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let{ret:a,offset:_}=e.fds[n].fd_tell();return o.setBigUint64(i,_,!0),a}else return 8},fd_write(n,i,o,a){let _=new DataView(e.inst.exports.memory.buffer),l=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let d=Y.read_bytes_array(_,i,o),R=0;for(let E of d){let p=l.slice(E.buf,E.buf+E.buf_len),{ret:N,nwritten:O}=e.fds[n].fd_write(p);if(N!=0)return _.setUint32(a,R,!0),N;if(R+=O,O!=p.byteLength)break}return _.setUint32(a,R,!0),0}else return 8},path_create_directory(n,i,o){let a=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let _=new TextDecoder("utf-8").decode(a.slice(i,i+o));return e.fds[n].path_create_directory(_)}else return 8},path_filestat_get(n,i,o,a,_){let l=new DataView(e.inst.exports.memory.buffer),d=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let R=new TextDecoder("utf-8").decode(d.slice(o,o+a)),{ret:E,filestat:p}=e.fds[n].path_filestat_get(i,R);return p?.write_bytes(l,_),E}else return 8},path_filestat_set_times(n,i,o,a,_,l,d){let R=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let E=new TextDecoder("utf-8").decode(R.slice(o,o+a));return e.fds[n].path_filestat_set_times(i,E,_,l,d)}else return 8},path_link(n,i,o,a,_,l,d){let R=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null&&e.fds[_]!=null){let E=new TextDecoder("utf-8").decode(R.slice(o,o+a)),p=new TextDecoder("utf-8").decode(R.slice(l,l+d)),{ret:N,inode_obj:O}=e.fds[n].path_lookup(E,i);return O==null?N:e.fds[_].path_link(p,O,!1)}else return 8},path_open(n,i,o,a,_,l,d,R,E){let p=new DataView(e.inst.exports.memory.buffer),N=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let O=new TextDecoder("utf-8").decode(N.slice(o,o+a));m.log(O);let{ret:L,fd_obj:mt}=e.fds[n].path_open(i,O,_,l,d,R);if(L!=0)return L;e.fds.push(mt);let St=e.fds.length-1;return p.setUint32(E,St,!0),0}else return 8},path_readlink(n,i,o,a,_,l){let d=new DataView(e.inst.exports.memory.buffer),R=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let E=new TextDecoder("utf-8").decode(R.slice(i,i+o));m.log(E);let{ret:p,data:N}=e.fds[n].path_readlink(E);if(N!=null){let O=new TextEncoder().encode(N);if(O.length>_)return d.setUint32(l,0,!0),8;R.set(O,a),d.setUint32(l,O.length,!0)}return p}else return 8},path_remove_directory(n,i,o){let a=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let _=new TextDecoder("utf-8").decode(a.slice(i,i+o));return e.fds[n].path_remove_directory(_)}else return 8},path_rename(n,i,o,a,_,l){let d=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null&&e.fds[a]!=null){let R=new TextDecoder("utf-8").decode(d.slice(i,i+o)),E=new TextDecoder("utf-8").decode(d.slice(_,_+l)),{ret:p,inode_obj:N}=e.fds[n].path_unlink(R);if(N==null)return p;if(p=e.fds[a].path_link(E,N,!0),p!=0&&e.fds[n].path_link(R,N,!0)!=0)throw"path_link should always return success when relinking an inode back to the original place";return p}else return 8},path_symlink(n,i,o,a,_){let l=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[o]!=null){let d=new TextDecoder("utf-8").decode(l.slice(n,n+i)),R=new TextDecoder("utf-8").decode(l.slice(a,a+_));return 58}else return 8},path_unlink_file(n,i,o){let a=new Uint8Array(e.inst.exports.memory.buffer);if(e.fds[n]!=null){let _=new TextDecoder("utf-8").decode(a.slice(i,i+o));return e.fds[n].path_unlink_file(_)}else return 8},poll_oneoff(n,i,o){if(o===0)return 28;if(o>1)return m.log("poll_oneoff: only a single subscription is supported"),58;let a=new DataView(e.inst.exports.memory.buffer),_=Z.read_bytes(a,n),l=_.eventtype,d=_.clockid,R=_.timeout;if(l!==dt)return m.log("poll_oneoff: only clock subscriptions are supported"),58;let E;if(d===1)E=()=>BigInt(Math.round(performance.now()*1e6));else if(d===0)E=()=>BigInt(new Date().getTime())*1000000n;else return 28;let p=(_.flags&Rt)!==0?R:E()+R;for(;p>E(););return new j(_.userdata,0,l).write_bytes(a,i),0},proc_exit(n){throw new V(n)},proc_raise(n){throw"raised signal "+n},sched_yield(){},random_get(n,i){let o=new Uint8Array(e.inst.exports.memory.buffer).subarray(n,n+i);if("crypto"in globalThis&&(typeof SharedArrayBuffer>"u"||!(e.inst.exports.memory.buffer instanceof SharedArrayBuffer)))for(let a=0;a<i;a+=65536)crypto.getRandomValues(o.subarray(a,a+65536));else for(let a=0;a<i;a++)o[a]=Math.random()*256|0},sock_recv(n,i,o){throw"sockets not supported"},sock_send(n,i,o){throw"sockets not supported"},sock_shutdown(n,i){throw"sockets not supported"},sock_accept(n,i){throw"sockets not supported"}}}};var g=class{fd_allocate(t,r){return 58}fd_close(){return 0}fd_fdstat_get(){return{ret:58,fdstat:null}}fd_fdstat_set_flags(t){return 58}fd_fdstat_set_rights(t,r){return 58}fd_filestat_get(){return{ret:58,filestat:null}}fd_filestat_set_size(t){return 58}fd_filestat_set_times(t,r,s){return 58}fd_pread(t,r){return{ret:58,data:new Uint8Array}}fd_prestat_get(){return{ret:58,prestat:null}}fd_pwrite(t,r){return{ret:58,nwritten:0}}fd_read(t){return{ret:58,data:new Uint8Array}}fd_readdir_single(t){return{ret:58,dirent:null}}fd_seek(t,r){return{ret:58,offset:0n}}fd_sync(){return 0}fd_tell(){return{ret:58,offset:0n}}fd_write(t){return{ret:58,nwritten:0}}path_create_directory(t){return 58}path_filestat_get(t,r){return{ret:58,filestat:null}}path_filestat_set_times(t,r,s,f,e){return 58}path_link(t,r,s){return 58}path_unlink(t){return{ret:58,inode_obj:null}}path_lookup(t,r){return{ret:58,inode_obj:null}}path_open(t,r,s,f,e,n){return{ret:54,fd_obj:null}}path_readlink(t){return{ret:58,data:null}}path_remove_directory(t){return 58}path_rename(t,r,s){return 58}path_unlink_file(t){return 58}},T=class c{static issue_ino(){return c.next_ino++}static root_ino(){return 0n}constructor(){this.ino=c.issue_ino()}};T.next_ino=1n;var G=class extends g{fd_allocate(t,r){if(!(this.file.size>t+r)){let s=new Uint8Array(Number(t+r));s.set(this.file.data,0),this.file.data=s}return 0}fd_fdstat_get(){return{ret:0,fdstat:new A(F,0)}}fd_filestat_set_size(t){if(this.file.size>t)this.file.data=new Uint8Array(this.file.data.buffer.slice(0,Number(t)));else{let r=new Uint8Array(Number(t));r.set(this.file.data,0),this.file.data=r}return 0}fd_read(t){let r=this.file.data.slice(Number(this.file_pos),Number(this.file_pos+BigInt(t)));return this.file_pos+=BigInt(r.length),{ret:0,data:r}}fd_pread(t,r){return{ret:0,data:this.file.data.slice(Number(r),Number(r+BigInt(t)))}}fd_seek(t,r){let s;switch(r){case et:s=t;break;case rt:s=this.file_pos+t;break;case v:s=BigInt(this.file.data.byteLength)+t;break;default:return{ret:28,offset:0n}}return s<0?{ret:28,offset:0n}:(this.file_pos=s,{ret:0,offset:this.file_pos})}fd_tell(){return{ret:0,offset:this.file_pos}}fd_write(t){if(this.file.readonly)return{ret:8,nwritten:0};if(this.file_pos+BigInt(t.byteLength)>this.file.size){let r=this.file.data;this.file.data=new Uint8Array(Number(this.file_pos+BigInt(t.byteLength))),this.file.data.set(r)}return this.file.data.set(t,Number(this.file_pos)),this.file_pos+=BigInt(t.byteLength),{ret:0,nwritten:t.byteLength}}fd_pwrite(t,r){if(this.file.readonly)return{ret:8,nwritten:0};if(r+BigInt(t.byteLength)>this.file.size){let s=this.file.data;this.file.data=new Uint8Array(Number(r+BigInt(t.byteLength))),this.file.data.set(s)}return this.file.data.set(t,Number(r)),{ret:0,nwritten:t.byteLength}}fd_filestat_get(){return{ret:0,filestat:this.file.stat()}}constructor(t){super(),this.file_pos=0n,this.file=t}},M=class extends g{fd_seek(t,r){return{ret:8,offset:0n}}fd_tell(){return{ret:8,offset:0n}}fd_allocate(t,r){return 8}fd_fdstat_get(){return{ret:0,fdstat:new A(S,0)}}fd_readdir_single(t){if(m.enabled&&(m.log("readdir_single",t),m.log(t,this.dir.contents.keys())),t==0n)return{ret:0,dirent:new U(1n,this.dir.ino,".",S)};if(t==1n)return{ret:0,dirent:new U(2n,this.dir.parent_ino(),"..",S)};if(t>=BigInt(this.dir.contents.size)+2n)return{ret:0,dirent:null};let[r,s]=Array.from(this.dir.contents.entries())[Number(t-2n)];return{ret:0,dirent:new U(t+1n,s.ino,r,s.stat().filetype)}}path_filestat_get(t,r){let{ret:s,path:f}=C.from(r);if(f==null)return{ret:s,filestat:null};let{ret:e,entry:n}=this.dir.get_entry_for_path(f);return n==null?{ret:e,filestat:null}:{ret:0,filestat:n.stat()}}path_lookup(t,r){let{ret:s,path:f}=C.from(t);if(f==null)return{ret:s,inode_obj:null};let{ret:e,entry:n}=this.dir.get_entry_for_path(f);return n==null?{ret:e,inode_obj:null}:{ret:0,inode_obj:n}}path_open(t,r,s,f,e,n){let{ret:i,path:o}=C.from(r);if(o==null)return{ret:i,fd_obj:null};let{ret:a,entry:_}=this.dir.get_entry_for_path(o);if(_==null){if(a!=44)return{ret:a,fd_obj:null};if((s&q)==q){let{ret:l,entry:d}=this.dir.create_entry_for_path(r,(s&P)==P);if(d==null)return{ret:l,fd_obj:null};_=d}else return{ret:44,fd_obj:null}}else if((s&it)==it)return{ret:20,fd_obj:null};return(s&P)==P&&_.stat().filetype!==S?{ret:54,fd_obj:null}:_.path_open(s,f,n)}path_create_directory(t){return this.path_open(0,t,q|P,0n,0n,0).ret}path_link(t,r,s){let{ret:f,path:e}=C.from(t);if(e==null)return f;if(e.is_dir)return 44;let{ret:n,parent_entry:i,filename:o,entry:a}=this.dir.get_parent_dir_and_entry_for_path(e,!0);if(i==null||o==null)return n;if(a!=null){let _=r.stat().filetype==S,l=a.stat().filetype==S;if(_&&l)if(s&&a instanceof B){if(a.contents.size!=0)return 55}else return 20;else{if(_&&!l)return 54;if(!_&&l)return 31;if(!(r.stat().filetype==F&&a.stat().filetype==F))return 20}}return!s&&r.stat().filetype==S?63:(i.contents.set(o,r),0)}path_unlink(t){let{ret:r,path:s}=C.from(t);if(s==null)return{ret:r,inode_obj:null};let{ret:f,parent_entry:e,filename:n,entry:i}=this.dir.get_parent_dir_and_entry_for_path(s,!0);return e==null||n==null?{ret:f,inode_obj:null}:i==null?{ret:44,inode_obj:null}:(e.contents.delete(n),{ret:0,inode_obj:i})}path_unlink_file(t){let{ret:r,path:s}=C.from(t);if(s==null)return r;let{ret:f,parent_entry:e,filename:n,entry:i}=this.dir.get_parent_dir_and_entry_for_path(s,!1);return e==null||n==null||i==null?f:i.stat().filetype===S?31:(e.contents.delete(n),0)}path_remove_directory(t){let{ret:r,path:s}=C.from(t);if(s==null)return r;let{ret:f,parent_entry:e,filename:n,entry:i}=this.dir.get_parent_dir_and_entry_for_path(s,!1);return e==null||n==null||i==null?f:!(i instanceof B)||i.stat().filetype!==S?54:i.contents.size!==0?55:e.contents.delete(n)?0:44}fd_filestat_get(){return{ret:0,filestat:this.dir.stat()}}fd_filestat_set_size(t){return 8}fd_read(t){return{ret:8,data:new Uint8Array}}fd_pread(t,r){return{ret:8,data:new Uint8Array}}fd_write(t){return{ret:8,nwritten:0}}fd_pwrite(t,r){return{ret:8,nwritten:0}}constructor(t){super(),this.dir=t}},W=class extends M{fd_prestat_get(){return{ret:0,prestat:K.dir(this.prestat_name)}}constructor(t,r){super(new B(r)),this.prestat_name=t}},D=class extends T{path_open(t,r,s){if(this.readonly&&(r&BigInt(64))==BigInt(64))return{ret:63,fd_obj:null};if((t&X)==X){if(this.readonly)return{ret:63,fd_obj:null};this.data=new Uint8Array([])}let f=new G(this);return s&st&&f.fd_seek(0n,v),{ret:0,fd_obj:f}}get size(){return BigInt(this.data.byteLength)}stat(){return new y(this.ino,F,this.size)}constructor(t,r){super(),this.data=new Uint8Array(t),this.readonly=!!r?.readonly}},C=class wt{static from(t){let r=new wt;if(r.is_dir=t.endsWith("/"),t.startsWith("/"))return{ret:76,path:null};if(t.includes("\0"))return{ret:28,path:null};for(let s of t.split("/"))if(!(s===""||s===".")){if(s===".."){if(r.parts.pop()==null)return{ret:76,path:null};continue}r.parts.push(s)}return{ret:0,path:r}}to_path_string(){let t=this.parts.join("/");return this.is_dir&&(t+="/"),t}constructor(){this.parts=[],this.is_dir=!1}},B=class c extends T{parent_ino(){return this.parent==null?T.root_ino():this.parent.ino}path_open(t,r,s){return{ret:0,fd_obj:new M(this)}}stat(){return new y(this.ino,S,0n)}get_entry_for_path(t){let r=this;for(let s of t.parts){if(!(r instanceof c))return{ret:54,entry:null};let f=r.contents.get(s);if(f!==void 0)r=f;else return m.log(s),{ret:44,entry:null}}return t.is_dir&&r.stat().filetype!=S?{ret:54,entry:null}:{ret:0,entry:r}}get_parent_dir_and_entry_for_path(t,r){let s=t.parts.pop();if(s===void 0)return{ret:28,parent_entry:null,filename:null,entry:null};let{ret:f,entry:e}=this.get_entry_for_path(t);if(e==null)return{ret:f,parent_entry:null,filename:null,entry:null};if(!(e instanceof c))return{ret:54,parent_entry:null,filename:null,entry:null};let n=e.contents.get(s);return n===void 0?r?{ret:0,parent_entry:e,filename:s,entry:null}:{ret:44,parent_entry:null,filename:null,entry:null}:t.is_dir&&n.stat().filetype!=S?{ret:54,parent_entry:null,filename:null,entry:null}:{ret:0,parent_entry:e,filename:s,entry:n}}create_entry_for_path(t,r){let{ret:s,path:f}=C.from(t);if(f==null)return{ret:s,entry:null};let{ret:e,parent_entry:n,filename:i,entry:o}=this.get_parent_dir_and_entry_for_path(f,!0);if(n==null||i==null)return{ret:e,entry:null};if(o!=null)return{ret:20,entry:null};m.log("create",f);let a;return r?a=new c(new Map):a=new D(new ArrayBuffer(0)),n.contents.set(i,a),o=a,{ret:0,entry:o}}constructor(t){super(),this.parent=null,t instanceof Array?this.contents=new Map(t):this.contents=t;for(let r of this.contents.values())r instanceof c&&(r.parent=this)}},z=class c extends g{fd_filestat_get(){return{ret:0,filestat:new y(this.ino,nt,BigInt(0))}}fd_fdstat_get(){let t=new A(nt,0);return t.fs_rights_base=BigInt(64),{ret:0,fdstat:t}}fd_write(t){return this.write(t),{ret:0,nwritten:t.byteLength}}static lineBuffered(t){let r=new TextDecoder("utf-8",{fatal:!1}),s="";return new c(f=>{s+=r.decode(f,{stream:!0});let e=s.split(`
`);for(let[n,i]of e.entries())n<e.length-1?t(i):s=i})}constructor(t){super(),this.ino=T.issue_ino(),this.write=t}};var ft=class{#e=0;#t=new Map;newJSVal(t){let r=++this.#e;return this.#t.set(r,t),r}getJSVal(t){if(!this.#t.has(t))throw new WebAssembly.RuntimeError(`getJSVal(${t})`);return this.#t.get(t)}freeJSVal(t){if(!this.#t.delete(t))throw new WebAssembly.RuntimeError(`freeJSVal(${t})`)}},At=await(async()=>{if(globalThis.setImmediate)return globalThis.setImmediate;if(globalThis.Deno)try{return(await import("node:timers")).setImmediate}catch{}if(globalThis.scheduler)return(c,...t)=>scheduler.postTask(()=>c(...t));if(globalThis.MessageChannel){class c{#e=[];#t=new MessageChannel;constructor(){this.#t.port1.addEventListener("message",()=>{this.#e.pop()()}),this.#t.port1.start()}setImmediate(s,...f){this.#e.push(()=>s(...f)),this.#t.port2.postMessage(void 0)}}let t=new c;return(r,...s)=>t.setImmediate(r,...s)}return(c,...t)=>setTimeout(c,0,...t)})(),Nt=c=>{let t=new ft,r=globalThis.FinalizationRegistry?new FinalizationRegistry(s=>c.rts_freeStablePtr(s)):{register:()=>{},unregister:()=>!0};return{newJSVal:s=>t.newJSVal(s),getJSVal:s=>t.getJSVal(s),freeJSVal:s=>t.freeJSVal(s),scheduleWork:()=>At(c.rts_schedulerLoop),ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC:(s,f)=>s.reject(new WebAssembly.RuntimeError(f)),ZC19ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC:s=>s.resolve(),ZC20ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC:s=>{s.throwTo=()=>{}},ZC21ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC:(s,f)=>{s.throwTo=e=>c.rts_promiseThrowTo(f,e)},ZC22ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC:()=>{let s,f,e=new Promise((n,i)=>{s=n,f=i});return e.resolve=s,e.reject=f,e},ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC:s=>`${s.stack?s.stack:s}`,ZC1ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC:(s,f)=>new TextDecoder("utf-8",{fatal:!0}).decode(new Uint8Array(c.memory.buffer,s,f)),ZC2ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC:(s,f,e)=>new TextEncoder().encodeInto(s,new Uint8Array(c.memory.buffer,f,e)).written,ZC3ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC:s=>s.length,ZC4ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC:s=>{try{r.unregister(s)}catch{}},ZC18ZCghczminternalZCGHCziInternalziWasmziPrimziImportsZC:(s,f)=>s.then(()=>c.rts_promiseResolveUnit(f),e=>c.rts_promiseReject(f,e)),ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziConcziInternalZC:async s=>new Promise(f=>setTimeout(f,s/1e3))}};var Ot={"countdown.imp":`// count down from ten

n := 10;
while n >= 0 do
    print n;
    n := n-1
end`,"factorial.imp":`// compute the 10-th factorial (10!)

fac := 1;
while n <= 10 do
    n := n+1;
    fac := fac * n
end;
print fac`,"divmod.imp":`// compute the quotient and remainder of d divided by v
procedure divmod(d, v; q, r) begin
    q := 0;
    r := d;
    if not v = 0 then
        while r >= v do
            tmpv := v;
            tmpq := 1;
            while r >= tmpv + tmpv do
                tmpv := tmpv + tmpv;
                tmpq := tmpq + tmpq
            end;
            r := r - tmpv;
            q := q + tmpq
        end
    end
end`,"fibonacci.imp":`// linearly compute and print the first 100 fibonacci numbers

f0 := 0;
f1 := 1;
while n < 100 do
    print f0;
    t := f0;
    f0 := f1;
    f1 := f0 + t;
    n := n + 1
end`,"gauss.imp":`// compute the sum from 0 to 100

while n < 100 do
    s := s + n;
    n := n + 1
end;
print s`,"local.imp":`// local variable definition

x := 42;
var x := x + 17 in
    print x
end;
print x`,"nondeterminism.imp":`// random execution

print 1 [] print 2`,"parallel.imp":`// parallel execution

x := 10;
(x := x+1; print x par x := x-1; print x)`,"primes.imp":`// compute the n-th prime number
procedure prime(n; p) begin
    // compute the floored square root
    procedure sqrtfloor(n; r) begin
        r := 1;
        temp := 1;
        while temp <= n do
            r *= 2;
            temp := r * r
        end;
        while temp > n do
            r -= 1;
            temp := r * r
        end
    end;
    candidate := 1;
    count := 0;
    while count < n do
        candidate += 1;
        if candidate = 2 or candidate = 3 then
            prime := 1
        else
            i := 2;
            prime := 1;
            sqrtfloor(candidate; sqrt);
            while i <= sqrt and prime = 1 do
                q := candidate / i;
                r := candidate % i;
                if r = 0 then
                    prime := 0
                end;
                if i = 2 then
                    i := 3
                else
                    i += 2
                end
            end
        end;
        if prime = 1 then
            count += 1
        end
    end;
    p := candidate
end`,"procedure.imp":`// procedure definition and invocation

procedure fib(n; x) begin
    f0 := 0;
    f1 := 1;
    while k < n do
        t := f0;
        f0 := f1;
        f1 := f0 + t;
        k := k + 1
    end;
    x := f1
end;

fib(10; x);
print x`,"turing.imp":`// register machine implemented in IMP to prove its turing completeness

// increment
procedure inc(pc, reg, next; pc, reg) begin
    reg := reg + 1;
    pc := next
end;

// decrement or jump on zero
procedure decjz(pc, reg, dec, label; pc, reg) begin
    if reg = 0 then
        pc := label
    else
        reg := reg - 1;
        pc := dec
    end
end;

// unconditional jump
procedure jump(pc, label; pc) begin
    pc := label
end;

// registers
r0 := 0;
r1 := 3;
pc := 0;
halt := 0;

// copy r1 to r0
while halt = 0 do
    case pc of
        0: decjz(pc, r1, 1, 3; pc, r1),
        1: inc(pc, r0, 0; pc, r0),
        3: halt := 1,
        default: skip
    end
end`};var lt={prompt:"IMP> ",welcome:"Welcome to the IMP REPL! Enter :help to list available metacommands."},ct=class extends globalThis.Terminal{constructor(){super({cursorBlink:!0,fontFamily:'"CommitMono", "Courier New Bold"',fontSize:13}),this.input="",this.cursor=0;let t=document.getElementById("terminal");t.focus(),this.open(t),this.writeln(lt.welcome),this.write(lt.prompt),this.onKey(r=>{let{key:s,domEvent:f}=r,e=!f.altKey&&!f.ctrlKey&&!f.metaKey;if(f.keyCode===13){let n=this.input;this.input="",this.cursor=0,this.writeln(""),n.trim()!=""&&this.onInputSubmit&&this.onInputSubmit(n),this.prompt()}else f.keyCode===8?this.cursorPos>0&&(this.input=this.input.substring(0,this.cursor-1),this.writeln(this.input),this.cursor--):e&&s&&(this.input+=s,this.cursor++,this.write(s))})}prompt(){this.write(lt.prompt),this.input="",this.cursor=0}setInputHandler(t){this.onInputSubmit=t}},ut=class{constructor(){this.exports={},this.terminal=new ct,this.terminal.setInputHandler(t=>this.interpret(t))}async init(){let t=new TextEncoder("utf-8"),r=[new G(new D([])),z.lineBuffered(e=>this.terminal.writeln(e)),z.lineBuffered(e=>console.warn(`[WASI stderr] ${e}`)),new W(".",Object.entries(Ot).map(([e,n])=>[e,new D(t.encode(n))]))],s=new _t([],[],r),f=await WebAssembly.instantiateStreaming(fetch("./impli.wasm"),{wasi_snapshot_preview1:s.wasiImport,ghc_wasm_jsffi:Nt(this.exports)});return s.initialize(f.instance),Object.assign(this.exports,f.instance.exports),this.pointer=this.exports.initialize(),this}async interpret(t){let r=await this.exports.execute(this.pointer,t);console.log(r)}};(async()=>globalThis.impli=await new ut().init())();
