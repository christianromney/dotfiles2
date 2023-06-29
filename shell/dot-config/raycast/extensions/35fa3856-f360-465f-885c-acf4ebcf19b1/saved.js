"use strict";var re=Object.create;var g=Object.defineProperty;var ae=Object.getOwnPropertyDescriptor;var de=Object.getOwnPropertyNames;var ce=Object.getPrototypeOf,le=Object.prototype.hasOwnProperty;var f=(s,e)=>()=>(e||s((e={exports:{}}).exports,e),e.exports),pe=(s,e)=>{for(var o in e)g(s,o,{get:e[o],enumerable:!0})},V=(s,e,o,r)=>{if(e&&typeof e=="object"||typeof e=="function")for(let t of de(e))!le.call(s,t)&&t!==o&&g(s,t,{get:()=>e[t],enumerable:!(r=ae(e,t))||r.enumerable});return s};var ue=(s,e,o)=>(o=s!=null?re(ce(s)):{},V(e||!s||!s.__esModule?g(o,"default",{value:s,enumerable:!0}):o,s)),he=s=>V(g({},"__esModule",{value:!0}),s);var B=f((Te,b)=>{"use strict";b.exports=function(e){var o=0,r;function t(){return o||(o=1,r=e.apply(this,arguments),e=null),r}return t.displayName=e.displayName||e.name||t.displayName||t.name,t}});var C=f((Pe,_)=>{var $=require("child_process"),A=B(),I=class{constructor(){this.child=null,this.baseSpeed=0}speak(e,o,r,t){if(typeof t!="function"&&(t=()=>{}),t=A(t),!e)return setImmediate(()=>{t(new TypeError("say.speak(): must provide text parameter"))});let{command:i,args:d,pipedData:a,options:n}=this.buildSpeakCommand({text:e,voice:o,speed:r});this.child=$.spawn(i,d,n),this.child.stdin.setEncoding("ascii"),this.child.stderr.setEncoding("ascii"),a&&this.child.stdin.end(a),this.child.stderr.once("data",c=>{t(new Error(c))}),this.child.addListener("exit",(c,u)=>{if(c===null||u!==null)return t(new Error(`say.speak(): could not talk, had an error [code: ${c}] [signal: ${u}]`));this.child=null,t(null)})}export(e,o,r,t,i){if(typeof i!="function"&&(i=()=>{}),i=A(i),!e)return setImmediate(()=>{i(new TypeError("say.export(): must provide text parameter"))});if(!t)return setImmediate(()=>{i(new TypeError("say.export(): must provide filename parameter"))});try{var{command:d,args:a,pipedData:n,options:c}=this.buildExportCommand({text:e,voice:o,speed:r,filename:t})}catch(u){return setImmediate(()=>{i(u)})}this.child=$.spawn(d,a,c),this.child.stdin.setEncoding("ascii"),this.child.stderr.setEncoding("ascii"),n&&this.child.stdin.end(n),this.child.stderr.once("data",u=>{i(new Error(u))}),this.child.addListener("exit",(u,w)=>{if(u===null||w!==null)return i(new Error(`say.export(): could not talk, had an error [code: ${u}] [signal: ${w}]`));this.child=null,i(null)})}stop(e){if(typeof e!="function"&&(e=()=>{}),e=A(e),!this.child)return setImmediate(()=>{e(new Error("say.stop(): no speech to kill"))});this.runStopCommand(),this.child=null,e(null)}convertSpeed(e){return Math.ceil(this.baseSpeed*e)}getInstalledVoices(e){typeof e!="function"&&(e=()=>{}),e=A(e);let{command:o,args:r}=this.getVoices();var t=[];this.child=$.spawn(o,r),this.child.stdin.setEncoding("ascii"),this.child.stderr.setEncoding("ascii"),this.child.stderr.once("data",i=>{e(new Error(i))}),this.child.stdout.on("data",function(i){t+=i}),this.child.addListener("exit",(i,d)=>{if(i===null||d!==null)return e(new Error(`say.getInstalledVoices(): could not get installed voices, had an error [code: ${i}] [signal: ${d}]`));t.length>0&&(t=t.split(`\r
`),t=t[t.length-1]===""?t.slice(0,t.length-1):t),this.child=null,e(null,t)}),this.child.stdin.end()}};_.exports=I});var F=f((Le,R)=>{var me=C(),Se=100,ye="festival",D=class extends me{constructor(){super(),this.baseSpeed=Se}buildSpeakCommand({text:e,voice:o,speed:r}){let t=[],i="",d={};return t.push("--pipe"),r&&(i+=`(Parameter.set 'Audio_Command "aplay -q -c 1 -t raw -f s16 -r $(($SR*${this.convertSpeed(r)}/100)) $FILE") `),o&&(i+=`(${o}) `),i+=`(SayText "${e}")`,{command:ye,args:t,pipedData:i,options:d}}buildExportCommand({text:e,voice:o,speed:r,filename:t}){throw new Error(`say.export(): does not support platform ${this.platform}`)}runStopCommand(){process.kill(this.child.pid+2)}getVoices(){throw new Error(`say.export(): does not support platform ${this.platform}`)}};R.exports=D});var z=f((Ne,U)=>{var fe=C(),we=175,K="say",T=class extends fe{constructor(){super(),this.baseSpeed=we}buildSpeakCommand({text:e,voice:o,speed:r}){let t=[],i="",d={};return o?t.push("-v",o,e):t.push(e),r&&t.push("-r",this.convertSpeed(r)),{command:K,args:t,pipedData:i,options:d}}buildExportCommand({text:e,voice:o,speed:r,filename:t}){let i=[],d="",a={};return o?i.push("-v",o,e):i.push(e),r&&i.push("-r",this.convertSpeed(r)),t&&i.push("-o",t,"--data-format=LEF32@32000"),{command:K,args:i,pipedData:d,options:a}}runStopCommand(){this.child.stdin.pause(),this.child.kill()}getVoices(){throw new Error(`say.export(): does not support platform ${this.platform}`)}};U.exports=T});var j=f((qe,W)=>{var ge=require("child_process"),Ae=C(),Ce=0,P="powershell",L=class extends Ae{constructor(){super(),this.baseSpeed=Ce}buildSpeakCommand({text:e,voice:o,speed:r}){let t=[],i="",d={},a="Add-Type -AssemblyName System.speech;$speak = New-Object System.Speech.Synthesis.SpeechSynthesizer;";return o&&(a+=`$speak.SelectVoice('${o}');`),r&&(a+=`$speak.Rate = ${this.convertSpeed(r||1)};`),a+="$speak.Speak([Console]::In.ReadToEnd())",i+=e,t.push(a),d.shell=!0,{command:P,args:t,pipedData:i,options:d}}buildExportCommand({text:e,voice:o,speed:r,filename:t}){let i=[],d="",a={},n="Add-Type -AssemblyName System.speech;$speak = New-Object System.Speech.Synthesis.SpeechSynthesizer;";if(o&&(n+=`$speak.SelectVoice('${o}');`),r&&(n+=`$speak.Rate = ${this.convertSpeed(r||1)};`),t)n+=`$speak.SetOutputToWaveFile('${t}');`;else throw new Error("Filename must be provided in export();");return n+="$speak.Speak([Console]::In.ReadToEnd());$speak.Dispose()",d+=e,i.push(n),a.shell=!0,{command:P,args:i,pipedData:d,options:a}}runStopCommand(){this.child.stdin.pause(),ge.exec(`taskkill /pid ${this.child.pid} /T /F`)}convertSpeed(e){return Math.max(-10,Math.min(Math.round(9.0686*Math.log(e)-.1806),10))}getVoices(){let e=[],o="Add-Type -AssemblyName System.speech;$speak = New-Object System.Speech.Synthesis.SpeechSynthesizer;$speak.GetInstalledVoices() | % {$_.VoiceInfo.Name}";return e.push(o),{command:P,args:e}}};W.exports=L});var X=f((Me,x)=>{var ve=F(),xe=z(),Ee=j(),G="darwin",H="linux",J="win32",v=class{constructor(e){if(e||(e=process.platform),e===G)return new xe;if(e===H)return new ve;if(e===J)return new Ee;throw new Error(`new Say(): unsupported platorm! ${e}`)}};x.exports=new v;x.exports.Say=v;x.exports.platforms={WIN32:J,MACOS:G,LINUX:H}});var Ie={};pe(Ie,{default:()=>ne});module.exports=he(Ie);var S=require("@raycast/api"),O=require("react");var h=require("@raycast/api"),N=ue(X()),k=require("react/jsx-runtime");var q=s=>(0,k.jsx)(h.Action.CopyToClipboard,{icon:h.Icon.CopyClipboard,...s}),Q=({content:s})=>(0,k.jsx)(h.Action,{icon:h.Icon.SpeechBubble,title:"Speak",onAction:()=>{N.default.stop(),N.default.speak(s)},shortcut:{modifiers:["cmd","shift"],key:"p"}});var M=({icon:s=h.Icon.Trash,title:e,dialog:o,onAction:r,shortcut:t={modifiers:["ctrl"],key:"x"}})=>(0,k.jsx)(h.Action,{style:h.Action.Style.Destructive,icon:s,title:e,onAction:async()=>{await(0,h.confirmAlert)({title:o.title??e,message:o.message??"This action cannot be undone",icon:s,primaryAction:{title:o.primaryButton??e,style:h.Alert.ActionStyle.Destructive,onAction:r}})},shortcut:t});var Y=require("@raycast/api");var E=require("react/jsx-runtime"),Z=({question:s,answer:e})=>(0,E.jsxs)(Y.ActionPanel.Section,{title:"Copy",children:[(0,E.jsx)(q,{title:"Copy Answer",content:e}),(0,E.jsx)(q,{title:"Copy Question",content:s})]});var y=require("@raycast/api"),ee=require("react/jsx-runtime"),te=()=>(0,ee.jsx)(y.ActionPanel.Section,{title:"Preferences",children:(0,ee.jsx)(y.Action,{icon:y.Icon.Gear,title:"Open Extension Preferences",onAction:y.openExtensionPreferences})});var l=require("@raycast/api"),m=require("react");function se(){let[s,e]=(0,m.useState)([]),[o,r]=(0,m.useState)(!0);(0,m.useEffect)(()=>{(async()=>{let a=await l.LocalStorage.getItem("savedChats");a&&e(n=>[...n,...JSON.parse(a)]),r(!1)})()},[]),(0,m.useEffect)(()=>{l.LocalStorage.setItem("savedChats",JSON.stringify(s))},[s]);let t=(0,m.useCallback)(async a=>{let n=await(0,l.showToast)({title:"Saving your answer...",style:l.Toast.Style.Animated}),c={...a,saved_at:new Date().toISOString()};e([...s,c]),n.title="Answer saved!",n.style=l.Toast.Style.Success},[e,s]),i=(0,m.useCallback)(async a=>{let n=await(0,l.showToast)({title:"Unsaving your answer...",style:l.Toast.Style.Animated}),c=s.filter(u=>u.id!==a.id);e(c),n.title="Answer unsaved!",n.style=l.Toast.Style.Success},[e,s]),d=(0,m.useCallback)(async()=>{let a=await(0,l.showToast)({title:"Clearing your saved answers...",style:l.Toast.Style.Animated});e([]),a.title="Saved answers cleared!",a.style=l.Toast.Style.Success},[e]);return(0,m.useMemo)(()=>({data:s,isLoading:o,add:t,remove:i,clear:d}),[s,o,t,i,d])}var oe=require("@raycast/api"),$e=require("react/jsx-runtime"),ie=s=>{let{chat:e,streamData:o}=s,r=o&&o.id===e.id,t=`**${e.question}**

${r?o.answer:e.answer}`;return(0,$e.jsx)(oe.List.Item.Detail,{markdown:t})};var p=require("react/jsx-runtime");function ne(){let s=se(),[e,o]=(0,O.useState)(""),[r,t]=(0,O.useState)(null),i=n=>(0,p.jsxs)(S.ActionPanel,{children:[(0,p.jsx)(Z,{answer:n.answer,question:n.question}),(0,p.jsx)(S.ActionPanel.Section,{title:"Output",children:(0,p.jsx)(Q,{content:n.answer})}),(0,p.jsxs)(S.ActionPanel.Section,{title:"Delete",children:[(0,p.jsx)(M,{title:"Unsave",dialog:{title:"Are you sure you want to unsave this answer from your collection?"},onAction:()=>s.remove(n)}),(0,p.jsx)(M,{title:"Clear All",dialog:{title:"Are you sure you want to clear all your saved answer from your collection?"},onAction:()=>s.clear(),shortcut:{modifiers:["cmd","shift"],key:"delete"}})]}),(0,p.jsx)(te,{})]}),a=s.data.sort((n,c)=>new Date(c.saved_at??0).getTime()-new Date(n.saved_at??0).getTime()).filter((n,c,u)=>c===u.findIndex(w=>w.id===n.id)).filter(n=>e===""?!0:n.question.toLowerCase().includes(e.toLowerCase())||n.answer.toLowerCase().includes(e.toLowerCase()));return(0,p.jsx)(S.List,{isShowingDetail:a.length!==0,isLoading:s.isLoading,filtering:!1,throttle:!1,selectedItemId:r||void 0,onSelectionChange:n=>{n!==r&&t(n)},searchBarPlaceholder:"Search saved answer/question...",searchText:e,onSearchTextChange:o,children:s.data.length===0?(0,p.jsx)(S.List.EmptyView,{title:"No saved answers",description:"Save generated question with \u2318 + S shortcut",icon:S.Icon.Stars}):(0,p.jsx)(S.List.Section,{title:"Saved",subtitle:a.length.toLocaleString(),children:a.map(n=>(0,p.jsx)(S.List.Item,{id:n.id,title:n.question,accessories:[{text:new Date(n.created_at??0).toLocaleDateString()}],detail:(0,p.jsx)(ie,{chat:n}),actions:n&&r===n.id?i(n):void 0},n.id))})})}0&&(module.exports={});