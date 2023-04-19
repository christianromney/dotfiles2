"use strict";var f=Object.defineProperty;var $=Object.getOwnPropertyDescriptor;var y=Object.getOwnPropertyNames;var D=Object.prototype.hasOwnProperty;var w=(t,e)=>{for(var s in e)f(t,s,{get:e[s],enumerable:!0})},P=(t,e,s,a)=>{if(e&&typeof e=="object"||typeof e=="function")for(let o of y(e))!D.call(t,o)&&o!==s&&f(t,o,{get:()=>e[o],enumerable:!(a=$(e,o))||a.enumerable});return t};var O=t=>P(f({},"__esModule",{value:!0}),t);var C={};w(C,{default:()=>M});module.exports=O(C);var S=require("@raycast/api");var r=require("@raycast/api"),u=require("child_process");var i=require("fs");var p=t=>{let e=Math.floor(t/3600),s=String(Math.floor(t%3600/60)).padStart(2,"0"),a=String(Math.floor(t%60)).padStart(2,"0");return`${e}:${s}:${a}`};var F=r.environment.supportPath+"/customTimers.json",T=(t=!1)=>{let e=(0,r.getPreferenceValues)();if(parseFloat(e.volumeSetting)>5){let s="\u26A0\uFE0F Timer alert volume should not be louder than 5 (it can get quite loud!)";return t?(0,r.showHUD)(s):(0,r.showToast)({style:r.Toast.Style.Failure,title:s}),!1}return!0};async function h(t,e="Untitled",s="default"){let o=(r.environment.supportPath+"/"+new Date().toISOString()+"---"+t+".timer").replace(/:/g,"__");(0,i.writeFileSync)(o,e);let c=(0,r.getPreferenceValues)(),g=`${r.environment.assetsPath+"/"+(s==="default"?c.selectedSound:s)}`,n=[`sleep ${t}`];n.push(`if [ -f "${o}" ]; then osascript -e 'display notification "Timer \\"${e}\\" complete" with title "Ding!"'`);let l=`afplay "${g}" --volume ${c.volumeSetting.replace(",",".")}`;if(c.selectedSound==="speak_timer_name"?n.push(`say "${e}"`):n.push(l),c.ringContinuously){let m=`${o}`.replace(".timer",".dismiss");(0,i.writeFileSync)(m,".dismiss file for Timers"),n.push(`while [ -f "${m}" ]; do ${l}; done`)}n.push(`rm "${o}"; else echo "Timer deleted"; fi`),(0,u.exec)(n.join(" && "),(m,d)=>{if(m){console.log(`error: ${m.message}`);return}if(d){console.log(`stderr: ${d}`);return}}),(0,r.popToRoot)(),await(0,r.showHUD)(`Timer "${e}" started for ${p(t)}! \u{1F389}`)}var M=async()=>{!T()||(await(0,S.closeMainWindow)(),h(60*15,"15 Minute Timer"))};0&&(module.exports={});
