/*
* Licensed to the Apache Software Foundation (ASF) under one
* or more contributor license agreements.  See the NOTICE file
* distributed with this work for additional information
* regarding copyright ownership.  The ASF licenses this file
* to you under the Apache License, Version 2.0 (the
* "License"); you may not use this file except in compliance
* with the License.  You may obtain a copy of the License at
*
*   http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
*/
this.segmentedDoughnutCustomSeriesInstaller=function(e){"use strict";var t=function(t,r){var s=[],n=t.itemPayload,o=Math.max(1,n.segmentCount||1),a=Math.max(0,r.value(0)||0),i=n.center||["50%","50%"],l=n.radius||["50%","60%"],h=r.getWidth(),d=r.getHeight(),c=Math.min(h,d),u=e.number.parsePercent(i[0],r.getWidth()),p=e.number.parsePercent(i[1],r.getHeight()),g=e.number.parsePercent(l[1],c/2),m=e.number.parsePercent(l[0],c/2),y=(e.zrUtil.retrieve2(n.padAngle,2)||0)*Math.PI/180,f=(2*Math.PI-y*o)/o,w={type:"group",children:[]};s.push(w);var b=n.backgroundStyle||{},v=!1!==b.show,x={fill:b.color||"rgba(180, 180, 180, 0.2)",stroke:b.borderColor||"none",lineWidth:b.borderWidth||0,lineType:b.borderType||"solid",opacity:b.opacity||1,shadowBlur:b.shadowBlur||0,shadowColor:b.shadowColor||"rgba(0, 0, 0, 0)",shadowOffsetX:b.shadowOffsetX||0,shadowOffsetY:b.shadowOffsetY||0},P={type:"group",children:[]};s.push(P);for(var A=r.style(),C=r.styleEmphasis(),M=-Math.PI/2,S=(g-m)/2,I=0;I<o;++I){var k=M+(f+y)*I,O=M+(f+y)*I+f;v&&w.children.push({type:"sector",shape:{cx:u,cy:p,r0:m,r:g,cornerRadius:S,startAngle:k,endAngle:O,clockwise:!0},style:x,silent:!0}),I<a&&P.children.push({type:"sector",shape:{cx:u,cy:p,r0:m,r:g,cornerRadius:S,startAngle:k,endAngle:O,clockwise:!0},style:A,styleEmphasis:C})}var W,z=n.label;z&&z.show&&(W={type:"text",style:{text:e.format.formatTpl(z.formatter||"{c}/{b}",{$vars:["seriesName","b","c","d"],seriesName:t.seriesName,b:o,c:a,d:Math.round(a/o*100)+"%"}),fontSize:z.fontSize||12,fill:z.color||"#000",textAlign:"center",textVerticalAlign:"middle"},x:u,y:p},s.push(W));return{type:"group",children:s}};return{install:function(e){e.registerCustomSeries("segmentedDoughnut",t)}}}(echarts),
// Automatically register the custom series
"undefined"!=typeof window&&window.echarts&&window.echarts.use(window.segmentedDoughnutCustomSeriesInstaller);
//# sourceMappingURL=index.auto.min.js.map
