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
this.violinCustomSeriesInstaller=function(){"use strict";function n(n){return Math.abs(n)<=1?.75*(1-n*n):0}var t=function(t,e){var i={};if(null==t.context.violins){t.context.violins=[],i=t.context.violins;for(var a=t.dataInsideLength,o=0;o<a;++o){var l=e.value(0,o);null==i[l]&&(i[l]={firstDataIndex:o,data:[]}),i[l].data.push(e.value(1,o))}}else i=t.context.violins;t.itemPayload.symbolSize;var r,s,u,d=e.value(0),c=e.value(1),v=e.coord([d,c]),f=t.itemPayload.bandWidthScale,h=(e.coord([1,0])[0]-e.coord([0,0])[0])*(null==f?1:f),p=i[d],y=null;if(p&&p.firstDataIndex===t.dataIndexInside){for(var g=(r=n,s=1,u=p.data,function(n){for(var t=0,e=0;e<u.length;e++)t+=r((n-u[e])/s);return t/(u.length*s)}),m=t.itemPayload.binCount||100,w=[],x=0;x<m;x++)w.push(x*(10/(m-1)));for(var I=w.map((function(n){return[n,g(n)]})),S=[],b=[],C=function(){if(b.length>1){for(var n=b.length-1;n>=0;--n)b.push([2*v[0]-b[n][0],b[n][1]]);var i=t.itemPayload.areaOpacity;S.push({type:"polygon",shape:{points:b.slice()},style:{fill:e.visual("color"),opacity:null==i?.5:i}})}b.length=0},P=0;P<I.length;++P){var D=e.coord([d,I[P][0]]);I[P][1]<.001?C():b.push([D[0]+h/2*I[P][1],D[1]])}C(),y={type:"group",children:S,silent:!0}}return y};return{install:function(n){n.registerCustomSeries("violin",t)}}}(),
// Automatically register the custom series
"undefined"!=typeof window&&window.echarts&&window.echarts.use(window.violinCustomSeriesInstaller);
//# sourceMappingURL=index.auto.min.js.map