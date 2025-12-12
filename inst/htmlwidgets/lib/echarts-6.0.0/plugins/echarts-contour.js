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
this.contourCustomSeriesInstaller=function(e,t){"use strict";function r(e){var t=Object.create(null);return e&&Object.keys(e).forEach((function(r){if("default"!==r){var n=Object.getOwnPropertyDescriptor(e,r);Object.defineProperty(t,r,n.get?n:{enumerable:!0,get:function(){return e[r]}})}})),t.default=e,Object.freeze(t)}var n=r(t),o=function(t,r){var o=t.dataInsideLength;if(t.dataIndex===o-1){for(var i=t.itemPayload,a=t.coordSys,l=[],u=0;u<o;u++){var c=r.value(0,u),s=r.value(1,u),d=r.value(2,u),h=r.coord([c,s]);l.push({coord:h,value:d})}var f=a.width,p=a.height,v=n.scaleLinear().domain(n.extent(l,(function(e){return e.coord[0]}))).range([0,f]),y=n.scaleLinear().domain(n.extent(l,(function(e){return e.coord[1]}))).range([0,p]),g=e.zrUtil.retrieve2(i.thresholds,8),w=e.zrUtil.retrieve2(i.bandwidth,20),b=n.contourDensity().x((function(e){return v(e.coord[0])})).y((function(e){return y(e.coord[1])})).size([f,p]).bandwidth(w).thresholds(g)(l.map((function(e){return{coord:e.coord,value:e.value}}))).map(n.geoPath()),m=i.itemStyle||{},z=m.color||[r.visual("color")],x=e.zrUtil.retrieve2(m.opacity,[.3,1]);"number"==typeof x&&(x=[x,x]);var O=i.lineStyle||{},S=O.color,U=e.zrUtil.retrieve2(O.width,1),j=[];return b.forEach((function(t,r){var o="none"===z?"none":function(e,t,r){if(t<=1)return e[0];var o=r/(t-1),i=Math.floor(o*(e.length-1));if(i<=0)return e[0];if(i>=e.length-1)return e[e.length-1];var a=n.color(e[i]),l=n.color(e[i+1]),u=(o-i/(e.length-1))*(e.length-1);return n.interpolate(a,l)(u)}(z,g,r);"none"!==z&&j.push({type:"path",shape:{pathData:t},style:{fill:o,opacity:x[0]+(x[1]-x[0])*(r/(b.length-1))},z2:-1,disableTooltip:!0}),"none"===S||null==S&&"none"===z||j.push({type:"path",shape:{pathData:t},style:{fill:"none",stroke:e.zrUtil.retrieve2(S,o),lineWidth:U,opacity:e.zrUtil.retrieve2(O.opacity,1)},z2:-1,disableTooltip:!0})})),{type:"group",children:j,x:a.x,y:a.y}}return null};return{install:function(e){e.registerCustomSeries("contour",o)}}}(echarts,d3),
// Automatically register the custom series
"undefined"!=typeof window&&window.echarts&&window.echarts.use(window.contourCustomSeriesInstaller);
//# sourceMappingURL=index.auto.min.js.map
