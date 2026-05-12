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
this.lineRangeCustomSeriesInstaller=function(e){"use strict";var o=function(o,t){var a={type:"group",children:[]},s=o.dataInsideLength;if(o.dataIndex===s-1){for(var i=o.itemPayload,r=1===o.encode.x.length,l=r?o.encode.y[0]:o.encode.x[0],n=r?o.encode.y[1]:o.encode.x[1],d=[],h="",f="",p=0;p<s;p++){var c=t.value(l,p),u=t.coord(r?[p,c]:[c,p]);d.push(u),h+=(0===p?"M":"L")+u[0]+","+u[1]+" "}for(p=s-1;p>=0;p--){var w=t.value(n,p),y=t.coord(r?[p,w]:[w,p]);d.push(y),f+=(p===s-1?"M":"L")+y[0]+","+y[1]+" "}if(i.areaStyle){var v=i.areaStyle;a.children.push({type:"polygon",shape:{points:d},style:{fill:v.color||t.visual("color"),opacity:e.zrUtil.retrieve2(v.opacity,.2),shadowBlur:v.shadowBlur,shadowColor:v.shadowColor,shadowOffsetX:v.shadowOffsetX,shadowOffsetY:v.shadowOffsetY},disableTooltip:!0})}var O=i.lineStyle||{},g={fill:"none",stroke:O.color||t.visual("color"),lineWidth:e.zrUtil.retrieve2(O.width,0),opacity:e.zrUtil.retrieve2(O.opacity,1),type:O.type,dashOffset:O.dashOffset,lineCap:O.cap,lineJoin:O.join,miterLimit:O.miterLimit,shadowBlur:O.shadowBlur,shadowColor:O.shadowColor,shadowOffsetX:O.shadowOffsetX,shadowOffsetY:O.shadowOffsetY};a.children.push({type:"path",shape:{pathData:h},style:g,disableTooltip:!0}),a.children.push({type:"path",shape:{pathData:f},style:g,disableTooltip:!0})}return a};return{install:function(e){e.registerCustomSeries("lineRange",o)}}}(echarts),
// Automatically register the custom series
"undefined"!=typeof window&&window.echarts&&window.echarts.use(window.lineRangeCustomSeriesInstaller);
//# sourceMappingURL=index.auto.min.js.map
