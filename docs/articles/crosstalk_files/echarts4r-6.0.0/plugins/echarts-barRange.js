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
this.barRangeCustomSeriesInstaller=function(){"use strict";var t=function(t,e){var r=e.value(0),i=e.value(1),l=e.coord([r,i]),n=e.value(2),o=e.coord([r,n]),a=e.coord([1,0])[0]-e.coord([0,0])[0],s=t.itemPayload.barWidth;null==s&&(s="70%");var d="string"==typeof s&&s.endsWith("%")?parseFloat(s)/100*a:s,u=t.itemPayload.borderRadius||0,c={type:"rect",shape:{x:l[0]-d/2,y:l[1],width:d,height:o[1]-l[1],r:u},style:{fill:e.visual("color")}},y=t.itemPayload.margin,g=null==y?10:y;return{type:"group",children:[c,{type:"text",x:o[0],y:o[1]-g,style:{text:n.toString()+t.itemPayload.textSymbol,textAlign:"center",textVerticalAlign:"bottom",fill:"#333"}},{type:"text",x:l[0],y:l[1]+g,style:{text:i.toString()+t.itemPayload.textSymbol,textAlign:"center",textVerticalAlign:"top",fill:"#333"}}]}};return{install:function(e){e.registerCustomSeries("barRange",t)}}}(),
// Automatically register the custom series
"undefined"!=typeof window&&window.echarts&&window.echarts.use(window.barRangeCustomSeriesInstaller);
//# sourceMappingURL=index.auto.min.js.map