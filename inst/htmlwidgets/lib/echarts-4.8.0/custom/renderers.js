//  JS renderers for error bars, bands, etc.

//   the original - works for non-grouped bars only
function renderErrorBar(params, api) {
  var xValue = api.value(0);
  var highPoint = api.coord([xValue, api.value(1)]);
  var lowPoint = api.coord([xValue, api.value(2)]);
  var halfWidth = api.size([1, 0])[0] * 0.1;
  var style = api.style({
      stroke: api.visual('color'),
      fill: null
  });

  return {
      type: 'group',
      children: [{
          type: 'line',
          shape: {
              x1: highPoint[0] - halfWidth, y1: highPoint[1],
              x2: highPoint[0] + halfWidth, y2: highPoint[1]
          },
          style: style
      }, {
          type: 'line',
          shape: {
              x1: highPoint[0], y1: highPoint[1],
              x2: lowPoint[0], y2: lowPoint[1]
          },
          style: style
      }, {
          type: 'line',
          shape: {
              x1: lowPoint[0] - halfWidth, y1: lowPoint[1],
              x2: lowPoint[0] + halfWidth, y2: lowPoint[1]
          },
          style: style
      }]
  };
}

	
/*
  added ErrorBar support for grouped bars, barGap and barCategoryGap
  author: helgasoft.com
     Notes:	
  Prefix 'ri' stands for 'renderItem' function.
  Error bars can have chart bars, lines and scatter points as "hosts".
  It's convenient to "attach" error bars to their related chart bars	
  so they'll show/hide together when user clicks on a legend button.	
  This is done by having the same name for error and chart bars.	
  Default legend = FALSE, since we'll have only chart bars in legend.	
  Error bars will inherit color from their chart bar, blending with them.
  Therefore it is preferable to set a different color, like so -
  e_error_bar(..., color='blue'). Black is now set as default color.
  
	To test in R:
  grps <- 5    # customizable number of groups
  rpt <- grps*2
  df <- data.frame('Category' = c(rep(LETTERS[1:grps], each=rpt)),
    'Xaxis' = rep(paste(rep(LETTERS[1:grps], each=2), 1:grps*2, sep='.'), grps*rpt/(grps*2)), 
    'Yaxis' = 50 * abs(rnorm(grps*rpt))) %>%
    mutate(Lower = Yaxis - 5 * runif(grps*rpt),
           Upper = Yaxis + 5 * runif(grps*rpt))
  df %>% group_by(Category) %>% 
    e_charts(Xaxis) %>% 
    e_bar(Yaxis) %>%  #, barGap ='22%', barCategoryGap='55%') %>%
    e_error_bar(Lower, Upper) %>%
    e_datazoom(start = 50)
*/
function riErrorBar(params, api) {

  // input oss contains 
  //   [last.barGap, last.barCategoryGap, series.count, ends.width]
  let oss = JSON.parse(sessionStorage.getItem('ErrorBar.oss'));
  if (oss===null || !Object.keys(oss).length) return null;   // needs 4 input values

  let totSeries = Number(oss[2]);

  let xValue = api.value(0);
  let highPoint = api.coord([xValue, api.value(1)]);
  let lowPoint = api.coord([xValue, api.value(2)]);
  let endsWidth = Number(oss[3]);  //api.size([1, 0])[0] * 0.1;
	
  let csil = api.currentSeriesIndices().length / 2;
	// idx is index of related main bar
  let idx = params.seriesIndex - (params.seriesIndex < totSeries ? 0 : totSeries);	

  if (csil > 1 && totSeries > 1) {
  	let bgm = oss[0];
  	let bcgm = oss[1];
  	let olay = { count: csil };
  	olay.barGap = bgm!=='' ? bgm : '30%';		// '30%' is default for e_bar
  	olay.barCategoryGap = bcgm!=='' ? bcgm : '20%';
  	let barLayouts = api.barLayout(olay);		// will be csil # of barLayouts
  	
  	if (barLayouts) {
	  	let mbar = 0;
	  	api.currentSeriesIndices().some( (item, index) => {
	  		if (item == idx) {
	  			highPoint[0] += barLayouts[mbar].offsetCenter;
	  			// endsWidth = barLayouts[mbar].width /2;
	  			return true;
	  		}
	  		mbar++;
	  		return mbar >= csil;  // false until true
	  	});
  	}
  }
  lowPoint[0] = highPoint[0];
  
  var style = api.style({
      stroke: api.visual('color'),
      fill: null
  });
  return {
      type: 'group',
      children: [{
          type: 'line',
          shape: {
              x1: highPoint[0] - endsWidth, y1: highPoint[1],
              x2: highPoint[0] + endsWidth, y2: highPoint[1]
          },
          style: style
      }, {
          type: 'line',		// vertical
          shape: {
              x1: highPoint[0], y1: highPoint[1],
              x2: lowPoint[0], y2: lowPoint[1]
          },
          style: style
      }, {
          type: 'line',
          shape: {
              x1: lowPoint[0] - endsWidth, y1: lowPoint[1],
              x2: lowPoint[0] + endsWidth, y2: lowPoint[1]
          },
          style: style
      }]
  };
}

/*
  renderItem function for Polygon
  author: helgasoft.com
  used also by e_band2
*/
function riPolygon(params, api) {
    if (params.context.rendered) return;
    params.context.rendered = true;
    
    // set polygon vertices
    let points = [];
    let i = 0;
    while (typeof api.value(0,i) != 'undefined' && !isNaN(api.value(0,i))) {
    	points.push(api.coord([api.value(0,i), api.value(1,i)]));
    	i++;
    }
    var color = api.visual('color');

    return {
        type: 'polygon',
        shape: {
            points: echarts.graphic.clipPointsByRect(points, {
                x: params.coordSys.x,
                y: params.coordSys.y,
                width: params.coordSys.width,
                height: params.coordSys.height
            })
        },
        style: api.style({
            fill: color,
            stroke: echarts.color.lift(color)
        })
    };
}
