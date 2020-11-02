// render error bars
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
  added support for grouped bars, barGap and barCategoryGap by helgasoft.com
	To test in R:
  grps <- 5; rpt <- grps*2
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
function renderErrorBar2(params, api) {
  let oss = JSON.parse(sessionStorage.getItem('ErrorBar.oss'));
  if (oss===null || !Object.keys(oss).length) return null;   // cant work without it

  function findMax(xx) {  	
  	// find max barGap or barCategoryGap
  	// if caller is e_bar they are all the same, but could be different if caller is e_list
  	let out = null;
  	let tmp = oss.map(d => d[xx]).filter(d => d);
  	if (tmp.length > 0) {
  		tmp = Math.max(...tmp.map(d => Number(d[0].replace('%', ''))) ); 
  		if (isFinite(tmp)) out = tmp + '%';
  	}
  	return out;
  }

  let xValue = api.value(0);
  let highPoint = api.coord([xValue, api.value(1)]);
  let lowPoint = api.coord([xValue, api.value(2)]);
  let halfWidth = api.size([1, 0])[0] * 0.1;
	
  let csil = api.currentSeriesIndices().length;		// always even
  if (csil > 2) {
  	let bgm = findMax('barGap');
  	let bcgm = findMax('barCategoryGap');
  	let olay = { count: csil/2 };
  	olay.barGap = bgm!==null ? bgm : '30%';		// '30%' is default for e_bar
  	olay.barCategoryGap = bcgm!==null ? bcgm : '20%';
  	let barLayouts = api.barLayout(olay);
  	
	  let idx = params.seriesIndex - (params.seriesIndex<oss.length ? 0 : oss.length);	
  	// idx is index of related main bar
  	let mbar = 0;
  	api.currentSeriesIndices().forEach( (item, index) => {
  		if (item == idx) {
  			highPoint[0] += barLayouts[mbar].offsetCenter;
  			halfWidth = barLayouts[mbar].width /2;
  		}
  		mbar++;
  	});
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
              x1: highPoint[0] - halfWidth, y1: highPoint[1],
              x2: highPoint[0] + halfWidth, y2: highPoint[1]
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
              x1: lowPoint[0] - halfWidth, y1: lowPoint[1],
              x2: lowPoint[0] + halfWidth, y2: lowPoint[1]
          },
          style: style
      }]
  };
}
