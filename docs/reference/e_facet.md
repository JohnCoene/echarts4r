# Facet

Create facets for multiple plots.

## Usage

``` r
e_facet(
  e,
  rows = NULL,
  cols = NULL,
  legend_pos = "top",
  legend_space = 10,
  margin_trbl = c(t = 2, r = 2, b = 5, l = 2),
  h_panel_space = NULL,
  v_panel_space = NULL
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- rows, cols:

  Number of rows and columns. If both are \`NULL\` the number of rows
  and columns will be determined automatically.

- legend_pos:

  Position of the legend. One of "top", "right", "bottom", "left".
  Determines to which side the \`legend_space\` argument applies.

- legend_space:

  Space between legend and plot area. The entered number will be used as
  percentage.

- margin_trbl:

  Adjusts the size of the outside margin around the plotting area.
  Default is \`c(t = 2, r = 2, b = 5, l = 2)\`. Numbers are used as
  percentage of total plotting area. To change only e.g. two sides
  \`c("r" = 8, "l" = 8)\` could be used, other sides will use defaults.

- h_panel_space, v_panel_space:

  Horizontal and vertical spacing between the individual grid elements.
  Expects numeric input, which will be used as percentage of total
  plotting area. Default \`NULL\` will automatically add some panel
  spacing for low dimensional grids.

## Details

Each serie, i.e.:
[`e_bar`](https://echarts4r.john-coene.com/reference/e_bar.md) will be
plotted against a facet.

## Examples

``` r
group_size <- 20
n_groups <- 13
df <- data.frame("day" = rep(1:group_size, times=n_groups),
                 "temperature" = runif(group_size * n_groups, 10, 40),
                 "location" = rep(LETTERS[1:n_groups], each=group_size))

df |>
  group_by(location) |>
  e_charts(day) |>
  e_line(temperature) |>
  e_facet(rows = 4, cols=4, legend_pos = "top", legend_space = 12)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true,"gridIndex":0},{"show":true,"gridIndex":1},{"show":true,"gridIndex":2},{"show":true,"gridIndex":3},{"show":true,"gridIndex":4},{"show":true,"gridIndex":5},{"show":true,"gridIndex":6},{"show":true,"gridIndex":7},{"show":true,"gridIndex":8},{"show":true,"gridIndex":9},{"show":true,"gridIndex":10},{"show":true,"gridIndex":11},{"show":true,"gridIndex":12}],"xAxis":[{"type":"value","gridIndex":0},{"type":"value","gridIndex":1},{"type":"value","gridIndex":2},{"type":"value","gridIndex":3},{"type":"value","gridIndex":4},{"type":"value","gridIndex":5},{"type":"value","gridIndex":6},{"type":"value","gridIndex":7},{"type":"value","gridIndex":8},{"type":"value","gridIndex":9},{"type":"value","gridIndex":10},{"type":"value","gridIndex":11},{"type":"value","gridIndex":12}],"legend":{"data":["A","B","C","D","E","F","G","H","I","J","K","L","M"]},"series":[{"data":[{"value":[1,16.30348939914256]},{"value":[2,31.75099335610867]},{"value":[3,27.93044273741543]},{"value":[4,20.10420412523672]},{"value":[5,30.82639295840636]},{"value":[6,20.97438569413498]},{"value":[7,19.30051418254152]},{"value":[8,34.17286839336157]},{"value":[9,15.50503734266385]},{"value":[10,18.0996528849937]},{"value":[11,35.33074754988775]},{"value":[12,13.67014801828191]},{"value":[13,38.47410226473585]},{"value":[14,39.00710937334225]},{"value":[15,16.13614219473675]},{"value":[16,16.84716324089095]},{"value":[17,12.89741884917021]},{"value":[18,32.02912205830216]},{"value":[19,19.4253344531171]},{"value":[20,28.0687786731869]}],"yAxisIndex":0,"xAxisIndex":0,"name":"A","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,22.72128029027954]},{"value":[2,36.49106191471219]},{"value":[3,13.42889382271096]},{"value":[4,19.17959278915077]},{"value":[5,11.80074254749343]},{"value":[6,39.8658039001748]},{"value":[7,23.21987554430962]},{"value":[8,22.12895293720067]},{"value":[9,14.84546091873199]},{"value":[10,37.89504900341853]},{"value":[11,12.90128584718332]},{"value":[12,26.19967479724437]},{"value":[13,33.32700861385092]},{"value":[14,26.35424013016745]},{"value":[15,16.67104197200388]},{"value":[16,37.0075039868243]},{"value":[17,35.66175414482132]},{"value":[18,36.21605064487085]},{"value":[19,31.51454702019691]},{"value":[20,35.84173826966435]}],"yAxisIndex":1,"xAxisIndex":1,"name":"B","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,37.81228076899424]},{"value":[2,11.99696670053527]},{"value":[3,39.18956982670352]},{"value":[4,14.95464778039604]},{"value":[5,29.4151216535829]},{"value":[6,11.23660410754383]},{"value":[7,30.29374450445175]},{"value":[8,22.15048810699955]},{"value":[9,21.29736826522276]},{"value":[10,35.57943675899878]},{"value":[11,22.66519353957847]},{"value":[12,14.50812484137714]},{"value":[13,31.08126317150891]},{"value":[14,18.79118286306038]},{"value":[15,12.91982499649748]},{"value":[16,22.72356272442266]},{"value":[17,23.60932127805427]},{"value":[18,21.71146127860993]},{"value":[19,11.09835080336779]},{"value":[20,17.70458315266296]}],"yAxisIndex":2,"xAxisIndex":2,"name":"C","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,35.50842385739088]},{"value":[2,23.62270736601204]},{"value":[3,16.78313560783863]},{"value":[4,20.19508405821398]},{"value":[5,37.766045674216]},{"value":[6,22.72244513733312]},{"value":[7,33.05749748600647]},{"value":[8,20.64536089543253]},{"value":[9,14.37441304093227]},{"value":[10,12.17551332199946]},{"value":[11,25.36407517967746]},{"value":[12,26.53527813963592]},{"value":[13,18.0230250582099]},{"value":[14,12.70003436831757]},{"value":[15,22.69848759053275]},{"value":[16,10.42492271168157]},{"value":[17,29.59193035727367]},{"value":[18,38.24710458982736]},{"value":[19,24.60241358028725]},{"value":[20,24.57949170144275]}],"yAxisIndex":3,"xAxisIndex":3,"name":"D","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,18.95496854092926]},{"value":[2,38.18955352762714]},{"value":[3,39.96799923479557]},{"value":[4,17.85599790280685]},{"value":[5,12.31355614261702]},{"value":[6,14.86576966941357]},{"value":[7,28.88694797642529]},{"value":[8,10.52860125899315]},{"value":[9,11.17744263028726]},{"value":[10,24.47895033983514]},{"value":[11,14.64169699233025]},{"value":[12,34.51452808687463]},{"value":[13,35.50299767171964]},{"value":[14,10.45221270760521]},{"value":[15,25.41536253644153]},{"value":[16,15.3828228241764]},{"value":[17,11.35485923150554]},{"value":[18,26.22685116250068]},{"value":[19,20.62514831312001]},{"value":[20,25.93061953317374]}],"yAxisIndex":4,"xAxisIndex":4,"name":"E","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,13.12391068320721]},{"value":[2,18.75108233653009]},{"value":[3,18.14676118548959]},{"value":[4,39.59074386162683]},{"value":[5,12.56635919678956]},{"value":[6,28.71403175173327]},{"value":[7,15.19163444172591]},{"value":[8,25.38492257939652]},{"value":[9,33.10920520918444]},{"value":[10,26.85781276784837]},{"value":[11,25.26440967107192]},{"value":[12,11.5475012245588]},{"value":[13,31.25371109927073]},{"value":[14,29.84897248912603]},{"value":[15,12.65188280725852]},{"value":[16,10.34399112686515]},{"value":[17,17.7890836680308]},{"value":[18,13.07025248417631]},{"value":[19,35.57934769429266]},{"value":[20,32.11532003479078]}],"yAxisIndex":5,"xAxisIndex":5,"name":"F","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,21.21868461603299]},{"value":[2,28.3074565581046]},{"value":[3,25.41074053850025]},{"value":[4,35.9306134050712]},{"value":[5,35.68971860455349]},{"value":[6,32.13322405237705]},{"value":[7,11.68562442064285]},{"value":[8,33.98216797038913]},{"value":[9,11.69974159216508]},{"value":[10,20.89113681344315]},{"value":[11,26.32017988245934]},{"value":[12,35.96153979888186]},{"value":[13,27.91823631851003]},{"value":[14,21.73294046428055]},{"value":[15,25.68361331243068]},{"value":[16,26.56547104706988]},{"value":[17,14.18010599678382]},{"value":[18,32.12693759007379]},{"value":[19,27.56870436947793]},{"value":[20,31.86221315059811]}],"yAxisIndex":6,"xAxisIndex":6,"name":"G","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,16.15177395520732]},{"value":[2,38.03754788124934]},{"value":[3,12.00118559179828]},{"value":[4,29.1083446261473]},{"value":[5,36.70021946309134]},{"value":[6,21.97623057523742]},{"value":[7,22.43636606726795]},{"value":[8,19.01601066580042]},{"value":[9,38.75565592432395]},{"value":[10,17.91972253704444]},{"value":[11,12.27985854493454]},{"value":[12,31.20483862468973]},{"value":[13,22.89876527385786]},{"value":[14,34.57772513618693]},{"value":[15,33.25809333240613]},{"value":[16,31.78853573277593]},{"value":[17,14.90623469697312]},{"value":[18,21.60429408540949]},{"value":[19,20.4877359373495]},{"value":[20,23.96582934539765]}],"yAxisIndex":7,"xAxisIndex":7,"name":"H","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,29.45769669255242]},{"value":[2,37.79956379439682]},{"value":[3,37.32434239936993]},{"value":[4,23.50988460239023]},{"value":[5,35.12141350656748]},{"value":[6,25.04333137301728]},{"value":[7,26.03560280054808]},{"value":[8,15.62122752889991]},{"value":[9,30.42113698553294]},{"value":[10,38.56747268466279]},{"value":[11,15.57865953072906]},{"value":[12,18.43501554103568]},{"value":[13,25.02668473869562]},{"value":[14,11.1241763131693]},{"value":[15,26.79737854050472]},{"value":[16,31.16237484151497]},{"value":[17,18.483888853807]},{"value":[18,28.5865171183832]},{"value":[19,27.78873686445877]},{"value":[20,16.93914353614673]}],"yAxisIndex":8,"xAxisIndex":8,"name":"I","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,36.91654325928539]},{"value":[2,18.47896984312683]},{"value":[3,12.46657577576116]},{"value":[4,33.03944991435856]},{"value":[5,26.38278347905725]},{"value":[6,16.72986856428906]},{"value":[7,34.41874615848064]},{"value":[8,24.28146749036387]},{"value":[9,25.46368867857382]},{"value":[10,25.8717706380412]},{"value":[11,24.54085789620876]},{"value":[12,14.11927849054337]},{"value":[13,33.74341938411817]},{"value":[14,13.92304606037214]},{"value":[15,25.40070529095829]},{"value":[16,24.40952636767179]},{"value":[17,32.39310466684401]},{"value":[18,19.46743638953194]},{"value":[19,22.38699494628236]},{"value":[20,21.22527001891285]}],"yAxisIndex":9,"xAxisIndex":9,"name":"J","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,24.81166854035109]},{"value":[2,23.76306721009314]},{"value":[3,39.44764482555911]},{"value":[4,17.45122040389106]},{"value":[5,37.98248725710437]},{"value":[6,38.80090015241876]},{"value":[7,21.3511062390171]},{"value":[8,19.52846232336015]},{"value":[9,38.30308706266806]},{"value":[10,12.9798145708628]},{"value":[11,13.76899881288409]},{"value":[12,15.74852084508166]},{"value":[13,34.77628875523806]},{"value":[14,12.58889640914276]},{"value":[15,13.59108643606305]},{"value":[16,36.5523543022573]},{"value":[17,37.58686690358445]},{"value":[18,36.80654375115409]},{"value":[19,22.89055962115526]},{"value":[20,14.5018643559888]}],"yAxisIndex":10,"xAxisIndex":10,"name":"K","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,17.72952975705266]},{"value":[2,20.86694907862693]},{"value":[3,13.28402818180621]},{"value":[4,14.22269374132156]},{"value":[5,27.97656780574471]},{"value":[6,18.8282056315802]},{"value":[7,11.77002922166139]},{"value":[8,35.71394474245608]},{"value":[9,33.75741705764085]},{"value":[10,28.09749323641881]},{"value":[11,23.67604522267357]},{"value":[12,19.42037588683888]},{"value":[13,16.69831472914666]},{"value":[14,11.11327144317329]},{"value":[15,33.89778369804844]},{"value":[16,21.97216163622215]},{"value":[17,14.37020017765462]},{"value":[18,39.96701252413914]},{"value":[19,27.64659351203591]},{"value":[20,21.27865528920665]}],"yAxisIndex":11,"xAxisIndex":11,"name":"L","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,39.78041878668591]},{"value":[2,20.78049548901618]},{"value":[3,39.3185348296538]},{"value":[4,13.13604933442548]},{"value":[5,16.75548238214105]},{"value":[6,39.12051453953609]},{"value":[7,12.88739792769775]},{"value":[8,24.37107069185004]},{"value":[9,27.91507023619488]},{"value":[10,25.755483165849]},{"value":[11,31.25382419908419]},{"value":[12,14.75726961391047]},{"value":[13,30.5998980300501]},{"value":[14,36.34825750254095]},{"value":[15,15.86964695947245]},{"value":[16,33.31200887681916]},{"value":[17,22.51801596023142]},{"value":[18,30.33900863258168]},{"value":[19,24.73877296550199]},{"value":[20,39.47454213630408]}],"yAxisIndex":12,"xAxisIndex":12,"name":"M","type":"line","coordinateSystem":"cartesian2d"}],"grid":[{"height":"15.75%","width":"19.5%","top":"14%","left":"2%"},{"height":"15.75%","width":"19.5%","top":"14%","left":"27.5%"},{"height":"15.75%","width":"19.5%","top":"14%","left":"53%"},{"height":"15.75%","width":"19.5%","top":"14%","left":"78.5%"},{"height":"15.75%","width":"19.5%","top":"35.75%","left":"2%"},{"height":"15.75%","width":"19.5%","top":"35.75%","left":"27.5%"},{"height":"15.75%","width":"19.5%","top":"35.75%","left":"53%"},{"height":"15.75%","width":"19.5%","top":"35.75%","left":"78.5%"},{"height":"15.75%","width":"19.5%","top":"57.5%","left":"2%"},{"height":"15.75%","width":"19.5%","top":"57.5%","left":"27.5%"},{"height":"15.75%","width":"19.5%","top":"57.5%","left":"53%"},{"height":"15.75%","width":"19.5%","top":"57.5%","left":"78.5%"},{"height":"15.75%","width":"19.5%","top":"79.25%","left":"2%"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
