# Wordcloud

Draw a wordcloud.

## Usage

``` r
e_cloud(e, word, freq, color, rm_x = TRUE, rm_y = TRUE, ...)

e_cloud_(e, word, freq, color = NULL, rm_x = TRUE, rm_y = TRUE, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- word, freq:

  Terms and their frequencies.

- color:

  Word color.

- rm_x, rm_y:

  Whether to remove x and y axis, defaults to `TRUE`.

- ...:

  Any other option to pass, check See Also section.

## See also

[official documentation](https://github.com/ecomfe/echarts-wordcloud)

## Examples

``` r
words <- function(n = 5000) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

tf <- data.frame(
  terms = words(100),
  freq = rnorm(100, 55, 10)
) |>
  dplyr::arrange(-freq)

tf |>
  e_color_range(freq, color) |>
  e_charts() |>
  e_cloud(terms, freq, color, shape = "circle", sizeRange = c(3, 15))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"type":"wordCloud","data":[{"value":78.15849301450461,"name":"DQIEY3351L","textStyle":{"color":"#F6EFA6"}},{"value":77.00885652483714,"name":"AUQML8890T","textStyle":{"color":"#F5EAA4"}},{"value":76.38995234899991,"name":"ULUCL2999R","textStyle":{"color":"#F4E7A2"}},{"value":76.16530742830059,"name":"UWFEH6222M","textStyle":{"color":"#F4E6A2"}},{"value":74.60755930712961,"name":"QKVGQ8378X","textStyle":{"color":"#F3DF9E"}},{"value":71.13081285889021,"name":"RTNTG9159S","textStyle":{"color":"#EFCF97"}},{"value":70.39376495155655,"name":"RVJXH7692T","textStyle":{"color":"#EECC95"}},{"value":70.26737997181787,"name":"DEFSX6600C","textStyle":{"color":"#EECC95"}},{"value":70.16556529445093,"name":"WJDQW6693K","textStyle":{"color":"#EECB95"}},{"value":69.62425734368152,"name":"OAWPP8746Y","textStyle":{"color":"#EDC994"}},{"value":69.30290161460823,"name":"DYCXY5316Q","textStyle":{"color":"#EDC793"}},{"value":69.28358850255424,"name":"QWVJT9251A","textStyle":{"color":"#EDC793"}},{"value":69.22340275905583,"name":"LDPVA3000R","textStyle":{"color":"#EDC793"}},{"value":68.19700102947753,"name":"JMRSU3309A","textStyle":{"color":"#ECC291"}},{"value":67.55820916285022,"name":"ECWWS1821S","textStyle":{"color":"#EBBF8F"}},{"value":67.23164016185305,"name":"KXTIU9234H","textStyle":{"color":"#EABE8F"}},{"value":66.97478247944777,"name":"KQAIK3149F","textStyle":{"color":"#EABD8E"}},{"value":66.58752180474988,"name":"NZFDF3298G","textStyle":{"color":"#EABB8D"}},{"value":66.37045938520943,"name":"VWJPX1951L","textStyle":{"color":"#E9BA8D"}},{"value":66.23909321821517,"name":"XQGBJ1735D","textStyle":{"color":"#E9BA8C"}},{"value":65.99244551634135,"name":"GNRAF0716Q","textStyle":{"color":"#E9B88C"}},{"value":65.74657121924588,"name":"HUZHN7992P","textStyle":{"color":"#E9B78B"}},{"value":65.13571835374846,"name":"TSOTX6567U","textStyle":{"color":"#E8B58A"}},{"value":64.6133969597919,"name":"BKBSM1212Z","textStyle":{"color":"#E7B289"}},{"value":63.62710826554081,"name":"MIRTN7454X","textStyle":{"color":"#E6AE87"}},{"value":63.53394680085335,"name":"OJNAS7682V","textStyle":{"color":"#E6AD87"}},{"value":63.52173584584716,"name":"QYVYT6581X","textStyle":{"color":"#E6AD87"}},{"value":63.40522625855864,"name":"UOXVL4573I","textStyle":{"color":"#E6AD87"}},{"value":63.27869297615099,"name":"NRYZB1954F","textStyle":{"color":"#E5AC86"}},{"value":62.7662539998112,"name":"RCYDH4522O","textStyle":{"color":"#E5AA85"}},{"value":62.62012389019607,"name":"EUZCH7974C","textStyle":{"color":"#E5A985"}},{"value":62.22232422667251,"name":"LPPBM4825O","textStyle":{"color":"#E4A884"}},{"value":62.02411712708128,"name":"PYXXE0458T","textStyle":{"color":"#E4A784"}},{"value":61.58526506382546,"name":"SVWKR2124E","textStyle":{"color":"#E3A583"}},{"value":61.5505667154527,"name":"SEQPN3261Y","textStyle":{"color":"#E3A583"}},{"value":61.45142724550194,"name":"YIOMZ3929L","textStyle":{"color":"#E3A482"}},{"value":61.23653457997048,"name":"MPERM3917I","textStyle":{"color":"#E3A382"}},{"value":61.06232667538287,"name":"CSXSE6102X","textStyle":{"color":"#E2A282"}},{"value":60.60050140634962,"name":"FXQKA1877H","textStyle":{"color":"#E2A081"}},{"value":60.55861005892056,"name":"GMKGI0906K","textStyle":{"color":"#E2A081"}},{"value":60.2272117886162,"name":"ZFYVQ7046H","textStyle":{"color":"#E19F80"}},{"value":60.05599115347081,"name":"NVHJR2777I","textStyle":{"color":"#E19E80"}},{"value":59.7380131019073,"name":"XEZSQ0953G","textStyle":{"color":"#E19C7F"}},{"value":57.95561895272746,"name":"UWOLM9289D","textStyle":{"color":"#DE947B"}},{"value":57.92127542416649,"name":"SBIDH2936K","textStyle":{"color":"#DE947B"}},{"value":57.38889683387322,"name":"FFRRP1345M","textStyle":{"color":"#DD927A"}},{"value":57.1799654886131,"name":"HOVGR1473K","textStyle":{"color":"#DD917A"}},{"value":56.89759677003288,"name":"HQSQM0769N","textStyle":{"color":"#DC9079"}},{"value":56.49297681515692,"name":"USVSR1639M","textStyle":{"color":"#DC8E78"}},{"value":56.27603168070977,"name":"CJVHZ1992A","textStyle":{"color":"#DC8D78"}},{"value":56.18178060520712,"name":"ISSSK2080Q","textStyle":{"color":"#DB8C78"}},{"value":56.12995340582098,"name":"XOYNU5848X","textStyle":{"color":"#DB8C77"}},{"value":55.91607576535964,"name":"NJSUF1660H","textStyle":{"color":"#DB8B77"}},{"value":55.53534542371712,"name":"AVUIF9854C","textStyle":{"color":"#DA8976"}},{"value":55.3168816397411,"name":"QLYZZ0239Z","textStyle":{"color":"#DA8876"}},{"value":54.94553289947044,"name":"GJFIL5416P","textStyle":{"color":"#DA8775"}},{"value":54.56588091175762,"name":"OENNU5311E","textStyle":{"color":"#D98574"}},{"value":54.28917443227643,"name":"SJIUN1054Y","textStyle":{"color":"#D98474"}},{"value":54.23207041475527,"name":"WGNXZ9855L","textStyle":{"color":"#D88374"}},{"value":54.22789137552483,"name":"DLWCZ3038O","textStyle":{"color":"#D88374"}},{"value":54.1848914892975,"name":"EPMXL4311V","textStyle":{"color":"#D88373"}},{"value":53.76866341939589,"name":"JPBJB8012Q","textStyle":{"color":"#D88273"}},{"value":53.70003448398932,"name":"DHWKS7756Y","textStyle":{"color":"#D88173"}},{"value":53.58602547317883,"name":"RYUXZ4871X","textStyle":{"color":"#D88172"}},{"value":53.57261607255158,"name":"UPJBE8249V","textStyle":{"color":"#D88172"}},{"value":53.49559644680315,"name":"MGGEH0711N","textStyle":{"color":"#D88172"}},{"value":53.05100638735933,"name":"LQGWG0837W","textStyle":{"color":"#D78072"}},{"value":52.65630471762929,"name":"ZRVDF1927U","textStyle":{"color":"#D77F71"}},{"value":52.63434334929794,"name":"WFNUE9804A","textStyle":{"color":"#D77F71"}},{"value":52.34787377842466,"name":"YOYGA4575R","textStyle":{"color":"#D67E70"}},{"value":52.3250041666887,"name":"KEESQ8734W","textStyle":{"color":"#D67E70"}},{"value":51.76656544010935,"name":"TMZUX5493I","textStyle":{"color":"#D67D6F"}},{"value":51.31238391679513,"name":"YGVBV5129X","textStyle":{"color":"#D67C6F"}},{"value":51.30208064507134,"name":"JLQEZ3788Q","textStyle":{"color":"#D67C6F"}},{"value":51.29107263657217,"name":"VDPLN0060J","textStyle":{"color":"#D57C6F"}},{"value":51.18401123226504,"name":"ZYBNJ0062S","textStyle":{"color":"#D57B6E"}},{"value":51.05287956315304,"name":"MQWFU6089Z","textStyle":{"color":"#D57B6E"}},{"value":50.4759771920446,"name":"VDJKU9847X","textStyle":{"color":"#D57A6D"}},{"value":50.04620989542276,"name":"XHLVF0662Z","textStyle":{"color":"#D4796D"}},{"value":49.84030626299101,"name":"BMFWQ9279N","textStyle":{"color":"#D4786C"}},{"value":49.70561684322619,"name":"CTABJ6666J","textStyle":{"color":"#D4786C"}},{"value":49.52282528022572,"name":"IDSSN3368R","textStyle":{"color":"#D4776C"}},{"value":49.01489401186336,"name":"MJLFF9637I","textStyle":{"color":"#D3766B"}},{"value":48.65642188228362,"name":"NCDEB3466O","textStyle":{"color":"#D3756A"}},{"value":48.23422173201202,"name":"QNRES8173A","textStyle":{"color":"#D3746A"}},{"value":47.99331327014117,"name":"JFWML6799J","textStyle":{"color":"#D27469"}},{"value":47.72005632395989,"name":"PJLEK6471J","textStyle":{"color":"#D27369"}},{"value":47.63113249088868,"name":"PDJAK5058M","textStyle":{"color":"#D27369"}},{"value":47.61737638964851,"name":"KTRPO9337O","textStyle":{"color":"#D27369"}},{"value":45.70278195429547,"name":"HYYKA6599P","textStyle":{"color":"#D06E65"}},{"value":44.12527212761986,"name":"CPKOQ5637L","textStyle":{"color":"#CE6A63"}},{"value":43.94783256467942,"name":"PGDRS8866G","textStyle":{"color":"#CE6A63"}},{"value":41.31887189558029,"name":"JVAJR7607E","textStyle":{"color":"#CC635E"}},{"value":40.38831414519948,"name":"OCAYV1895A","textStyle":{"color":"#CB615D"}},{"value":38.63207881042331,"name":"CNBJX6195I","textStyle":{"color":"#C95C5A"}},{"value":38.18426973970691,"name":"USDIJ2800N","textStyle":{"color":"#C85B59"}},{"value":36.86181578008797,"name":"UMOWV5663U","textStyle":{"color":"#C75757"}},{"value":36.04030819074579,"name":"ERCJS8630G","textStyle":{"color":"#C65556"}},{"value":35.16952168903823,"name":"GPOJT0621K","textStyle":{"color":"#C55354"}},{"value":29.72247072391995,"name":"CYAYI6230T","textStyle":{"color":"#BF444C"}}],"shape":"circle","sizeRange":[3,15]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
