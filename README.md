## Live website: https://richarddmorey.github.io/pcurveAppTest

[![p curve demonstration app banner](https://richarddmorey.github.io/pcurveAppTest/og_image.png)](https://richarddmorey.github.io/pcurveAppTest)

This is a p curve app meant to support Morey and Davis-Sober's paper "On the statistical properties of the p-curve meta-analytic procedure" (in preparation; title subject to change). This app is meant to improve on Simonsohn's app in various ways, including:

* Improved transparency through the ability to comment test statistics (e.g. by labelling lines with the source of the test statistic)
* Improved transparency through comments around the test statistics, because the app gracefully ignores non-test statistics
* Improved transparency and reproducibility through being able to save and link to analyses
* Improved transparency as both test and data tables can be saved as CSV files
* Improved visualization
* Improved value in demonstrations, as analysis updates on the fly
* Improved p curve code (e.g., doesn't truncate p values to 2.2e-16)
* Bundled examples
* Static website design with [WebR](https://docs.r-wasm.org/webr/latest/) and [Github actions](https://vitejs.dev/guide/static-deploy#github-pages) will allow anyone to remix and redeploy; no PHP server needed

Based on [hrbrmstr](https://rud.is/)'s [WebR + Vite + Glitch: Fully In-browser WebR App Development](https://vite-webr-glitch.glitch.me/) example (see his other [excellent WebR examples](https://rud.is/webr-experiments/) too).

### R code

The underlying R functions can be found in [`public/pcurve.R`](https://github.com/richarddmorey/pcurveAppTest/blob/main/public/pcurve.R).

You can run the functions yourself if you like, e.g., by loading the functions from GitHib:

```r
pcurve_functions_url <- "https://raw.githubusercontent.com/richarddmorey/pcurveAppTest/refs/heads/main/public/pcurve.R"
tf <- tempfile()

download.file(pcurve_functions_url, destfile = tf)

source(tf)
```

*Note that you should not make a habit of running code directly from GitHub, and you should review all code before `source()`ing it.* If you plan to run the code more than once, you should save it locally and source from there.

Then running the appropriate `pcurve` functions with data:

```r
pcurve_prep(
  stat = c("z", "f"),
  df1 = c(NA, 10),
  df2 = c(NA, 25),
  value = c(3, 6),
  comment = c("z stat","f stat"),
  line = c("","")
) |>
  pcurve()
```

...which will produce the following output:

```
$prep_table
  stat df1 df2 value comment line     string      ncp        lp significant
z chi2   1  NA     9  z stat             Z=3 2.336492 -5.914579        TRUE
f    f  10  25     6  f stat      F(10,25)=6 8.533369 -8.921079        TRUE
  contr_probit contr_log
z    -1.136522  5.837694
f    -1.969764 11.850693

$tests
  test alphaBound teststat_log    pval_log teststat_probit  pval_probit k_total
1   EV       0.05     17.68839 0.001419698       -3.106286 0.0009472653       2
  k_sig
1     2

```

### Deploying your own version of the online app

You can have a working version of this app for free in less than 5 minutes using GitHub pages. You are welcome to edit this app to make it do whatever you like (but please cite our work if you do).

1. **Fork the repository.**
2. **Enable GitHub Actions.** Under setttings, enable Github Pages on your forked repository. The source should be "GitHub Actions".
4. **Run the included deployment action.** Go to "Actions" and select the Action "Deploy static content to Pages". Run this action manually ("Run workflow")
5. **Replace the URLs in the README.** Go to Settings/Pages and note the URL of your site. Then go to `README.md` and edit this file so that the URL of the live website is the URL of fork. You need to edit multiple URLs in the readme.

After you do the above, every time you push a change to your fork GitHub Actions will automatically rebuild the app for you.

If you make substantial changes to the app, you might also change the app name, version, and author in the `package.json` file.

#### Testing locally

Once you have your own fork, use `git` (e.g. GitHub desktop) to clone the repository to your own computer. You'll need to [install `node.js`](https://nodejs.org/en/download) if you don't already have it installed.

If you have `node.js` installed, navigate to the repository's root directory in a terminal window. Then:

1. Install the necessary packages with `npm`:

```
npm ci
```

2. Build the site with `vite`:

```
npm run build
```

3. Serve the site with `vite`:

```
npm run serve
```

4. Using a browser, navigate to the local URL indicated by the output of the serve command. Kill the server with `Ctrl-C`.

5. Change the app code, then rebuild (from step 2) and start the server again to test. Once you're satisfied with your changes, you can push the changes to GitHub and then the app will be automatically rebuild.

