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

### Deploying your own version

You can have a working version of this app for free in less than 5 minutes using GitHub pages. You are free to edit this app to make it do whatever you like (but please cite our work if you do).

1. **Fork** the repository.
2. **Enable GitHub Actions**. Under setttings, enable Github Pages on your forked repository. The source should be "GitHub Actions".
4. **Run the included deployment action**. Go to "Actions" and select the Action "Deploy static content to Pages". Run this action manually ("Run workflow")
5. **Replace the URLs in the README**. Go to Settings/Pages and note the URL of your site. Then go to `README.md` and edit this file so that the URL of the live website is the URL of fork. You need to edit multiple URLs in the readme; just search for "richarddmorey" and you'll find all the instances.

After you do the above, every time you push a change to your fork GitHub Actions will automatically rebuild the app for you.

