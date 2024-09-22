Live website: https://richarddmorey.github.io/pcurveAppTest

![p curve demonstration app banner](https://richarddmorey.github.io/pcurveAppTest/og_image.png)

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

If you fork this repo and deploying your own version, note that the webpage gets copied to the 'build' directory (not the 'dist' directory) so your GitHub Actions script will need to relfect this: e.g:

```
      - name: Upload artifact
        uses: actions/upload-pages-artifact@v3
        with:
          # Upload build folder
          path: './build'
```
Also ensure you change the `base` in `vite.config.js` to reflect your own repository name.
