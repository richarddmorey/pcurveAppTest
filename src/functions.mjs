/*
* Javascript functions to make the p curve app work
* Richard D. Morey, 2024
*/

NodeList.prototype.map = Array.prototype.map;

import { Base64 } from 'js-base64';
import * as Plot from "@observablehq/plot"
import { WebR } from 'webr';

// Initialize WebR
const webR = new WebR();
await webR.init();
export const webRVersion = await webR.evalRString(`R.version.string`)

// Grab the URL from the browser so that we know where important files will be
const pathname = window.location.pathname.replace("index.html","");

await webR.installPackages(
  ['knitr', 'memoise'], 
  { 
    repos: `${window.location.origin}${pathname}webr/repo/`
  }
);

// Download and source the necessary R script to get the functions
await webR.evalR(`tf = tempfile();download.file('${window.location.origin}${pathname}pcurve.R',tf);source(tf)`);

var lastString = ""; // global to store the last input analysed, to prevent rerunning if nothing has changed.

/**
 * Call the R code to return the p curve analysis tables from the results of the 
 * regular expression matching
 *
 * @param {string} stat - The test statistic ("z", "chi2", "r", "t", or "f").
 * @param {string} df1 - The numerator, or lone, degrees of freedom for the test statistic (may be null for z statistics)
 * @param {string} df2 - The denominator degrees of freedom for the test statistic (may be null for all but f statistics)
 * @param {string} value - The value of the test statistic
 * @param {string} comment - The comment component of the line from the input
 * @param {string} line - The full text of the line from the input
 * @returns {function} A debounced version of the function.
 */
const pcurve = await webR.evalR('\\(stat,df1,df2,value,comment,line) make_tables(pcurve_prep(stat,df1,df2,value,comment,line), pvalcols = c("pval_log","pval_probit"), prep_class = "table", test_class="pcurvetab")');

/**
 * Call the R code to return the data necessary for creating the plot from the results of the 
 * regular expression matching. pplot1() gets the main plot data (study points).
 * 
 * pplot1() and pplot2() are split into separate calls to get the conversion 
 * to the proper types (data frames) in each call. The make_plot_data R function 
 * is memoised so that it doesn't take any extra time for the second call.
 *
 * @param {string} stat - The test statistic ("z", "chi2", "r", "t", or "f").
 * @param {string} df1 - The numerator, or lone, degrees of freedom for the test statistic (may be null for z statistics)
 * @param {string} df2 - The denominator degrees of freedom for the test statistic (may be null for all but f statistics)
 * @param {string} value - The value of the test statistic
 * @param {string} comment - The comment component of the line from the input
 * @param {string} line - The full text of the line from the input
 * @returns {function} A debounced version of the function.
 */
const pplot1 = await webR.evalR('\\(stat,df1,df2,value,comment,line) make_plot_data(pcurve_prep(stat,df1,df2,value,comment,line))[["plotdata"]]');

/**
 * Call the R code to return the data necessary for creating the plot from the results of the 
 * regular expression matching. pplot2() gets the Fisher test information.
 * 
 * pplot1() and pplot2() are split into separate calls to get the conversion 
 * to the proper types (data frames) in each call. The make_plot_data R function 
 * is memoised so that it doesn't take any extra time for the second call.
 *
 * @param {string} stat - The test statistic ("z", "chi2", "r", "t", or "f").
 * @param {string} df1 - The numerator, or lone, degrees of freedom for the test statistic (may be null for z statistics)
 * @param {string} df2 - The denominator degrees of freedom for the test statistic (may be null for all but f statistics)
 * @param {string} value - The value of the test statistic
 * @param {string} comment - The comment component of the line from the input
 * @param {string} line - The full text of the line from the input
 * @returns {function} A debounced version of the function.
 */
const pplot2 = await webR.evalR('\\(stat,df1,df2,value,comment,line) make_plot_data(pcurve_prep(stat,df1,df2,value,comment,line))[["plotdata2"]]');

// Regular expressions for matching important elements of test statistics
const numRegex0 = '-?(0|[1-9]\\d*)?(\\.\\d+)?(?<=\\d)';
const numRegex1 = '(?<value>-?(0|[1-9]\\d*)?(\\.\\d+)?(?<=\\d)(e-?(0|[1-9]\\d*))?)';
const commentRegex = '#(?<comment>.*)'

// Regular expressions for matching test statistics
const statRegex = [
  new RegExp(`^\\s*(?<stat>z)\\s*=\\s*${numRegex1}\\s*(?:${commentRegex})?$`,'i'),
  new RegExp(`^\\s*(?<stat>[rt]|chi2)\\(\\s*(?<df1>${numRegex0})\\s*\\)\\s*=\\s*${numRegex1}\\s*(?:${commentRegex})?$`,'i'),
  new RegExp(`^\\s*(?<stat>F)\\(\\s*(?<df1>${numRegex0})\\s*,\\s*(?<df2>${numRegex0})\\s*\\)\\s*=\\s*${numRegex1}\\s*(?:${commentRegex})?$`,'i')
];

// Various important elements of the display, so we can refer to them within the functions
const statusMessage = document.getElementById("status-message")
const textInput = document.getElementById("TApcurve");
const simpletext = document.getElementById("simpletext");
const tab = document.getElementById("table1")
const tab2 = document.getElementById("table2")
const displayurl = document.getElementById("displayurl")
const ojsplot = document.getElementById("ojs-plot");
const backdrop = document.getElementById("backdrop");
const halftoggle= document.getElementById("halftoggle");
const lstoggle= document.getElementById("lstoggle");


/* Important utility functions
 *
 */

/**
 * Create a debounced function: that is, it only runs every so often
 * so that (e.g.) not every keypress leads to a re-run.
 *
 * See https://codepen.io/ondrabus/pen/WNGaVZN
 *
 * @param {string} func - The function to be debounced.
 * @param {string} timeout - the amount of time between runs, in milliseconds.
 * @returns {function} A debounced version of the function.
 */
function debounce(func, timeout = 200){
  let timer;
  return (...args) => {
    clearTimeout(timer);
    timer = setTimeout(() => { func.apply(this, args); }, timeout);
  };
}

/**
 * Escape all the special characters in a string so that it can be used in a regex 
 *
 * See https://stackoverflow.com/a/6969486/1129889
 *
 * @param {string} string - The string to be escaped.
 * @returns {function} An escaped version of the function.
 */
function escapeRegExp(string) {
    return string.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'); // $& means the whole matched string
}

/**
 * Create a display version of a (particularly large) number  
 *
 * @param {number} x - A number to display nicely.
 * @returns {string} A string containing a nicely formatted version of the number.
 */
function niceNum(x){
  const d = 2
  const p = Math.ceil(Math.abs(Math.log10(x)))
  const f = parseFloat(x.toFixed(p+d-1)) + "";
  const e = x.toExponential(d-1);
  return Array.from(f).length > Array.from(e).length ? e : f;
}

/**
 * Toggle the test table rows that represent the half p curve to be displayed or not. Running
 * this function just hides or unhides the appropriate rows based on the setting.
 */
export function togglehalfls(){
  setURLstring();
  if(halftoggle.checked & lstoggle.checked){
    tab.querySelectorAll('tbody>tr:nth-child(even)>td').map(x=>x.style.display='');
    tab.querySelectorAll('tbody>tr:nth-child(n+5)>td').map(x=>x.style.display='');
  }else if(halftoggle.checked){
    tab.querySelectorAll('tbody>tr:nth-child(even)>td').map(x=>x.style.display='');
    tab.querySelectorAll('tbody>tr:nth-child(n+5)>td').map(x=>x.style.display='none');
  }else if(lstoggle.checked){
    tab.querySelectorAll('tbody>tr:nth-child(even)>td').map(x=>x.style.display='none');
    tab.querySelectorAll('tbody>tr:nth-child(5)>td').map(x=>x.style.display='');
  }else{
    tab.querySelectorAll('tbody>tr:nth-child(even)>td').map(x=>x.style.display='none');
    tab.querySelectorAll('tbody>tr:nth-child(n+5)>td').map(x=>x.style.display='none');
  }
}

/**
 * Ensure that the input textarea's backdrop is synced to the 
 * textarea. The backdrop is where the color highlighting is done.
 */
export function backdropScroll(){
  backdrop.scrollTop = textInput.scrollTop;
  backdrop.scrollLeft = textInput.scrollLeft;
}

/**
 * Ensure that the css of the input textarea's backdrop matches the css
 * of the textarea in all but the important ways. This ensures that the 
 * highlighting and text are is visible (because the textarea is actually
 * transparent!).
 */
export function backdropStyle(){
  let css = window.getComputedStyle(textInput);
    let cssstring = "";
    for (let i = 0; i < css.length; i++) {
        cssstring +=(css[i] +': '+css.getPropertyValue(css[i])+";");
    }
    backdrop.style = cssstring;
    backdrop.style.position = 'absolute';
    backdrop.style.zIndex = '-1'
    backdrop.style.overflow = 'hidden';
    backdrop.style.backgroundColor = '#ffffff';
    backdrop.style.resize = 'none';
    backdrop.style["-webkit-text-fill-color"] = "rgba(0,0,0,0)";
    backdrop.style.color ="rgba(0,0,0,0)";
    backdrop.style['border-color'] = "rgba(0,0,0,0)";
}

/**
 * Search a regular expression result from a line of input for a valid test statistic.
 * The values must be valid, as well as the degrees of freedom.
 *
 * @param {object} m - An object created by a search of a line by the regular 
 *                   expressions defined above (that match the various test statistics)
 * @returns {boolean} Is there a valid test statistic in the line?
 */
function testMatch(m){
  const stat = m.stat.toLowerCase();
  switch(stat){
    case "f":
      return parseFloat(m.df1)>=1 & parseFloat(m.df2)>=1 & parseFloat(m.value)>=0;
      break;
    case "chi2":
      return parseFloat(m.df1)>=1 & parseFloat(m.value)>=0;
      break;
    case "r":
      return parseFloat(m.df1)>=3 & parseFloat(m.value)>=-1 & parseFloat(m.value)<=1;
      break;
    case "z":
      return !isNaN(parseFloat(m.value));
      break;
    case "t":
      return parseFloat(m.df1)>=1 & !isNaN(parseFloat(m.value));
      break;
    default:
      return false;
  }
}

/**
 * Send the values from the input off to the appropriate R functions to
 * perform the actual p curve analysis
 *
 * @param {object} matchesObj - An object created by a search of a line by the regular 
 *                   expressions defined above (that match the various test statistics),
 *                   and that contains valid values (see testMatch())
 */
async function doAnalysis(matchesObj){
  const newString = JSON.stringify(matchesObj);
  
  if(lastString === newString){
    return;
  }
  
  const plotdata = await pplot1.exec(
    matchesObj.stat,
    matchesObj.df1,
    matchesObj.df2,
    matchesObj.value,
    matchesObj.comment,
    matchesObj.line
    );
  const d3data = await plotdata.toD3();
  if(d3data.length == 0){
    wipeAnalysis();
    return;
  }
  document.querySelectorAll(".onlynosig").map((x)=>{x.style.display='none'});
  document.querySelectorAll(".onlysig").map((x)=>{x.style.display='inline-block'});

  const fisherdata = await pplot2.exec(
    matchesObj.stat,
    matchesObj.df1,
    matchesObj.df2,
    matchesObj.value,
    matchesObj.comment,
    matchesObj.line
    );
  updatePlot(d3data, await fisherdata.toArray());

  
  const pcurve_table = await pcurve(
    matchesObj.stat,
    matchesObj.df1,
    matchesObj.df2,
    matchesObj.value,
    matchesObj.comment,
    matchesObj.line
  );
  
  tab.innerHTML = pcurve_table.values[1];
  togglehalfls();
  tab.querySelector("table").classList.add('table','table-striped');
  tab.querySelector("thead").classList.add('thead-dark');
  
  tab2.innerHTML = pcurve_table.values[0];
  tab2.querySelector("table").classList.add('table','table-striped','sortable');
  tab2.querySelector("thead").classList.add('thead-dark');
  
  lastString = newString; 
}

/**
 * A debounced version of doAnalysis() above
 */
const doAnalysis2 = debounce(async (x) => {await doAnalysis(x)});

/**
 * Parse the text input, search for test statistics, perfrom the analysis, and tidy up
 */
export async function findTestStatistics(){
  backdropStyle();
  backdrop.innerHTML = "";
  simpletext.innerHTML = "";
  const str = textInput.value;
  setURLstring();
  if(str === ''){
    wipeAnalysis();
    return;
  }
  const lines = str.split(/\r?\n|\r|\n/g);
  const matches = lines.flatMap(function(y, idx){
    var matchArray;
    for(var i=0;i<statRegex.length;i++){
      matchArray = statRegex[i].exec(y);
      if(matchArray){
        if(testMatch(matchArray.groups)){
          const noCommentMatch = matchArray.input.match("^.*?(?=#)");
          if(noCommentMatch !== null){
            simpletext.innerHTML += noCommentMatch[0] + '\n';
          }else{
            simpletext.innerHTML += matchArray.input + '\n';
          }
          matchArray.groups.line = idx + 1;
          if(matchArray.groups.comment === undefined){
            matchArray.groups.comment = "";
          }else{
            const highlightCommentRE = '#'+matchArray.groups.comment;
            y = y.replace(highlightCommentRE,"<span class='commentinput'>$&<span>");
          }
          
          backdrop.innerHTML += `${y}<br/>`;
          return matchArray.groups;
        }
      }
    }
    backdrop.innerHTML += `<span class='invalidinput'>${y}</span><br/>`;
    return [];
  });
  if(!matches.length){
    wipeAnalysis();
    return;
  }
  const matchesObj = {
    line: [],
    stat: [],
    df1: [],
    df2: [],
    value: [],
    comment: []
  }
  for(let i=0;i<matches.length;i++){
    matchesObj.line.push(matches[i].line);
    matchesObj.stat.push(matches[i].stat);
    matchesObj.df1.push(parseFloat(matches[i].df1));
    matchesObj.df2.push(parseFloat(matches[i].df2));
    matchesObj.value.push(parseFloat(matches[i].value));
    matchesObj.comment.push(matches[i].comment);
  }
  
  await doAnalysis2(matchesObj);
  
}

function setURLstring(){
  const str    = textInput.value;
  const lstest = lstoggle.checked ? "lstest&" : "";
  const halfp  = halftoggle.checked ? "halfp&" : "";
  displayurl.value = `${window.location.origin}${window.location.pathname}?${lstest}${halfp}data=${Base64.encodeURI(str)}`;
  if ('URLSearchParams' in window) {
    const url = new URL(window.location)
    if(lstest){
      url.searchParams.set("lstest", "");
    }else{
      url.searchParams.delete("lstest");
    }    
    if(halfp){
      url.searchParams.set("halfp", "");
    }else{
      url.searchParams.delete("halfp");
    }
    url.searchParams.set("data", Base64.encodeURI(str));
    history.replaceState(null, '', url);
  }
}


/**
 * Reset all the outputs (tables, graphs, etc) as though there is no valid input
 */
function wipeAnalysis(){
  lastString = "";
  document.querySelectorAll(".onlysig").map((x)=>{x.style.display='none'});
  document.querySelectorAll(".onlynosig").map((x)=>{x.style.display='inline-block'});
  ojsplot.innerHTML = "";
  tab.innerHTML = "";
  tab2.innerHTML = "";
}

/**
 * Create the main visualization using d3.js
 *
 * @param {object} data - An object containing the data points for the graph (i.e. the studies)
 * @param {object} fisher - An object containing the information for the Fisher test (log) visualization 
 */
async function updatePlot(data,fisher) {

  const k = data.length;
  const fisherPoint = [{x:fisher[0],y:1/(2*k),statistic:-2*k*Math.log(fisher[0]/.05)}];
  const fisherLine = [
    {x:fisher[1],y:1/(2*k)},
    {x:fisher[2],y:1/(2*k)}
    ];
  ojsplot.replaceChildren(
    
    Plot.plot({
      marginBottom: 80,
      marginTop: 30,
      x: {type: "log", label: "p value (log scale)"},
      y: {domain: [0,1], label: "Empirical cumulative probability"},
      style: {
        background: "#ffffff",
        color: "#000000",
        fontSize: "14px"
      },
      marks: 
        [
          Plot.axisX({tickFormat: niceNum,tickRotate: -90}),
          Plot.ruleX([.05]),
          Plot.ruleY([0]),
          Plot.ruleY([1]),
          Plot.line(fisherLine, {x: "x", y: "y", strokeOpacity: 0.5, stroke: "steelblue", strokeWidth: 5, title: d => 'Expected 95% range for Fisher\'s statistic under the null'}),
          Plot.dot(fisherPoint, {x: "x", y: "y", r: 8, symbol: "diamond", fill: "plum", fillOpacity: .7, channels: {"Fisher's statistic": d=>`𝛘²(${2*k})=${d.statistic.toFixed(3)}`,"geometric mean p": d=>d.x}, tip: {format:{y: false, x: false}}}),
          Plot.areaX(data, {y: "Fp", x1: "lo", x2: "up" , fill: "steelblue",fillOpacity: 0.5}),
          Plot.line(data, {y: "Fp", x: "med", stroke: "royalblue", strokeWidth: 4}),
          Plot.line(data, {x: "pval", y: "Fp", curve: "step-after", stroke: "lightgray", strokeWidth: 3}),
          Plot.dot(data, {x: "pval", y: "Fp", r: 6, fill: "currentColor", channels: {comment: "comment", "Line#": "line", input: "input_string", p: "p_string"}, tip: {format:{x: false, y: false}}})
        ],
    })
  )
}
