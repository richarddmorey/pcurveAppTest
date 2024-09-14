
NodeList.prototype.map = Array.prototype.map;

import { Base64 } from 'js-base64';
import * as Plot from "@observablehq/plot"
import { WebR } from 'webr';


const webR = new WebR();
await webR.init();


/*
await webR.evalR(`webr::mount("/my-library", "${window.location.origin}/library.data")`)
await webR.evalR('.libPaths(c(.libPaths(), "/my-library"))')
*/

await webR.installPackages(['knitr', 'memoise']);
export const webRVersion = await webR.evalRString(`R.version.string`)

const pathname = window.location.pathname.replace("index.html","");

console.log(`${window.location.origin}${pathname}pcurve.R`);

await webR.evalR(`tf = tempfile();download.file('${window.location.origin}${pathname}pcurve.R',tf);source(tf)`);

const pcurve = await webR.evalR('\\(stat,df1,df2,value,comment,line) make_tables(pcurve_prep(stat,df1,df2,value,comment,line), pvalcols = c("pval_log","pval_probit"), prep_class = "table", test_class="pcurvetab")');
const pplot = await webR.evalR('\\(stat,df1,df2,value,comment,line) make_plot_data(pcurve_prep(stat,df1,df2,value,comment,line))');



var lastString = "";

const numRegex0 = '-?(0|[1-9]\\d*)?(\\.\\d+)?(?<=\\d)';
const numRegex1 = '(?<value>-?(0|[1-9]\\d*)?(\\.\\d+)?(?<=\\d)(e-?(0|[1-9]\\d*))?)';
const commentRegex = '#(?<comment>.*)'

const statRegex = [
  new RegExp(`^\\s*(?<stat>z)\\s*=\\s*${numRegex1}\\s*(?:${commentRegex})?$`,'i'),
  new RegExp(`^\\s*(?<stat>[rt]|chi2)\\(\\s*(?<df1>${numRegex0})\\s*\\)\\s*=\\s*${numRegex1}\\s*(?:${commentRegex})?$`,'i'),
  new RegExp(`^\\s*(?<stat>F)\\(\\s*(?<df1>${numRegex0})\\s*,\\s*(?<df2>${numRegex0})\\s*\\)\\s*=\\s*${numRegex1}\\s*(?:${commentRegex})?$`,'i')
];
const statusMessage = document.getElementById("status-message")
const textInput = document.getElementById("TApcurve");
const simpletext = document.getElementById("simpletext");
const tab = document.getElementById("table1")
const tab2 = document.getElementById("table2")
const displayurl = document.getElementById("displayurl")
const ojsplot = document.getElementById("ojs-plot");
const backdrop = document.getElementById("backdrop");
const halftoggle= document.getElementById("halftoggle");


// Functions

// https://codepen.io/ondrabus/pen/WNGaVZN
function debounce(func, timeout = 200){
  let timer;
  return (...args) => {
    clearTimeout(timer);
    timer = setTimeout(() => { func.apply(this, args); }, timeout);
  };
}

// https://stackoverflow.com/a/6969486/1129889
function escapeRegExp(string) {
    return string.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'); // $& means the whole matched string
}

function niceNum(x){
  const d = 2
  const p = Math.ceil(Math.abs(Math.log10(x)))
  const f = parseFloat(x.toFixed(p+d-1)) + "";
  const e = x.toExponential(d-1);
  return Array.from(f).length > Array.from(e).length ? e : f;
}


export function togglehalf(){
  if(halftoggle.checked){
    tab.querySelectorAll('tbody>tr:nth-child(even)>td').map(x=>x.style.display='');
  }else{
    tab.querySelectorAll('tbody>tr:nth-child(even)>td').map(x=>x.style.display='none');
  }
}

export function backdropScroll(){
  backdrop.scrollTop = textInput.scrollTop;
  backdrop.scrollLeft = textInput.scrollLeft;
}


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

async function doAnalysis(matchesObj){
  const newString = JSON.stringify(matchesObj);
  
  if(lastString === newString){
    return;
  }
  
  await pplot(
    matchesObj.stat,
    matchesObj.df1,
    matchesObj.df2,
    matchesObj.value,
    matchesObj.comment,
    matchesObj.line
    );
  const plotdata = await webR.evalR(`plotdata`);
  const d3data = await plotdata.toD3();
  if(d3data.length == 0){
    wipeAnalysis();
    return;
  }
  document.querySelectorAll(".onlynosig").map((x)=>{x.style.display='none'});
  document.querySelectorAll(".onlysig").map((x)=>{x.style.display='inline-block'});

  const fisherdata = await webR.evalR(`plotdata2`);
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
  togglehalf();
  tab.querySelector("table").classList.add('table','table-striped');
  tab.querySelector("thead").classList.add('thead-dark');
  
  tab2.innerHTML = pcurve_table.values[0];
  tab2.querySelector("table").classList.add('table','table-striped','sortable');
  tab2.querySelector("thead").classList.add('thead-dark');
  
  lastString = newString; 
}

const doAnalysis2 = debounce(async (x) => {await doAnalysis(x)});

export async function findTestStatistics(){
  backdropStyle();
  backdrop.innerHTML = "";
  simpletext.innerHTML = "";
  const str = textInput.value;
  displayurl.value = `${window.location.origin}${window.location.pathname}?data=${Base64.encodeURI(str)}`;
  if ('URLSearchParams' in window) {
    const url = new URL(window.location)
    url.searchParams.set("data", Base64.encodeURI(str))
    history.pushState(null, '', url);
  }
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

function wipeAnalysis(){
  lastString = "";
  document.querySelectorAll(".onlysig").map((x)=>{x.style.display='none'});
  document.querySelectorAll(".onlynosig").map((x)=>{x.style.display='inline-block'});
  ojsplot.innerHTML = "";
  tab.innerHTML = "";
  tab2.innerHTML = "";
}

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
