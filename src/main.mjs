document.getElementById('TApcurve').disabled = true;

const statusMessage = document.getElementById("status-message")
statusMessage.innerHTML =
  (crossOriginIsolated ? "🟢" : "🌕") + " WebR Loading…"

import * as Plot from "@observablehq/plot"

import { WebR } from "https://webr.r-wasm.org/latest/webr.mjs";
import { Base64 } from 'js-base64';

const tab = document.getElementById("table1")
const displayurl = document.getElementById("displayurl")
const ojsBarplot = document.getElementById("ojs-barplot");
const textInput = document.getElementById("TApcurve");
const backdrop = document.getElementById("backdrop");

const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
if(urlParams.has('data')){
  const urldata = urlParams.get('data');
  if(Base64.isValid(urldata)){
    const decoded = Base64.decode(urldata);
    textInput.innerHTML = decoded;
  }
}

const webR = new WebR();
await webR.init();
await webR.installPackages(['xtable', 'memoise']);

const webRVersion = await webR.evalRString(`R.version.string`)

statusMessage.innerHTML = (crossOriginIsolated ? "🟢" : "🌕") + `WebR Loaded! [${webRVersion}]`

await webR.evalR(`tf = tempfile();download.file('${window.location.origin}/pcurve.R',tf);source(tf)`);

const pcurve = await webR.evalR('\\(stat,df1,df2,value,comment) xtab(pcurve(pcurve_prep(stat,df1,df2,value,comment))[[2]], class="pcurvetab")');

document.getElementById('TApcurve').disabled = false;

const alphaBound = 0.05;

const numRegex0 = '-?(0|[1-9]\\d*)?(\\.\\d+)?(?<=\\d)';
const numRegex1 = '(?<value>-?(0|[1-9]\\d*)?(\\.\\d+)?(?<=\\d)(e-?(0|[1-9]\\d*))?)';
const commentRegex = '#(?<comment>.*)'

const statRegex = [
  new RegExp(`^\\s*(?<stat>z)\\s*=\\s*${numRegex1}\\s*(?:${commentRegex})?$`,'i'),
  new RegExp(`^\\s*(?<stat>[rt]|chi2)\\(\\s*(?<df1>${numRegex0})\\s*\\)\\s*=\\s*${numRegex1}\\s*(?:${commentRegex})?$`,'i'),
  new RegExp(`^\\s*(?<stat>F)\\(\\s*(?<df1>${numRegex0})\\s*,\\s*(?<df2>${numRegex0})\\s*\\)\\s*=\\s*${numRegex1}\\s*(?:${commentRegex})?$`,'i')
];


function backdropScroll(){
  backdrop.scrollTop = textInput.scrollTop;
}

function backdropStyle(){
  let css = window.getComputedStyle(textInput);
    let cssstring = "";
    for (let i = 0; i < css.length; i++) {
        cssstring +=(css[i] +': '+css.getPropertyValue(css[i])+";");
    }
    backdrop.style = cssstring;
    backdrop.style.position = 'absolute';
    backdrop.style.zIndex = '-1'
    backdrop.style.overflow = 'hidden';
    backdrop.style.backgroundColor = '#fff';
    backdrop.style.fontColor ='#f00';
    backdrop.style.resize = 'none';
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

async function findTestStatistics(){
  backdropStyle();
  backdrop.innerHTML = "";
  const str = textInput.value;
  displayurl.value = `${window.location.origin}/?data=${Base64.encodeURI(str)}`;
  if ('URLSearchParams' in window) {
    const url = new URL(window.location)
    url.searchParams.set("data", Base64.encodeURI(str))
    history.pushState(null, '', url);
  }
  if(str === '') return;
  const lines = str.split(/\r?\n|\r|\n/g);
  const matches = lines.flatMap(function(y){
    var matchArray;
    for(var i=0;i<statRegex.length;i++){
      matchArray = statRegex[i].exec(y);
      if(matchArray){
        if(testMatch(matchArray.groups)){
          backdrop.innerHTML += `${y}<br/>`;
          matchArray.groups.comment = matchArray.groups.comment === undefined ? "" : matchArray.groups.comment;
          return matchArray.groups;
        }
      }
    }
    backdrop.innerHTML += `<span style='background-color: pink'>${y}</span><br/>`;
    return [];
  });
  const matchesObj = {
    stat: [],
    df1: [],
    df2: [],
    value: [],
    comment: []
  }
  for(let i=0;i<matches.length;i++){
    matchesObj.stat.push(matches[i].stat);
    matchesObj.df1.push(parseFloat(matches[i].df1));
    matchesObj.df2.push(parseFloat(matches[i].df2));
    matchesObj.value.push(parseFloat(matches[i].value));
    matchesObj.comment.push(matches[i].comment);
  }
  
  const pcurve_table = await pcurve(
    matchesObj.stat,
    matchesObj.df1,
    matchesObj.df2,
    matchesObj.value,
    matchesObj.comment
  );
  tab.innerHTML = pcurve_table.values;
}

async function updatePlot() {

/*  
  ojsBarplot.replaceChildren(
    Plot.plot({
      style: {
        background: "#202e32",
        color: "#dfdcb9",
      },
      marks: [Plot.rectY(numbers)],
    })
  )
*/
}

// await updatePlot()

textInput.oninput = findTestStatistics;
textInput.onpaste = findTestStatistics;
textInput.onmousedown = function(){
  backdrop.style.display = 'none';
};
textInput.onmouseup = backdropStyle;
textInput.onscroll = backdropScroll;

//textInput.onkeypress = findTestStatistics;
//textInput.onchange = findTestStatistics;

findTestStatistics();
backdropScroll();
