
const statusMessage = document.getElementById("status-message")
statusMessage.innerHTML =
  (crossOriginIsolated ? "🟢" : "🌕") + " WebR Loading…"

import { Base64 } from 'js-base64';

import { togglehalf, backdropScroll, backdropStyle, findTestStatistics, webRVersion } from './functions';

statusMessage.innerHTML = (crossOriginIsolated ? "🟢" : "🌕") + `WebR Loaded! [${webRVersion}]`

const backdrop = document.getElementById("backdrop");
const halftoggle= document.getElementById("halftoggle");
const loadingoverlay = document.getElementById("loadingoverlay");
const textInput = document.getElementById("TApcurve");


const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
if(urlParams.has('data')){
  const urldata = urlParams.get('data');
  if(Base64.isValid(urldata)){
    const decoded = Base64.decode(urldata);
    textInput.innerHTML = decoded;
  }
}

loadingoverlay.style.display = 'none';
textInput.disabled = false;

halftoggle.onchange = togglehalf;
textInput.oninput = findTestStatistics;

var textInputTimer;
textInput.onmousedown = function(){
    textInputTimer = setTimeout(() => { 
      textInput.style.backgroundColor = "white";
      backdrop.style.display = "none"; 
    }, 100);
};
textInput.onmouseup = function(){
  if(textInputTimer !== null) clearTimeout(textInputTimer);
  textInput.style.backgroundColor = "";
  backdropStyle();
}
textInput.onscroll = backdropScroll;


findTestStatistics();
backdropScroll();

