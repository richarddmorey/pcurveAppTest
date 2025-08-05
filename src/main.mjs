
const statusMessage = document.getElementById("status-message")
statusMessage.innerHTML = (crossOriginIsolated ? "🟢" : "🌕") + " WebR Loading…"

import { Base64 } from 'js-base64';

import { togglehalfls, backdropScroll, backdropStyle, findTestStatistics, webRVersion } from './functions';

statusMessage.innerHTML = (crossOriginIsolated ? "🟢" : "🌕") + `WebR Loaded! [${webRVersion}]`

const backdrop = document.getElementById("backdrop");
const halftoggle= document.getElementById("halftoggle");
const lstoggle= document.getElementById("lstoggle");
const loadingoverlay = document.getElementById("loadingoverlay");
const textInput = document.getElementById("TApcurve");

// Decode the data in the query string to load a saved analysis
const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
if(urlParams.has('halfp')){
    halftoggle.checked = true;
}
if(urlParams.has('lstest')){
  lstoggle.checked = true;
}
if(urlParams.has('data')){
  const urldata = urlParams.get('data');
  if(Base64.isValid(urldata)){
    const decoded = Base64.decode(urldata);
    textInput.innerHTML = decoded;
  }
}

// Remove the loading overlay and "wiggle" the help button to attract attention
loadingoverlay.style.display = 'none';
textInput.disabled = false;
[...document.getElementsByClassName("wiggle1")].forEach(
    (el) => {
      el.style.animationPlayState="running";
    });

// Set important events
halftoggle.onchange = togglehalfls;
lstoggle.onchange = togglehalfls;
textInput.oninput = findTestStatistics;

/* Because text in textareas can't be easily styled automatically
 * (hence we can't highlight it based on the input), the text 
 * highlighting in the input textarea is accomplished by an 
 * elaborate trick whereby the actual textarea is transparent,
 * and a matching "backdrop" is shown with identical text (that
 * can be styled). However, this does not play nice with resizing 
 * the textArea
 * 
 * The two events below ensure that when textArea is resized
 * (or, actually, when the mouse button is pressed) the
 * highlighting is "turned off" (really, the textarea is displayed
 * instead of the backdrop). The backdrop is turned back on when the 
 * mouse button is released. The timer ensures that it only happens
 * if the mouse is held for more than 100ms.
 */
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
// Ensure the backdrop scrolls with the input textArea
textInput.onscroll = backdropScroll;

// Look in the input textarea to see if there is a valid analysis
findTestStatistics();
// Ensure that the backdrop scroll (that controls the highlighting in the input textarea) matches the input textarea
backdropScroll();

