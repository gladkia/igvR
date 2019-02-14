"use strict";
//import css from './css/IGV.css';
//var igv = require('igv.js.npm')
//var igv = require('igv.esm.min.js')
//import igv from "igv.esm.min.js"
//require('igv.js.npm/igv-all.css')
$ = require('jquery');
require('jquery-ui-bundle');
var hub = require("browservizjs")  // see https://github.com/paul-shannon/browservizjs
//----------------------------------------------------------------------------------------------------
var IGV = (function(hub){

  var hub = hub;

//----------------------------------------------------------------------------------------------------
// development aid: used to ensure that the right "this" -- corresponding to the IGV object --
// is avaialable when needed
function checkSignature(obj, callersName)
{
   var success = false;  // be pessimistic
   if(Object.keys(obj).indexOf("signature") >= 0 && obj.signature.indexOf("IGV" == 0)){
      success = true;
      }

   if(!success){
      console.log("--- error: not a IGV object: " + callersName);
      console.log(JSON.stringify(Object.keys(obj)))
      throw new Error("object is not a IGV this!");
      }

} // checkSignature
//----------------------------------------------------------------------------------------------------
function addMessageHandlers()
{
   var self = this;  // the context of the current object, IGV
   checkSignature(self, "addMessageHandlers");

   self.hub.addMessageHandler("ping",               respondToPing.bind(self));
   self.hub.addMessageHandler("setGenome",          setGenome.bind(self));

   self.hub.addMessageHandler("getTrackNames",      getTrackNames.bind(self));
   self.hub.addMessageHandler("removeTracksByName", removeTracksByName.bind(self));

   self.hub.addMessageHandler("showGenomicRegion",  showGenomicRegion.bind(self));
   self.hub.addMessageHandler("getGenomicRegion",   getGenomicRegion.bind(self));


   self.hub.addMessageHandler("displayBedTrackFromUrl",  displayBedTrackFromUrl.bind(self));
   self.hub.addMessageHandler("displayVcfTrackFromUrl",   displayVcfTrackFromUrl.bind(self));
   self.hub.addMessageHandler("displayQuantitativeTrackFromUrl",   displayQuantitativeTrackFromUrl.bind(self));


   self.hub.addMessageHandler("addBedTrackFromHostedFile", addBedTrackFromHostedFile.bind(self));

   self.hub.addMessageHandler("addBedGraphTrackFromDataFrame",  addBedGraphTrackFromDataFrame.bind(self));

   self.hub.addMessageHandler("getTrackNames",      getTrackNames.bind(self));
   self.hub.addMessageHandler("removeTracksByName", removeTracksByName.bind(self));



} // addMessageHandlers
//----------------------------------------------------------------------------------------------------
// called out of the hub once the web page (the DOM) is ready (fully loaded).
// tv(this) is explicitly bound to this function
//   1. create tabs
//   2. window resize handler is bound and assignes
function initializeUI()
{
   var self = this;
   checkSignature(self, "initializeUI");

   var trenaVizDiv = $("#trenaVizDiv");

   var activateFunction = function(event, ui){
      if(ui.newPanel.is("#cyOuterDiv")){
        console.log("cy!");
        self.handleWindowResize();
        if(self.cyjs != null){
           self.cyjs.resize();
	   }
        } // cyOuterDiv
      else if(ui.newPanel.is("#igvOuterDiv")){
         console.log("IGV!");
         }
      else{
         console.log("unrecognized tab activated");
	 }
      }; // activateFunction

   var tabOptions = {activate: activateFunction};
   setTimeout(function() {$("#trenaVizDiv").tabs(tabOptions)}, 0);

   var bound_handleWindowResize = this.handleWindowResize.bind(self);
   setTimeout(function(){bound_handleWindowResize();}, 250)
   $(window).resize(bound_handleWindowResize);

}  // initializeUI
//----------------------------------------------------------------------------------------------------
function handleWindowResize ()
{
   //console.log("trenaviz, handleWindowResize");
   //console.log("jquery version: " + $().jquery)

   var tabsDiv = $("#trenaVizDiv");

     // i have no grasp of why document is needed to track height, window for width.
     // pshannon (18 feb 2018) jquery 3.3.1

   var browserWindowHeight = $(document).innerHeight();
   var browserWindowWidth  = $(window).innerWidth();
   tabsDiv.width(0.98  * browserWindowWidth);
   tabsDiv.height(0.92 * browserWindowHeight);
   $("#cyDiv").width($("#cyMenubarDiv").width()) // Width0.92 * tabsDiv.width());
   $("#cyDiv").height(tabsDiv.height() - 3 * $("#cyMenubarDiv").height()); //tabsDiv.height()-100);
   $("#igvOuterDiv").height($("#trenaVizDiv").height() - (3 * $(".ui-tabs-nav").height()))

} // handleWindowResize
//--------------------------------------------------------------------------------
function respondToPing (msg)
{
   var self = this;
   checkSignature(self, "respondToPing")

   var return_msg = {cmd: msg.callback, status: "success", callback: "", payload: "pong"};
   self.hub.send(return_msg);

} // respondToPing
//------------------------------------------------------------------------------------------------------------------------
function setGenome(msg)
{
   var self = this;
   checkSignature(self, "setGenome")

   var supportedGenomes = ["hg19", "hg38", "mm10", "tair10"];
   var genomeName = msg.payload;
   var returnPayload = "";

   if(supportedGenomes.indexOf(genomeName) < 0){
      status = "failure"
      returnPayload = "error, unsupported genome: '" + genomeName + "'";
      var return_msg = {cmd: msg.callback, status: status, callback: "", payload: returnPayload};
      hub.send(return_msg);
      } // if unsupported genome

    $('a[href="#igvOuterDiv"]').click();
    setTimeout(function(){window.igvBrowser = initializeIGV(self, genomeName);}, 0);
    self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // setGenome
//----------------------------------------------------------------------------------------------------
function initializeIGV(self, genomeName)
{
   console.log("--- trenaViz, initializeIGV");

   checkSignature(self, "initializeIGV")

    var hg19_options = {
     flanking: 1000,
     showRuler: true,
     minimumBases: 5,

     reference: {id: "hg19"},
     tracks: [
        {name: 'Gencode v18',
              url: "https://s3.amazonaws.com/igv.broadinstitute.org/annotations/hg19/genes/gencode.v18.collapsed.bed",
         indexURL: "https://s3.amazonaws.com/igv.broadinstitute.org/annotations/hg19/genes/gencode.v18.collapsed.bed.idx",
         visibilityWindow: 2000000,
         displayMode: 'EXPANDED'
         }
        ]
     }; // hg19_options


    var hg38_options = {
       locus: initialLocus,
       minimumBases: 5,
       flanking: 1000,
       showRuler: true,
       genome: "hg38"
       }; // hg38_options


   var mm10_options = {
      locus: initialLocus,
      flanking: 2000,
      minimumBases: 5,
      showRuler: true,
      genome: "mm10"
      }; // mm10_options


   var tair10_options = {
         flanking: 2000,
	 showKaryo: false,
         showNavigation: true,
         minimumBases: 5,
         showRuler: true,
         reference: {id: "TAIR10",
                fastaURL: "https://igv-data.systemsbiology.net/static/tair10/Arabidopsis_thaliana.TAIR10.dna.toplevel.fa",
                indexURL: "https://igv-data.systemsbiology.net/static/tair10/Arabidopsis_thaliana.TAIR10.dna.toplevel.fa.fai",
                aliasURL: "https://igv-data.systemsbiology.net/static/tair10/chromosomeAliases.txt"
                },
         tracks: [
           {name: 'Genes TAIR10',
            type: 'annotation',
            visibilityWindow: 500000,
            url: "https://igv-data.systemsbiology.net/static/tair10/TAIR10_genes.sorted.chrLowered.gff3.gz",
            color: "darkred",
            indexed: true,
            height: 200,
            displayMode: "EXPANDED"
            },
            ]
          }; // tair10_options

   var igvOptions = null;

   switch(genomeName) {
      case "hg19":
         igvOptions = hg19_options;
         break;
      case "hg38":
         igvOptions = hg38_options;
         break;
       case "mm10":
         igvOptions = mm10_options;
         break;
       case "tair10":
         igvOptions = tair10_options;
         break;
         } // switch on genoneName

   $("#igvDiv").children().remove()

   console.log("--- trenaViz, igv:");
   console.log(igv)
   console.log("about to createBrowser");

   igv.createBrowser($("#igvDiv"), igvOptions)
       .then(function(browser){
           window.igvBrowser = browser;
           console.log("created igvBrowser in resolved promise")
           browser.on("locuschange", function(referenceFrame, chromLocString){
               self.chromLocString = chromLocString;
           });
       });

   // return(igvBrowser);

} // initializeIGV
//----------------------------------------------------------------------------------------------------
function showGenomicRegion(msg)
{
   var self = this;
   checkSignature(self, "showGenomicRegion")

   var regionString = msg.payload.regionString;
   window.igvBrowser.search(regionString)

   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // showGenomicRegion
//----------------------------------------------------------------------------------------------------
function getGenomicRegion(msg)
{
   var self = this;
   checkSignature(self, "getGenomicRegion")

   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: this.chromLocString});

} // getGenomicRegion
//----------------------------------------------------------------------------------------------------
function getTrackNames(msg)
{
   var self = this;
   checkSignature(self, "getTrackNames");

   var result = [];
   var count = window.igvBrowser.trackViews.length;

   for(var i=0; i < count; i++){
      var trackName = window.igvBrowser.trackViews[i].track.name;
      if(trackName.length > 0){
         result.push(trackName)
	 }
      } // for i

   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: result});

} // getTrackNames
//----------------------------------------------------------------------------------------------------
function removeTracksByName(msg)
{
   var self = this;
   checkSignature(self, "removeTracksByName")

   var trackNames = msg.payload;
   if(typeof(trackNames) == "string")
      trackNames = [trackNames];

   var count = window.igvBrowser.trackViews.length;

   for(var i=(count-1); i >= 0; i--){
     var trackView = window.igvBrowser.trackViews[i];
     var trackViewName = trackView.track.name;
     var matched = trackNames.indexOf(trackViewName) >= 0;
     //console.log(" is " + trackViewName + " in " + JSON.stringify(trackNames) + "? " + matched);
     if (matched){
        window.igvBrowser.removeTrack(trackView.track);
        } // if matched
     } // for i

   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});


} // removeTracksByName
//----------------------------------------------------------------------------------------------------
function displayBedTrackFromUrl(msg)
{
   var self = this;
   checkSignature(self, "displayBedTrackFromUrl")

   var trackName = msg.payload.name;
   var bedFileName = msg.payload.bedFileName;
   var displayMode = msg.payload.displayMode;
   var color = msg.payload.color;
   var trackHeight = msg.payload.trackHeight;

   //var url = window.location.href + "?" + bedFileName;
   var url = msg.payload.dataURL
   console.log("=== displayBedTrackFromUrl, msg");
   console.log(msg)

   var config = {format: "bed",
                 name: trackName,
                 url: url,
                 indexed:false,
                 displayMode: displayMode,
                 sourceType: "file",
                 color: color,
		 height: trackHeight,
                 type: "annotation"};

   console.log(JSON.stringify(config));

   window.igvBrowser.loadTrack(config);

   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // displayBedTrackFromDataFrame
//----------------------------------------------------------------------------------------------------
function displayVcfTrackFromUrl(msg)
{
   var self = this;
   checkSignature(self, "displayVcfTrackFromUrl")

   var trackName = msg.payload.name;
   var displayMode = msg.payload.displayMode;
   var trackHeight = msg.payload.trackHeight;
   var dataURL = msg.payload.dataURL;
   var indexURL = msg.payload.indexURL;
   var indexed = indexURL.length > 0;
   var locationColor = msg.payload.color;      // rendered above the line
   var homvarColor = msg.payload.homvarColor;
   var hetvarColor = msg.payload.hetvarColor;
   var homrefColor = msg.payload.homrefColor;

   var config = {format: "vcf",
                 name: trackName,
                 url: dataURL,
                 indexURL: indexURL,
                 indexed: indexed,
                 displayMode: displayMode,
                 sourceType: "file",
		 height: trackHeight,
                 visibilityWindow: 1000000,
                 homvarColor: homvarColor,
                 hetvarColor: hetvarColor,
                 homrefColor: homrefColor,
                 color: locationColor,
                 type: "variant"};

   console.log(JSON.stringify(config));
   window.igvBrowser.loadTrack(config);

   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // displayVcfTrackFromUrl
//----------------------------------------------------------------------------------------------------
function displayQuantitativeTrackFromUrl(msg)
{
   var self = this;
   checkSignature(self, "displayBedTrackFromUrl")

   var trackName = msg.payload.name;
   var color = msg.payload.color;
   var trackHeight = msg.payload.trackHeight;
   var format = msg.payload.fileFormat
   var url = msg.payload.dataURL;
   var autoscale = msg.payload.autoscale;
   var min = msg.payload.min;
   var max = msg.payload.max;

   console.log("=== displayQuantitativeTrackFromUrl, msg");
   console.log(msg)

   var config = {format: format,
                 name: trackName,
                 url: url,
                 indexed: false,
                 sourceType: "file",
                 color: color,
		 height: trackHeight,
                 autoscale: autoscale,
                 min: min,
                 max: max,
                 type: "wig"};

   console.log(JSON.stringify(config));

   window.igvBrowser.loadTrack(config);

   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // displayBedTrackFromDataFrame
//----------------------------------------------------------------------------------------------------
function addBedGraphTrackFromDataFrame(msg)
{
   var self = this;
   checkSignature(self, "addBedGraphTrackFromDataFrame")

   console.log("--- addBedGraphTrackFromDataFrame");
   console.log(msg.payload)

   var trackName = msg.payload.name;
   var bedFileName = msg.payload.bedFileName;
   var displayMode = msg.payload.displayMode;
   var color = msg.payload.color;
   var minValue = msg.payload.min
   var maxValue = msg.payload.max
   var trackHeight = msg.payload.trackHeight;

   var url = window.location.href + "?" + bedFileName;

   var config = {format: "bedgraph",
                 name: trackName,
                 url: url,
                 min: minValue,
                 max: maxValue,
                 indexed:false,
                 displayMode: displayMode,
                 sourceType: "file",
                 color: color,
                 height: trackHeight,
                 type: "wig"};

   window.igvBrowser.loadTrack(config);
   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // addBedGraphTrackFromDataFrame
//----------------------------------------------------------------------------------------------------
function addBedTrackFromHostedFile(msg)
{
   var self = this;
   checkSignature(self, "addBedTrackFromHostedFile")

   console.log("=== addBedTrackFromFile");

   var trackName = msg.payload.name;
   var displayMode = msg.payload.displayMode;
   var color = msg.payload.color;
   var uri       = msg.payload.uri;
   var indexUri  = msg.payload.indexUri;
   var indexed = true;

   if(indexUri==null){
     indexed = false;
     }

    /***********
   var config = {format: "bed",
                 name: trackName,
                 url: uri,
                 indexed: indexed,
                 displayMode: displayMode,
                 color: color,
                 type: "annotation"};
    *******/


   if(indexed){
     config.indexURL = indexUri;
     }

   var config = {url: uri, name: trackName, color: color};
   console.log("---- about to loadTrack");
   console.log(config)
   window.igvBrowser.loadTrack(config);

   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // addBedTrackFromHostedFile
//----------------------------------------------------------------------------------------------------
  return({

    signature: "IGV 0.99.25",

    addMessageHandlers: addMessageHandlers,
    initializeUI: initializeUI,
    handleWindowResize: handleWindowResize.bind(this),
    hub: hub,
    igvBrowser: null,
    chromLocString: null
    });

}); // IGV
//----------------------------------------------------------------------------------------------------
console.log(1);
var IGV = IGV(hub);
console.log(2);
//hub.init();
console.log(3);
IGV.addMessageHandlers()
console.log(4);
hub.addOnDocumentReadyFunction(IGV.initializeUI.bind(IGV));
console.log(5);
hub.start();
console.log(6);
window.IGV = IGV;
console.log(7);
window.hub = hub;
console.log(8);
