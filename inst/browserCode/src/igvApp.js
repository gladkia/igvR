"use strict";
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
   self.hub.addMessageHandler("setCustomGenome",    setCustomGenome.bind(self));

   self.hub.addMessageHandler("showTrackLabels",    showTrackLabels.bind(self));
   self.hub.addMessageHandler("getTrackNames",      getTrackNames.bind(self));
   self.hub.addMessageHandler("removeTracksByName", removeTracksByName.bind(self));

   self.hub.addMessageHandler("showGenomicRegion",  showGenomicRegion.bind(self));
   self.hub.addMessageHandler("getGenomicRegion",   getGenomicRegion.bind(self));

   self.hub.addMessageHandler("zoomIn",             zoomIn.bind(self));
   self.hub.addMessageHandler("zoomOut",            zoomOut.bind(self));

   self.hub.addMessageHandler("setTrackClickFunction",  setTrackClickFunction.bind(self));

   self.hub.addMessageHandler("displayBedTrackFromUrl",  displayBedTrackFromUrl.bind(self));
   self.hub.addMessageHandler("displayVcfTrackFromUrl",   displayVcfTrackFromUrl.bind(self));
   self.hub.addMessageHandler("displayAlignmentTrackFromUrl",   displayAlignmentTrackFromUrl.bind(self));
   self.hub.addMessageHandler("displayQuantitativeTrackFromUrl",   displayQuantitativeTrackFromUrl.bind(self));

    self.hub.addMessageHandler("displayBedpeInteractionsTrackFromUrl",
			       displayBedpeInteractionsTrackFromUrl.bind(self));

   // self.hub.addMessageHandler("addBedTrackFromHostedFile", addBedTrackFromHostedFile.bind(self));

   self.hub.addMessageHandler("addBedGraphTrackFromDataFrame",  addBedGraphTrackFromDataFrame.bind(self));

   self.hub.addMessageHandler("getSVG", getSVG.bind(self));



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
   var  delay = msg.payload
   console.log("waiting " + delay + " msecs in ping");

   setTimeout(function(){
       console.log("ping wait complete")
       var return_msg = {cmd: msg.callback, status: "success", callback: "", payload: "pong"};
       self.hub.send(return_msg);
       }, delay)

} // respondToPing
//------------------------------------------------------------------------------------------------------------------------
function setGenome(msg)
{
   var self = this;
   checkSignature(self, "setGenome")

   var genomeName = msg.payload.toLowerCase();
   var supportedGenomes = ["hg38", "hg19", "hg18", "mm10", "gorgor4", "pantro4", "panpan2",
                           "susscr11", "bostau8", "canfam3", "rn6", "danrer11", "danrer10",
                           "dm6", "ce11", "saccer3",
                              // these last two are hosted on trena, aka igv-data.systemsbiology.net
                           "tair10", "pfal3d7"]

   var returnPayload = "";
   var foundIndex = supportedGenomes.indexOf(genomeName);
   var found = foundIndex >= 0

   if(!found){
     status = "failure"
     returnPayload = "error, unsupported genome: '" + genomeName + "'";
     var return_msg = {cmd: msg.callback, status: status, callback: "", payload: returnPayload};
     if(msg.callback != null){
        hub.send(return_msg);
        }
     else{
        return(return_msg)
	}
     } // if unsupported genome

    console.log("--- genome name recognized, proceeding with igv init: " + genomeName);

    $('a[href="#igvOuterDiv"]').click();

    initializeIGV(self, genomeName).then(
        function(result){
           console.log("=== successful return from async initializeIGV");
           if(msg.callback != null){
              self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});
              }
            else{
              return;
              }
           },
        function(error){
           console.log("=== failed return from async initializeIGV");
           status = "failure"
           returnPayload = "error, failure in intializeIGV: '" + genomeName + "'";
           var return_msg = {cmd: msg.callback, status: status, callback: "", payload: returnPayload};
           if(msg.callback != null){
              hub.send(return_msg);
              }
            else{
              return(return_msg);
              }
           });

} // setGenome
//----------------------------------------------------------------------------------------------------
async function setCustomGenome(msg)
{
   var self = this;
   checkSignature(self, "setCustomGenome")

   $('a[href="#igvOuterDiv"]').click();

   console.log("--- setCustomGenome")
   console.log(msg.payload)

   var options = {
       flanking: 2000,
       showKaryo: false,
       showNavigation: true,
       minimumBases: 5,
       showRuler: true,
       locus: msg.payload.initialLocus,
       genome: {id: msg.payload.id,
		name: msg.payload.genomeName,
                fastaURL: msg.payload.fastaURL,
                indexURL: msg.payload.fastaIndexURL,
		cytobandURL: msg.payload.cytobandURL,
		aliasURL: msg.payload.chromosomeAliasURL
               }
       };

    if(msg.payload.geneAnnotationName){
       options.tracks =  [
           {name: msg.payload.geneAnnotationName,
            type: 'annotation',
            visibilityWindow: msg.payload.visibilityWindow,
            url: msg.payload.geneAnnotationURL,
            color: msg.payload.geneAnnotationTrackColor,
	    indexed: true,
            height: msg.payload.geneAnnotationTrackHeight,
            displayMode: "EXPANDED"
           },
           ]
       }

   jsonObj = "{\"arguments\":\"track, popoverData\",\"body\":\"{console.log(track); console.log(popoverData);  console.log('track-click 4');}\"}"
   obj = JSON.parse(jsonObj)

   trackClickFunction = new Function(obj.arguments, obj.body)

     // todo: some duplicated code in this try block, stolen from initializeIGV
     //   the latter accepts a genomeName arg, which if chaned to a genome-specific options object
     //   would restore its generality.
     //   with that refactoring, we would have three different functions to create the options object:
     //     1) simple recipe for the genomes supported by igv (all that is needed is a genome name, igv "hg38")
     //     2) our own canned genomes (arabidopsis and pfal)
     //     3) the custom genome, with the key file urls provided by the user
   try{
      console.log("--- setCustomGenome, executing igv.createBrowser, line 231 of igvApp.js")
      var div = document.getElementById("igvDiv");
      igv.createBrowser(div, options)
          .then(function(browser){
              window.igvBrowser = browser;
              console.log("created igvBrowser in resolved promise")
              igvBrowser.on("locuschange", function(referenceFrame){
                 var chromLocString = referenceFrame.label;
                 self.chromLocString = chromLocString;
                 });
             igvBrowser.on("trackclick", trackClickFunction);
             self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});
             }); // then
       } // try
    catch(err){
      console.log(err);
      returnPayload = "error, failure in setCustomGenome: '" + msg.payload.genomeName + "'";
      var return_msg = {cmd: msg.callback, status: status, callback: "", payload: returnPayload};
      hub.send(return_msg);
      }

} // setCustomGenome
//----------------------------------------------------------------------------------------------------
// assumption: this function is called only with supported genomes.  see "setGenome" above
// the only client of this function.
async function initializeIGV(self, genomeName)
{
    console.log("-------- intializeIGV, line 258")
    
    checkSignature(self, "initializeIGV")

    $("#igvDiv").children().remove()

    const customSupportedGenomes = ["tair10", "pfal3d7"];

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

    var pfal3D7_options = {
         flanking: 2000,
	 showKaryo: false,
         showNavigation: true,
         minimumBases: 5,
         showRuler: true,
         reference: {id: "Pfal3D7",
             fastaURL: "https://igv-data.systemsbiology.net/static/Pfalciparum3D7/PlasmoDB-43_Pfalciparum3D7_Genome.fasta",
             indexURL: "https://igv-data.systemsbiology.net/static/Pfalciparum3D7/PlasmoDB-43_Pfalciparum3D7_Genome.fasta.fai",

             },
          tracks: [
            {name: 'genes',
             type: "annotation",
             nameField: "gene",
             url: "https://igv-data.systemsbiology.net/static/Pfalciparum3D7/PlasmoDB-43_Pfalciparum3D7.gff",
             format: 'gff',
             searchable: 'true',
             visibilityWindow: 4000000,
             displayMode: 'EXPANDED',
             height: 150,
             },
            ]
         }; // pfal3D7 options

    var genomeOptions;

       // we use lower-case names for simplicity but igv.js expects some names to
       // include traditional capitalization.   before sending a genomeName off to
       // igv's genome server, fix the capitalization

    genomeName = genomeName.toLowerCase();

    switch(genomeName){
       case "gorgor4":  genomeName = "gorGor4";  break;
       case "pantro4":  genomeName = "panTro4";  break;
       case "panpan2":  genomeName = "panPan2";  break;
       case "susscr11": genomeName = "susScr11"; break;
       case "bostau8":  genomeName = "bosTau8";  break;
       case "canfam3":  genomeName = "canFam3";  break;
       case "danrer11": genomeName = "danRer11"; break;
       case "danrer10": genomeName = "danRer10"; break;
       case "saccer3":  genomeName = "sacCer3";  break;
       };

    console.log(" actual genome name we will use: " + genomeName);

    if(genomeName == "tair10"){
      genomeOptions = tair10_options
      }
    else if(genomeName == "pfal3d7"){
      genomeOptions = pfal3D7_options;
      }
    else{
      genomeOptions =  {
          minimumBases: 5,
          flanking: 1000,
          height: 200,
          autoHeight: true,
          showRuler: true,
          genome: genomeName
          };
    }; // else: must be an igv-supported genome

   console.log("about to createBrowser, line 352");

   jsonObj = "{\"arguments\":\"track, popoverData\",\"body\":\"{console.log(track); console.log(popoverData);  console.log('track-click 4');}\"}"
   obj = JSON.parse(jsonObj)

   trackClickFunction = new Function(obj.arguments, obj.body)
   console.log("---- genomeOptions");
   console.log(genomeOptions);

   try{
      var div = document.getElementById("igvDiv");
      igv.createBrowser(div, genomeOptions)
          .then(function(browser){
              window.igvBrowser = browser;
              console.log("igv created igvBrowser in resolved promise")
              igvBrowser.on("locuschange", function(referenceFrame){
                 var chromLocString = referenceFrame.label;
                 self.chromLocString = chromLocString;
                 });
             igvBrowser.on("trackclick", trackClickFunction);
             //self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});
             }); // then
       } // try
    catch(err){
      console.log(err);
      //returnPayload = "error, failure in setCustomGenome: '" + msg.payload.genomeName + "'";
      //var return_msg = {cmd: msg.callback, status: status, callback: "", payload: returnPayload};
      //hub.send(return_msg);
      }

      //console.log("created igvBrowser in resolved promise")
      //igvBrowser.on("locuschange", function(referenceFrame){
      //   var chromLocString = referenceFrame.label;
      //   self.chromLocString = chromLocString;
      //   });
      //igvBrowser.on("trackclick", trackClickFunction);
      //}
    //catch(err){
    //  console.log(err);
    //  }

} // initializeIGV
//----------------------------------------------------------------------------------------------------
function setTrackClickFunction(msg)
{
   console.log("--- setTrackClickFunction, adding new listener");
   parts = msg.payload.jsFunction;

   window.igvBrowser.off("trackclick")   // remove all of our trackclick functions

   trackClickFunction = new Function(parts.arguments, parts.body);

   window.igvBrowser.on("trackclick", trackClickFunction)
   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});
   self.hub.send({cmd: msg.callback, status: "disabled", callback: "", payload: ""});

} // setTrackClickFunction
//----------------------------------------------------------------------------------------------------
function enableMotifLogoPopups()
{

} // enableMotifLogoPopups
//----------------------------------------------------------------------------------------------------
async function showGenomicRegion(msg)
{
   var self = this;
   checkSignature(self, "showGenomicRegion")

   var regionString = msg.payload.regionString;
   console.log("--- about to search: " + regionString)
    try{
       await(window.igvBrowser.search(regionString));
       console.log("after search request: " + regionString);
       if(msg.callback != null){
          self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: regionString});
	  }
       else{
          return(regionString);
          }
       }
    catch(err){
      console.log("search failure")
      console.log(err)
       if(msg.callback != null){
          self.hub.send({cmd: msg.callback, status: "failure", callback: "",
                         payload: "unrecognized locus '" + regionString + "'"})
          }
       else{
          return("failure")
          }
      };

} // showGenomicRegion
//----------------------------------------------------------------------------------------------------
function getGenomicRegion(msg)
{
   var self = this;
   checkSignature(self, "getGenomicRegion")
   var chromLocString = window.igvBrowser.currentLoci()[0];

   console.log("getGenomicRegion returning new currentLocus " + chromLocString);
   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: chromLocString});

} // getGenomicRegion
//----------------------------------------------------------------------------------------------------
function zoomIn(msg)
{
   var self = this;
   checkSignature(self, "zoomIn")
   window.igvBrowser.zoomIn();
   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // zoomIn
//----------------------------------------------------------------------------------------------------
function zoomOut(msg)
{
   var self = this;
   checkSignature(self, "zoomOut")
   window.igvBrowser.zoomOut();
   self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // zoomOut
//----------------------------------------------------------------------------------------------------
function showTrackLabels(msg)
{
    var newState = msg.payload.newState;

    if(newState){
       window.igvBrowser.showTrackLabels()
       }
    else{
       window.igvBrowser.hideTrackLabels()
       }

    self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // showTrackLabels
//----------------------------------------------------------------------------------------------------
function getTrackNames(msg)
{
   that = this;
   setTimeout(function(){
       var self = that;
       checkSignature(self, "getTrackNames");

       var result = [];
       var count = window.igvBrowser.trackViews.length;

       for(var i=0; i < count; i++){
          // console.log("--- looking for name of track " + i);
          var configured = window.igvBrowser.trackViews[i].track.config != null;
          // console.log("--- configured? " + configured)
          if(configured){
             var trackName = window.igvBrowser.trackViews[i].track.config.name
             if(trackName != null && trackName.length > 0){
                result.push(trackName)
                } // has length
            } // a configured (user created) track
       } // for i

       self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: result});
   }, 1000);

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
function getSVG(msg)
{
   var self = this;
   checkSignature(self, "getSVG");

   promise = window.igvBrowser.toSVG()

    promise.then(
	function(result){
          self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: result});
	  },
	function(error){
          self.hub.send({cmd: msg.callback, status: "failure", callback: "", payload: "svg error"});
          })

} // getSVG
//----------------------------------------------------------------------------------------------------
async function displayBedTrackFromUrl(msg)
{
   var self = this;
   checkSignature(self, "displayBedTrackFromUrl")

   var trackName = msg.payload.name;
    console.log(" ** displayBedTrackFromUrl, trackName: " + trackName);
   var bedFileName = msg.payload.bedFileName;
   var displayMode = msg.payload.displayMode;
   var color = msg.payload.color;
   var trackHeight = msg.payload.trackHeight;

   //var url = window.location.href + "?" + bedFileName;
   var url = msg.payload.dataURL
   console.log("=== displayBedTrackFromUrl, msg");
   console.log("   color: " + color)
   console.log(msg)

   var config = {format: "bed",
                 name: trackName,
                 url: url,
                 indexed:false,
                 displayMode: displayMode,
                 sourceType: "file",
                 color: color,
                 order: Number.MAX_VALUE,
		 height: trackHeight,
                 type: "annotation"};

   console.log(JSON.stringify(config));

    try{
       await(window.igvBrowser.loadTrack(config))
       console.log("=== after loadTrack, bed track")
       self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});
       }
    catch(error){
       console.log("=== load bed track error")
       console.log(error)
       self.hub.send({cmd: msg.callback, status: "failure", callback: "", payload: error});
       }

} // displayBedTrackFromDataFrame
//----------------------------------------------------------------------------------------------------
async function displayVcfTrackFromUrl(msg)
{
   var self = this;
   checkSignature(self, "displayVcfTrackFromUrl")

   var trackName = msg.payload.name;
   var displayMode = msg.payload.displayMode;
   var trackHeight = msg.payload.trackHeight;
   var dataURL = msg.payload.dataURL;
   var visibilityWindow = msg.payload.visibilityWindow;

   console.log("vcf dataURL: " + dataURL)

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
                 visibilityWindow: visibilityWindow,
                 homvarColor: homvarColor,
                 hetvarColor: hetvarColor,
                 homrefColor: homrefColor,
                 color: locationColor,
                 order: Number.MAX_VALUE,
                 type: "variant"};

   console.log(JSON.stringify(config));

   try{
      await(window.igvBrowser.loadTrack(config))
      console.log("=== after loadTrack, vcf track")
      setTimeout(function(){
          console.log("   about to send vcf success msg back to R");
          self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});
          }, 5000)
      }
   catch(error){
      console.log("=== load bed track error")
      console.log(error)
      self.hub.send({cmd: msg.callback, status: "failure", callback: "", payload: error});
     }

} // displayVcfTrackFromUrl
//----------------------------------------------------------------------------------------------------
async function displayBedpeInteractionsTrackFromUrl(msg)
{
   var self = this;
   checkSignature(self, "displayBedpeInteractionsTrackFromUrl")

   var trackName = msg.payload.name;
   var displayMode = msg.payload.displayMode;
   var trackHeight = msg.payload.trackHeight;
   var dataURL = msg.payload.dataURL;
   var color = msg.payload.color;
   var visibilityWindow = msg.payload.visibilityWindow;

   console.log("bedpe dataURL: " + dataURL)
   console.log("bedpe visibilityWindow: " + visibilityWindow);

   //var indexURL = msg.payload.indexURL;
   //var indexed = indexURL.length > 0;

   var config = {format: "bedpe",
                 name: trackName,
                 type: "interaction",
                 format: "bedpe",
                 url: dataURL,
                 sourceType: "file",
		 height: trackHeight,
                 visibilityWindow: visibilityWindow,
                 color: color,
                 order: Number.MAX_VALUE};

   console.log(JSON.stringify(config));

   try{
      await(window.igvBrowser.loadTrack(config))
      console.log("=== after loadTrack, bedpe track")
      setTimeout(function(){
          console.log("   about to send bedpe success msg back to R");
          self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});
          }, 5000)
      }
   catch(error){
      console.log("=== load bedpe track error")
      console.log(error)
      self.hub.send({cmd: msg.callback, status: "failure", callback: "", payload: error});
     }

} // displayBedpeInteractionsTrackFromUrl
//----------------------------------------------------------------------------------------------------
async function displayAlignmentTrackFromUrl(msg)
{
   var self = this;
   checkSignature(self, "displayAlignmentTrackFromUrl")

   var trackName = msg.payload.name;
   var trackHeight = msg.payload.trackHeight;
   var dataURL = msg.payload.dataURL;
   var visibilityWindow = msg.payload.visibilityWindow;

   console.log("==== bam visibilityWindow: " + visibilityWindow);
   console.log("==== bam height: " + trackHeight);

   console.log("dataURL: " + dataURL)

   var indexURL = msg.payload.indexURL;
   var indexed = indexURL.length > 0;
   var color = msg.payload.color;

   var config = {name: trackName,
                 type: "alignment",
                 format: "bam",
                 url: dataURL,
                 indexed: indexed,
                 indexURL: indexURL,
                 sync: true,
                 order: Number.MAX_VALUE,
                 visibilityWindow: visibilityWindow,
		 height: trackHeight,
                 color: color
                 };

   console.log(JSON.stringify(config));

   async function blockingLoad(config) {
      await window.igvBrowser.loadTrack(config);
      };

   try{
      console.log(" about to call 'await blockingLoad(config)'");
      await blockingLoad(config);
      self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});
      }
   catch(error){
      console.log("=== load bed track error")
      console.log(error)
      self.hub.send({cmd: msg.callback, status: "failure", callback: "", payload: error});
      }

} // displayAlignmentTrackFromUrl
//----------------------------------------------------------------------------------------------------
async function displayQuantitativeTrackFromUrl(msg)
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
                 order: Number.MAX_VALUE,
		 height: trackHeight,
                 autoscale: autoscale,
                 min: min,
                 max: max,
                 type: "wig"};

   console.log(JSON.stringify(config));

   try{
      await(window.igvBrowser.loadTrack(config))
      console.log("=== after loadTrack, quantitative from Url")
      self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});
      }
   catch(error){
      console.log("=== load bed track error")
      console.log(error)
      self.hub.send({cmd: msg.callback, status: "failure", callback: "", payload: error});
      }

} // displayQuantitativeTrackFromUrl
//----------------------------------------------------------------------------------------------------
async function addBedGraphTrackFromDataFrame(msg)
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
                 order: Number.MAX_VALUE,
                 height: trackHeight,
                 type: "wig"};


   try{
      await(window.igvBrowser.loadTrack(config))
      console.log("=== after loadTrack, bedGraphFromDataFrame from Url")
      self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});
      }
   catch(error){
      console.log("=== load bed track error")
      console.log(error)
      self.hub.send({cmd: msg.callback, status: "failure", callback: "", payload: error});
      }

} // addBedGraphTrackFromDataFrame
//----------------------------------------------------------------------------------------------------
// function addBedTrackFromHostedFile(msg)
// {
//    var self = this;
//    checkSignature(self, "addBedTrackFromHostedFile")
//
//    console.log("=== addBedTrackFromFile");
//
//    var trackName = msg.payload.name;
//    var displayMode = msg.payload.displayMode;
//    var color = msg.payload.color;
//    var uri       = msg.payload.uri;
//    var indexUri  = msg.payload.indexUri;
//    var indexed = true;
//
//    if(indexUri==null){
//      indexed = false;
//      }
//
//     /***********
//    var config = {format: "bed",
//                  name: trackName,
//                  url: uri,
//                  indexed: indexed,
//                  displayMode: displayMode,
//                  color: color,
//                  type: "annotation"};
//     *******/
//
//
//    if(indexed){
//      config.indexURL = indexUri;
//      }
//
//    var config = {url: uri, name: trackName, color: color};
//    console.log("---- about to loadTrack");
//    console.log(config)
//    window.igvBrowser.loadTrack(config);
//
//    self.hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});
//
// } // addBedTrackFromHostedFile
//----------------------------------------------------------------------------------------------------
  return({

    signature: "IGV 0.99.25",

    addMessageHandlers: addMessageHandlers,
    initializeUI: initializeUI,
    handleWindowResize: handleWindowResize.bind(this),
    hub: hub,
    igvBrowser: null,
    chromLocString: null,
    setGenome: setGenome,
    showGenomicRegion: showGenomicRegion
    });

}); // IGV
//----------------------------------------------------------------------------------------------------
async function test_app()
{
   console.log("--- direct (w/o R) testing igvApp.js");
   msg = {"cmd": "setGenome",
          "callback": null,
          "status": "request",
          "payload": "hg38"};

   await IGV.setGenome(msg);
   console.log("--- after setGenome in test_app");
   msg = {"cmd": "showGenomicRegion",
          "callback": null,
          "status": "request",
	  "payload": {"regionString": "BACH1"}};
   
   setTimeout(function(){IGV.showGenomicRegion(msg)}, 2000);

}  test_app
//----------------------------------------------------------------------------------------------------
hub = BrowserViz;
var IGV = IGV(hub);
IGV.addMessageHandlers()
hub.addOnDocumentReadyFunction(IGV.initializeUI.bind(IGV));
hub.start();
window.IGV = IGV;
window.hub = hub;
