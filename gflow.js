// Generic Workflow Server Usage
// file: gflow.js
// author: Gert Schmeltz Pedersen - gertsp45@gmail.com
/***************************************************************************************************/
		
function gflowOnload() {
	workflowName = ''; 
	speciesName = ' '; 
	obstablePids = '';
    $('#gflowStatusArea').html(gflowGetDateTime()+' Welcome!');
}

function gflowClear() {
	gflowClearMarkers();
	currentBBox.setMap(null);
	document.getElementById("gflowNorth").value = '';
	document.getElementById("gflowEast").value = '';
	document.getElementById("gflowSouth").value = '';
	document.getElementById("gflowWest").value = '';
	document.getElementById("gflowClearButton").disabled = "true";
	document.getElementById("gflowFindButton").disabled = "true";
	drawingManager.setDrawingMode(google.maps.drawing.OverlayType.RECTANGLE);
	currentBBox = new google.maps.Rectangle(rectangleOptions);
	currentBBox.setMap(gflowMap);
	$('#gflowBottomArea').html('&#160;');
	$('#gflowStatusArea').html(gflowGetDateTime()+' clear');
	gflowSetRunWorkflowButton(document.getElementById("gflowWorkflow"));
}

function gflowFind() {
	var north = document.getElementById("gflowNorth").value;
	var east = document.getElementById("gflowEast").value;
	var south = document.getElementById("gflowSouth").value;
	var west = document.getElementById("gflowWest").value;
	var beginDate = document.getElementById("gflowBegin").value.replace(/\//g, '');
	var endDate = document.getElementById("gflowEnd").value.replace(/\//g, '');
    $('#gflowStatusArea').html(gflowGetDateTime()+' Finding observation tables ...');
	var url = '/gflow/find/'+south+'/'+west+'/'+north+'/'+east+'/'+beginDate+'/'+endDate;
	$('#gflowBottomArea').load(
			encodeURI(url)+' #gflowFindResult',
			function(response, status, xhr) {
				if (status == 'error') {
				    $('#gflowStatusArea').html(gflowGetDateTime()+' Error: '+xhr.status+' '+xhr.statusText+gflowGetPreviousStatus());
				} else {
				    $('#gflowStatusArea').html(gflowGetDateTime()+' Found '+(document.getElementById("gflowFindCount")).textContent+' observation tables, see below'+gflowGetPreviousStatus());
					gflowSetMarkers();
				}
			}
	);
}

function gflowWorkflowSelected(selectWorkflowElement) {
	var i = selectWorkflowElement.selectedIndex;
	if (i == 0) {
		document.getElementById("gflowSelectObservationTables").style.display = "";
		document.getElementById("gflowSelectSpecies").style.display = "none";
		document.getElementById("gflowSpecies").selectedIndex = 0;
		speciesName = ' '; 
	}
	if (i == 1) {
		document.getElementById("gflowSelectObservationTables").style.display = "";
		document.getElementById("gflowSelectSpecies").style.display = "";
	}
	if (i>-1) {
		var selectedOptionElement = selectWorkflowElement.options[i];
		workflowName = selectedOptionElement.value;
	}
	gflowSetRunWorkflowButton(selectWorkflowElement);
}

function gflowSetRunWorkflowButton(selectWorkflowElement) {
	gflowGetObstables();
	if (selectWorkflowElement.selectedIndex == 0) {
		if (obstablePids=="") {
			document.getElementById("gflowRunWorkflowButton").disabled = "true";
		} else {
			document.getElementById("gflowRunWorkflowButton").disabled = "";
		}
	}
	if (selectWorkflowElement.selectedIndex == 1) {
		if (obstablePids=="" || speciesName==" ") {
			document.getElementById("gflowRunWorkflowButton").disabled = "true";
		} else {
			document.getElementById("gflowRunWorkflowButton").disabled = "";
		}
	}
}

function gflowSpeciesSelected(selectSpeciesElement) {
	var i = selectSpeciesElement.selectedIndex;
	if (i>-1) {
		var selectedOptionElement = selectSpeciesElement.options[i];
		speciesName = selectedOptionElement.value;
	}
	gflowSetRunWorkflowButton(document.getElementById("gflowWorkflow"));
}

function gflowGetObstables() {
	obstablePids = document.getElementById("gflowObstables").value;
}

function gflowGetDateTime() {
	return (new Date()).toLocaleString();
}

function gflowGetPreviousStatus() {
	return '<br/>'+$('#gflowStatusArea').html();
}

function gflowRun() {
	var workflowSelect = document.getElementById("gflowWorkflow");
	var workflowSelectedOption = workflowSelect.options[workflowSelect.selectedIndex];
	var workflowInit = gflowGetDateTime()+' Initializing Workflow: '+workflowSelectedOption.text+' ('+workflowSelectedOption.value+')';
	gflowGetObstables();
    $('#gflowStatusArea').html(workflowInit);
	document.getElementById("gflowStatusDiv").style.display = "";
	UUID = "";
	var url = '/gflow/gflowRun/'+workflowName;
	$('#gflowAjaxArea').load(
			encodeURI(url)+' #gflowResult',
			function(response, status, xhr) {
				if (status == 'error') {
				    $('#gflowStatusArea').html(gflowGetDateTime()+' Error: '+xhr.status+' '+xhr.statusText+gflowGetPreviousStatus());
				} else {
					if ($('#gflowAjaxArea').text().indexOf('GFLOW ERROR')>-1) {
					    $('#gflowStatusArea').html(gflowGetDateTime()+' '+$('#gflowAjaxArea').text()+gflowGetPreviousStatus());
					} else {
						UUID = $('#gflowAjaxArea').text();
					    $('#gflowStatusArea').html(gflowGetDateTime()+' Initialized : UUID='+UUID+gflowGetPreviousStatus());
					    inputs = new Array('pids_of_observation_tables', obstablePids);
					    if (speciesName != " ") {
					    	inputs.push('species_name');
					    	inputs.push(speciesName);
					    }
					    gflowSetInputs(UUID, inputs);
					}
				}
			}
	);
}

function gflowSetInputs(UUID, inputs) {
	var url = '/gflow/gflowSetInput/'+UUID+'/'+inputs.shift()+'/'+inputs.shift();
	$('#gflowAjaxArea').load(
			encodeURI(url)+' #gflowResult',
			function(response, status, xhr) {
				if (status == 'error') {
				    $('#gflowStatusArea').html(gflowGetDateTime()+' Error: '+xhr.status+' '+xhr.statusText+gflowGetPreviousStatus());
				} else {
					tavernaStatus = $('#gflowAjaxArea').text();
				    $('#gflowStatusArea').html(gflowGetDateTime()+' '+tavernaStatus+gflowGetPreviousStatus());
				    if (inputs.length > 0) {
					    gflowSetInputs(UUID, inputs);
				    } else {
				    	gflowSetStatus(UUID, 'Operating');
				    }
				}
			}
	);
}

function gflowSetStatus(UUID, newStatus) {
	var url = '/gflow/gflowSetStatus/'+UUID+'/'+newStatus;
	$('#gflowAjaxArea').load(
			encodeURI(url)+' #gflowResult',
			function(response, status, xhr) {
				if (status == 'error') {
				    $('#gflowStatusArea').html(gflowGetDateTime()+' Error: '+xhr.status+' '+xhr.statusText+gflowGetPreviousStatus());
				} else {
					tavernaStatus = $('#gflowAjaxArea').text();
				    $('#gflowStatusArea').html(gflowGetDateTime()+' '+tavernaStatus+gflowGetPreviousStatus());
				    if (tavernaStatus == 'Operating') {
				    	window.setTimeout("gflowWaitFinish(UUID)",3000);
				    } else {
					    if (tavernaStatus == 'Finished') {
					    	gflowWaitFinish(UUID);
					    } else {
						    $('#gflowStatusArea').html(gflowGetDateTime()+' Error:<BR/>'+$('#gflowAjaxArea').text()+gflowGetPreviousStatus());
					    }
				    }
				}
			}
	);
}

function gflowWaitFinish(UUID) {
	var url = '/gflow/gflowWaitFinish/'+UUID;
	$('#gflowAjaxArea').load(
			encodeURI(url)+' #gflowResult',
			function(response, status, xhr) {
				if (status == 'error') {
				    $('#gflowStatusArea').html(gflowGetDateTime()+' Error: '+xhr.status+' '+xhr.statusText+gflowGetPreviousStatus());
				} else {
					tavernaStatus = $('#gflowAjaxArea').text();
				    $('#gflowStatusArea').html(gflowGetDateTime()+' '+tavernaStatus+gflowGetPreviousStatus());
				    if (tavernaStatus == 'Operating') {
				    	window.setTimeout("gflowWaitFinish(UUID)",3000);
				    } else {
					    if (tavernaStatus == 'Finished') {
					    	gflowGetResult(UUID);
					    } else {
						    $('#gflowStatusArea').html(gflowGetDateTime()+' Error:<BR/>'+$('#gflowAjaxArea').text()+gflowGetPreviousStatus());
					    }
				    }
				}
			}
	);
}

function gflowGetResult(UUID) {
	var url = '/gflow/gflowGetResult/'+UUID;
	$('#gflowAjaxArea').load(
			encodeURI(url)+' #gflowResult',
			function(response, status, xhr) {
				if (status == 'error') {
				    $('#gflowStatusArea').html(gflowGetDateTime()+' Error: '+xhr.status+' '+xhr.statusText+gflowGetPreviousStatus());
				} else {
					if ($('#gflowAjaxArea').text().indexOf('GFLOW ERROR')>-1) {
					    $('#gflowStatusArea').html(gflowGetDateTime()+' '+$('#gflowAjaxArea').text()+gflowGetPreviousStatus());
					} else {
					    $('#gflowStatusArea').html('<a id="gflowResultButton" target="'+UUID+'" href="'+$('#gflowAjaxArea').text()+'/'+UUID+'/wd/out/RESULT/1">Retrieve the Result</a><br/>'+gflowGetPreviousStatus());
					}
				}
			}
	);
}
