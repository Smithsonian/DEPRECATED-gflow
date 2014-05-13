<?php

// Generic Workflow Server Usage
// file: GFLOW.inc
// author: Gert Schmeltz Pedersen gertsp45@gmail.com

class GFLOW {

  function GFLOW() {
  }

  function gflowGetFile($file='') {
		global $user;
	 	$datestamp = date('Y-m-d H:i:s');
 		$logdatestamp = date('Y-m-d');
		$logfp = fopen('/tmp/sianisdemo-' . $logdatestamp . '.log', 'a');
		fwrite($logfp, "\n\n[$datestamp] $user->name / gflowGetFile / $file");
	$filePath = drupal_get_path('module', 'gflow') . '/' . $file;
		fwrite($logfp, "\n\n[$datestamp] $user->name / gflowGetFile / $filePath");
	$fp = fopen($filePath, 'r') or exit("Unable to open file!");
	while(!feof($fp)) {
		$line = fgets($fp);
		print $line;
			fwrite($logfp, $line);
	}
		fclose($logfp);
	fclose($fp);
	module_invoke_all('exit');
	exit();
  }

  function gflowRun($workflowName='') {
		global $user;
	 	$datestamp = date('Y-m-d H:i:s');
 		$logdatestamp = date('Y-m-d');
		$logfp = fopen('/tmp/gflow-' . $logdatestamp . '.log', 'a');
		fwrite($logfp, "\n\n[$datestamp] $user->name / $workflowName");
    $workflowUrl = variable_get('gflow_block_workflow_server', '');
    $userpass = variable_get('gflow_block_workflow_userpass', '');
    $mimetype = variable_get('gflow_block_workflow_mimetype', '');
	$command = 'curl -u ' . $userpass . ' -v --data-binary @' . drupal_get_path('module', 'gflow') . '/' . $workflowName . ' -H Content-Type:' . $mimetype . ' ' . $workflowUrl . ' 2>&1';
	$curlResult = shell_exec($command);
	$loc = substr($curlResult,strpos($curlResult,"Location:"));
	$i = strpos($loc,"/runs/")+6;
	$j = strpos($loc,"\n",$i)-1-$i;
	$UUID = substr($loc,$i,$j);
	 	$datestamp = date('Y-m-d H:i:s');
	if (strlen(trim($UUID)) != 36) {
			fwrite($logfp, "\n[$datestamp] Error, a UUID was not found in: \n$curlResult");
			fclose($logfp);
		return '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"><div id="gflowResult">' . 'GFLOW ERROR initializing the workflow: <br/>' . $curlResult . '</div>';
	}
		$uuidfp = fopen('/tmp/gflow-' . $UUID . '.log', 'a');
		fwrite($uuidfp, "\n[$datestamp] gflowRun $user->name / $workflowName");
		fwrite($uuidfp, "\n[$datestamp] command: $command");
		fwrite($uuidfp, "\n[$datestamp] curlResult: $curlResult");
		fwrite($uuidfp, "\n[$datestamp] UUID: $UUID");
	$fedoraUrl = variable_get('gflow_block_fedora', '');
	$command = 'curl -u ' . $userpass . ' -s -S -H Content-Type:application/xml -X PUT --data-binary "<t2sr:runInput xmlns:t2sr=\"http://ns.taverna.org.uk/2010/xml/server/rest/\"><t2sr:value>' . $fedoraUrl . '</t2sr:value></t2sr:runInput>" ' . $workflowUrl . '/' . $UUID . '/input/input/' . 'fedora_url' . ' 2>&1';
		fwrite($uuidfp, "\n[$datestamp] command: $command");
	$curlResult = shell_exec($command);
	 	$datestamp = date('Y-m-d H:i:s');
		fwrite($uuidfp, "\n[$datestamp] curlResult: $curlResult");
		fclose($uuidfp);
		fclose($logfp);
	print '
	<div id="gflowResult">' . $UUID . '</div>
	
	';
	module_invoke_all('exit');
	exit();
  }

  function gflowSetInput($UUID='', $inputName='', $inputValue='') {
		global $user;
		$uuidfp = fopen('/tmp/gflow-' . $UUID . '.log', 'a');
	 	$datestamp = date('Y-m-d H:i:s');
		fwrite($uuidfp, "\n[$datestamp] gflowSetInput: $UUID $inputName=$inputValue");
 		$logdatestamp = date('Y-m-d');
		$logfp = fopen('/tmp/gflow-' . $logdatestamp . '.log', 'a');
		fwrite($logfp, "\n\n[$datestamp] $user->name / $UUID / $inputName / $inputValue");
    $workflowUrl = variable_get('gflow_block_workflow_server', '');
    $userpass = variable_get('gflow_block_workflow_userpass', '');
	$command = 'curl -u ' . $userpass . ' -s -S -H Content-Type:application/xml -X PUT --data-binary "<t2sr:runInput xmlns:t2sr=\"http://ns.taverna.org.uk/2010/xml/server/rest/\"><t2sr:value>' . $inputValue . '</t2sr:value></t2sr:runInput>" ' . $workflowUrl . '/' . $UUID . '/input/input/' . $inputName . ' 2>&1';
		fwrite($uuidfp, "\n[$datestamp] command: $command");
	$curlResult = shell_exec($command);
	 	$datestamp = date('Y-m-d H:i:s');
		fwrite($uuidfp, "\n[$datestamp] curlResult: $curlResult");
		fclose($uuidfp);
		fclose($logfp);
	print '
	<div id="gflowResult">
	';
	print $inputName. '=' . $inputValue;
	print '
	</div>
	
	';
	module_invoke_all('exit');
	exit();
  }

  function gflowSetStatus($UUID='', $newStatus='') {
		global $user;
		$uuidfp = fopen('/tmp/gflow-' . $UUID . '.log', 'a');
	 	$datestamp = date('Y-m-d H:i:s');
		fwrite($uuidfp, "\n[$datestamp] gflowSetStatus: $UUID $newStatus");
 		$logdatestamp = date('Y-m-d');
		$logfp = fopen('/tmp/gflow-' . $logdatestamp . '.log', 'a');
		fwrite($logfp, "\n\n[$datestamp] $user->name / gflowSetStatus: $UUID $newStatus");
    $workflowUrl = variable_get('gflow_block_workflow_server', '');
    $userpass = variable_get('gflow_block_workflow_userpass', '');
	$command = 'curl -u ' . $userpass . ' -s -S -H Content-Type:text/plain -X PUT -d ' . $newStatus . ' ' . $workflowUrl . '/' . $UUID . '/status 2>&1';
		fwrite($uuidfp, "\n[$datestamp] command: $command");
	$curlResult = shell_exec($command);
	 	$datestamp = date('Y-m-d H:i:s');
		fwrite($uuidfp, "\n[$datestamp] curlResult: $curlResult");
		fclose($uuidfp);
		fclose($logfp);
	print '
	<div id="gflowResult">' . $curlResult . '</div>
	
	';
	module_invoke_all('exit');
	exit();
  }

  function gflowWaitFinish($UUID='') {
		$uuidfp = fopen('/tmp/gflow-' . $UUID . '.log', 'a');
	 	$datestamp = date('Y-m-d H:i:s');
		fwrite($uuidfp, "\n[$datestamp] gflowWaitFinish: $UUID");
    $workflowUrl = variable_get('gflow_block_workflow_server', '');
    $userpass = variable_get('gflow_block_workflow_userpass', '');
	$command = 'curl -u ' . $userpass . ' -s -S ' . $workflowUrl . '/' . $UUID . '/status 2>&1';
		fwrite($uuidfp, "\n[$datestamp] command: $command");
	$curlResult = shell_exec($command);
	 	$datestamp = date('Y-m-d H:i:s');
		fwrite($uuidfp, "\n[$datestamp] curlResult: $curlResult");
		fclose($uuidfp);
	print '
	<div id="gflowResult">' . $curlResult . '</div>
	
	';
	module_invoke_all('exit');
	exit();
  }

  function gflowGetResult($UUID='') {
		global $user;
		$uuidfp = fopen('/tmp/gflow-' . $UUID . '.log', 'a');
	 	$datestamp = date('Y-m-d H:i:s');
		fwrite($uuidfp, "\n[$datestamp] gflowGetResult: $UUID");
 		$logdatestamp = date('Y-m-d');
		$logfp = fopen('/tmp/gflow-' . $logdatestamp . '.log', 'a');
    $workflowUrl = variable_get('gflow_block_workflow_server', '');
    $userpass = variable_get('gflow_block_workflow_userpass', '');
	$resultUrl = $workflowUrl . '/' . $UUID . '/wd/out/RESULT/1';
	$command = 'curl -u ' . $userpass . ' -s -S ' . $resultUrl . ' 2>&1';
		fwrite($uuidfp, "\n[$datestamp] command: $command");
	$curlResult = shell_exec($command);
	 	$datestamp = date('Y-m-d H:i:s');
		fwrite($uuidfp, "\n[$datestamp] curlResult: $curlResult");
	if (!strstr($curlResult, 'no such directory entry')) {
			fwrite($logfp, "\n[$datestamp] $user->name resultUrl: $resultUrl");
		$curlResult = $resultUrl;
	} else {
		$errorUrl = $resultUrl . '.error';
		$command = 'curl -u ' . $userpass . ' -s -S ' . $errorUrl . ' 2>&1';
			fwrite($uuidfp, "\n[$datestamp] command: $command");
		$curlResult = shell_exec($command);
	 		$datestamp = date('Y-m-d H:i:s');
			fwrite($uuidfp, "\n[$datestamp] curlResult: $curlResult");
			fwrite($logfp, "\n[$datestamp] $user->name errorUrl: $errorUrl");
		$curlResult = $errorUrl;
	}
		fclose($uuidfp);
		fclose($logfp);

	print '
	<div id="gflowResult">' . $curlResult . '</div>
	
	';
	module_invoke_all('exit');
	exit();
  } 
}

?>
