<?php

//https://developers.facebook.com/docs/marketing-api/reference/ad-account/delivery_estimate/

function getAccessToken(){
  return "<ACCESS_TOKEN>";
}

function getAdAccountID(){
  return "act_<ID>";
}

function getTargetingSpec($json){
  return urlencode($json);
}

function getOptimizationGoal(){
  return "APP_INSTALLS";
}

function getEstimatedMau($json){
  $url = 'https://graph.facebook.com/v4.0/'.getAdAccountID().'/delivery_estimate/?access_token='.getAccessToken()."&targeting_spec=".getTargetingSpec($json)
          ."&optimization_goal=".getOptimizationGoal();
  $ch = curl_init($url);
  curl_setopt($ch, CURLOPT_HTTPGET, 1);
  curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
  $result = curl_exec($ch);
  $result_array = json_decode($result, true);
  return $result_array['data'][0]["estimate_mau"];
}

function printEstimatedMau($input){
  $est_mau = getEstimatedMau($input[2]);
  print($input[1] ."->". $est_mau."\n");
}

function readCSV(){
    $csvFile = "temp.csv";
    $file_handle = fopen($csvFile, 'r');
    while (!feof($file_handle) ) {
        $line_of_text[] = fgetcsv($file_handle, 1024);
    }
    fclose($file_handle);
    return $line_of_text;
}

$csv = readCSV();
array_pop($csv);
array_map(printEstimatedMau, $csv);
