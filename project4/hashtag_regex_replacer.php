<?php
$input = <<<'EOT'
<h1>Hello World</h1>
<h2>Regex Replacement</h2>
<p>#START_metric</p>
<p>1</p>
<p>22</p>
<p>333</p>
<p>#END_metric</p>
<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>
<p>#START_metric</p>
<p>444</p>
<p>55</p>
<p>6</p>
<p>#END_metric</p>
<p>Sed lacus purus, auctor at placerat vel, mattis vel odio.</p>
<p>#START_duplicate</p>
<p>9</p>
<p>#END_duplicate</p>
EOT;

/**
* HASH TAG CLEANUP
**/
function hashtag_clean($input){
  $n = preg_match_all('/<p>#(.*)<\/p>/', $input, $matches);
  $output = $input;
  for( $i = 0; $i<$n; $i++ ) {
    $pattern = $matches[0][$i];
    $replace = "#".$matches[1][$i];
    $output = str_replace($pattern, $replace, $output);
  }
  return $output;
}

/**
*** HASH TAG REPLACER
**/
// function metricReplacer($input){
//   $n = preg_match_all('/#START_metric(.*?)#END_metric/s', $input, $matches);
//   $output = $input;
//   for( $i = 0; $i<$n; $i++ ) {
//     $pattern = $matches[0][$i];
//     $replace = "<table border='1'><tr>";
//     preg_match_all('/<p>(.*)<\/p>/', $matches[1][$i], $matchvalues);
//     foreach($matchvalues[1] as $matchvalue){
//       $replace .= "<td>".$matchvalue."</td>";
//     }
//     $replace .= "</tr></table>";
//     $output = str_replace($pattern, $replace, $output);
//   }
//   return $output;
// }
//
// function duplicateReplacer($input){
//   $n = preg_match_all('/#START_duplicate(.*?)#END_duplicate/s', $input, $matches);
//   $output = $input;
//   for( $i = 0; $i<$n; $i++ ) {
//     $pattern = $matches[0][$i];
//     $replace = "<i>";
//     preg_match_all('/<p>(.*)<\/p>/', $matches[1][$i], $matchvalues);
//     foreach($matchvalues[1] as $matchvalue){
//       $replace .= str_repeat($matchvalue,$matchvalue);
//     }
//     $replace .= "</i>";
//     $output = str_replace($pattern, $replace, $output);
//   }
//   return $output;
// }

function hashtag_replacer($input, $hash, $fn, $open_tag="", $close_tag=""){
  $n = preg_match_all('/#START_'.$hash.'(.*?)#END_'.$hash.'/s', $input, $matches);
  $output = $input;
  for( $i = 0; $i<$n; $i++ ) {
    $pattern = $matches[0][$i];
    $replace = $open_tag;
    preg_match_all('/<p>(.*)<\/p>/', $matches[1][$i], $matchvalues);
    foreach($matchvalues[1] as $matchvalue){
      $replace .= $fn($matchvalue);
    }
    $replace .= $close_tag;
    $output = str_replace($pattern, $replace, $output);
  }
  return $output;
}

/**
*** OPERATIONS
**/
$output = hashtag_clean($input);

// Loop hash tag replacment
$availableHashs = ["metric","duplicate"];
foreach($availableHashs as $hash){
  // $output = ($hash."Replacer")($output);
  $fn = null;
  $open_tag = "";
  $close_tag = "";
  switch ($hash){
    case "metric" :
      $fn = function($x){return "<td>".$x."</td>";};
      $open_tag = "<table border='1' style='border-color: #0000ff;'><tr>";
      $close_tag = "</tr></table>";
      break;
    case "duplicate":
      $fn = function($x){return str_repeat($x,$x);};
      $open_tag = "<i>";
      $close_tag = "</i>";
      break;
  }
  $output = hashtag_replacer($output, $hash, $fn, $open_tag, $close_tag);
}

/**
*** OUTPUT
**/
echo "<textarea rows='20' cols='200'>";
echo $input;
echo "</textarea>";
echo "<hr>";
echo "<textarea rows='20' cols='200'>";
echo $output;
echo "</textarea>";
echo "<hr>";
echo $output;
?>
