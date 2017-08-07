<?php
http_response_code(200);
header("Status: 200");

$verify_token = "verify_token_12345";

function getAccessToken($pageid){
  switch ($pageid){
    case 12345: //Page_1_page_id
      return "page_1_access_token";
      break;
    case 67890: //Page_2_page_id
      return "page_2_access_token";
      break;
  }
}

function getUserLocale($id, $page_id){
    $url = "https://graph.facebook.com/v2.6/".$id."?fields=locale&access_token=".getAccessToken($page_id);
    $ch = curl_init();
    curl_setopt($ch, CURLOPT_URL, $url);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, TRUE);
    curl_setopt($ch, CURLOPT_HEADER, 0);
    curl_setopt($ch, CURLOPT_FOLLOWLOCATION, 0);
    $server_output = curl_exec($ch);
    curl_close($ch);
    $arr_server = json_decode($server_output);
    return $arr_server->locale;
}

function getDifferentLanguageGreeting($locale){
  switch ($locale){
    case "zh_CN":
    case "zh_TW":
    case "zh_HK":
      return "你好";
      break;
    case "ja_JP":
      return "こんにちは";
      break;
    case "ko_KR":
      return "안녕하세요";
      break;
    case "th_TH":
      return "สวัสดี";
      break;
    case "vi_VN":
      return "xin chào";
      break;
    case "ru_RU":
      return "Здравствуйте";
      break;
    default:
      return "Hello";
      break;
  }
}

// Main
if (!empty($_REQUEST['hub_mode']) && $_REQUEST['hub_mode'] == 'subscribe' && $_REQUEST['hub_verify_token'] == $verify_token) {
    // Webhook setup request
    echo $_REQUEST['hub_challenge'];
} else {
    //Parsing information
    $input = json_decode(file_get_contents('php://input'), true);

    // Skipping delivery messages
    if (!empty($input['entry'][0]['messaging'][0]['delivery'])) {
        exit;
    }
    // skip the echo of my own messages
    if (($input['entry'][0]['messaging'][0]['message']['is_echo'] == "true")) {
        exit;
    }

    //Getting message information
    $sender = $input['entry'][0]['messaging'][0]['sender']['id'];
    $message = $input['entry'][0]['messaging'][0]['message']['text'];
    $page_id = $input['entry'][0]['messaging'][0]['recipient']['id'];

    //Getting user locale
    $userLocale = getUserLocale($sender, $page_id);

    //Posting of replied message
    $reply_message = getDifferentLanguageGreeting($userLocale)." \u000AYour Locale:".$userLocale."\u000AYour Message:".$message."";
    $jsonData = '{
        "recipient":{
            "id":"' . $sender . '"
        },
        "message":{
            "text":"'.$reply_message.'"
        }
    }';
    $url = 'https://graph.facebook.com/v2.10/me/messages?access_token='.getAccessToken($page_id);
    $ch = curl_init($url);
    curl_setopt($ch, CURLOPT_POST, 1);
    curl_setopt($ch, CURLOPT_POSTFIELDS, $jsonData);
    curl_setopt($ch, CURLOPT_HTTPHEADER, array('Content-Type: application/json'));
    if (!empty($message)) {
        $result = curl_exec($ch); // user will get the message
    }
}
?>
