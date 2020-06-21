package com.example.flutter_test_app

import android.os.Bundle
import androidx.annotation.NonNull
import com.facebook.FacebookSdk
import com.facebook.appevents.AppEventsConstants
import com.facebook.appevents.AppEventsLogger
import io.flutter.embedding.android.FlutterActivity
import io.flutter.embedding.engine.FlutterEngine
import io.flutter.plugin.common.MethodChannel
import io.flutter.plugins.GeneratedPluginRegistrant
import java.math.BigDecimal
import java.util.*


class MainActivity : FlutterActivity() {
    override fun configureFlutterEngine(@NonNull flutterEngine: FlutterEngine) {
        FacebookSdk.setIsDebugEnabled(true)
        val logger = AppEventsLogger.newLogger(this)
        GeneratedPluginRegistrant.registerWith(flutterEngine)
        MethodChannel(flutterEngine.dartExecutor, "com.into.the.dumpster/flutter").setMethodCallHandler { call, result ->
            if (call.method == "hellofromflutter") {
                val productId = call.argument<Int>("productId")
                result.success("Hello back, and you have selected product ID:" + productId)
            }
            else if (call.method == "fbpurchase"){
                val productId = call.argument<Int>("productId")
                val productPrice = call.argument<Double>("productPrice")
                if(null != productId && null != productPrice){
                    val params = Bundle()
                    params.putString(AppEventsConstants.EVENT_PARAM_CONTENT_TYPE, "product")
                    params.putString(AppEventsConstants.EVENT_PARAM_CONTENT, "[{\"id\": \"$productId\", \"quantity\": 1}]")
                    logger.logPurchase(BigDecimal.valueOf(productPrice), Currency.getInstance("USD"), params)
                }
            }
            else if(call.method == "fbaddtocart"){
                val productId = call.argument<Int>("productId")
                val productPrice = call.argument<Double>("productPrice")
                if(null != productId && null != productPrice){
                    val params = Bundle()
                    params.putString(AppEventsConstants.EVENT_PARAM_CONTENT_TYPE, "product")
                    params.putString(AppEventsConstants.EVENT_PARAM_CONTENT, "[{\"id\": \"$productId\", \"quantity\": 1}]")
                    logger.logEvent(AppEventsConstants.EVENT_NAME_ADDED_TO_CART, params)
                }
            }
            else {
                result.notImplemented()
            }
        }
    }
}
