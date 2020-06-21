package com.example.flutter_test_app

import androidx.annotation.NonNull
import com.facebook.FacebookActivity
import com.facebook.FacebookSdk
import com.facebook.appevents.AppEventsConstants
import com.facebook.appevents.AppEventsLogger
import io.flutter.embedding.android.FlutterActivity
import io.flutter.embedding.engine.FlutterEngine
import io.flutter.plugin.common.MethodChannel
import io.flutter.plugins.GeneratedPluginRegistrant


class MainActivity : FlutterActivity() {
    override fun configureFlutterEngine(@NonNull flutterEngine: FlutterEngine) {
        FacebookSdk.setIsDebugEnabled(true)
        val logger = AppEventsLogger.newLogger(this)
        GeneratedPluginRegistrant.registerWith(flutterEngine)
        MethodChannel(flutterEngine.dartExecutor, "com.into.the.dumpster/flutter").setMethodCallHandler { call, result ->
            if (call.method == "hellofromflutter") {
                logger.logEvent(AppEventsConstants.EVENT_NAME_ACHIEVED_LEVEL)
                val productId = call.argument<Int>("productId")
                result.success("Hello back, and you have selected product ID:" + productId)
            } else {
                result.notImplemented()
            }
        }
    }
}
