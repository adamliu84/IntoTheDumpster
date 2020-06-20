package com.example.flutter_test_app

import androidx.annotation.NonNull
import io.flutter.embedding.android.FlutterActivity
import io.flutter.embedding.engine.FlutterEngine
import io.flutter.plugin.common.MethodChannel
import io.flutter.plugins.GeneratedPluginRegistrant
import java.util.*

class MainActivity : FlutterActivity() {
    override fun configureFlutterEngine(@NonNull flutterEngine: FlutterEngine) {
        GeneratedPluginRegistrant.registerWith(flutterEngine)
        MethodChannel(flutterEngine.dartExecutor, "com.into.the.dumpster/flutter").setMethodCallHandler { call, result ->
            if (call.method == "hellofromflutter") {
                val productId = call.argument<Int>("productId")
                result.success("Hello back, and you have selected product ID:" + productId)
            } else {
                result.notImplemented()
            }
        }
    }
}
