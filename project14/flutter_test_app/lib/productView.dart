import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test_app/manager/product.dart';
import 'package:http/http.dart' as http;

class ProductView extends StatelessWidget {
  final int _productID;

  ProductView(this._productID);

  @override
  Widget build(BuildContext context) {
    return FutureBuilder(
      future: Future.wait(
          [ProductManager.readProduct(this._productID), this._getCatPicture()]),
      builder: (context, snapshot) {
        if (snapshot.hasData) {
          Product seletedProduct = snapshot.data[0];
          String catPicUrl = snapshot.data[1];
          return Scaffold(
            appBar: AppBar(
              title: Text("Viewing " + seletedProduct.productName),
            ),
            body: Center(
              child: Container(
                child: Column(
                  children: <Widget>[
                    _getProductSection(seletedProduct, catPicUrl),
                    ..._getButtonSection(context, seletedProduct),
                  ],
                ),
              ),
            ),
          );
        } else {
          return Container();
        }
      },
    );
    // Product selectedProduct = ;
  }

  Widget _getProductSection(Product product, String catPicUrl) {
    //UI reference from https://flutter.dev/docs/development/ui/layout/tutorial
    return Container(
        padding: const EdgeInsets.all(32),
        child: Row(
          children: [
            Expanded(
              /*1*/
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  if (null != catPicUrl) (Image.network(catPicUrl)),
                  /*2*/
                  Container(
                    padding: const EdgeInsets.only(bottom: 8),
                    child: Text(
                      product.productName,
                      style: TextStyle(
                        fontWeight: FontWeight.bold,
                      ),
                    ),
                  ),
                  Text(
                    'Price: ' + product.productPrice.toString(),
                    style: TextStyle(
                      color: Colors.grey[500],
                    ),
                  ),
                ],
              ),
            ),
            /*3*/
            Icon(
              Icons.star,
              color: Colors.red[500],
            ),
            Text('99.9'),
          ],
        ));
  }

  List<Widget> _getButtonSection(BuildContext context, Product seletedProduct) {
    return [
      RaisedButton(
        onPressed: () async =>
            {await this._onclickAddToCart(context, seletedProduct)},
        child: Text('Add To Cart'),
      ),
      RaisedButton(
        onPressed: () async {
          await this._onclickPurchase(context, seletedProduct);
        },
        child: Text('Purchase'),
      ),
      RaisedButton(
        onPressed: () {
          Navigator.pop(context);
        },
        child: Text('Go back!'),
      ),
    ];
  }

  static const methodChannel = MethodChannel('com.into.the.dumpster/flutter');

  Future<void> _onclickAddToCart(BuildContext context, Product product) async {
    print("ATC >>> " + product.productId.toString());
    try {
      final String responseMessage = await methodChannel.invokeMethod<String>(
          'hellofromflutter', {'productId': product.productId});
      print(responseMessage);
      await methodChannel.invokeMethod<String>('fbaddtocart', {
        'productId': product.productId,
        'productPrice': product.productPrice.toDouble()
      });
    } on Exception catch (e) {
      print("Exception:" + e.toString());
    } catch (e) {
      print('Error:' + e.toString());
    }
  }

  Future<void> _onclickPurchase(BuildContext context, Product product) async {
    print("Purchasing >>> " + product.productId.toString());
    try {
      await methodChannel.invokeMethod<String>('fbpurchase', {
        'productId': product.productId,
        'productPrice': product.productPrice.toDouble()
      });
    } on Exception catch (e) {
      print("Exception:" + e.toString());
    } catch (e) {
      print('Error:' + e.toString());
    }
  }

  Future<String> _getCatPicture() async {
    final response =
        await http.get('https://api.thecatapi.com/v1/images/search');
    var parseCat = json.decode(response.body);
    if (response.statusCode == 200) {
      return parseCat[0]['url'];
    } else {
      return null;
    }
  }
}
