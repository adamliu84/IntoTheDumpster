import 'package:flutter/cupertino.dart';
import 'package:flutter/material.dart';
import 'package:flutter_test_app/ProductView.dart';
import 'package:flutter_test_app/manager/product.dart';
import 'package:flutter_test_app/websocketChatView.dart';
import 'package:flutter_test_app/youtubeView.dart';

class ProductList extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return FutureBuilder(
        future: ProductManager.listProducts(),
        builder: (context, snapshot) {
          if (snapshot.hasData) {
            List<Product> products = snapshot.data;
            var productListTiles = products.map((e) => (ListTile(
                  title: Text(e.productName),
                  onTap: () async =>
                      {await this._onclick(context, e.productId)},
                )));
            return Column(
              children: <Widget>[
                Expanded(
                  child: ListView(
                    scrollDirection: Axis.vertical,
                    shrinkWrap: true,
                    children: productListTiles.toList(),
                  ),
                ),
              ],
            );
          } else {
            return Center(child: CircularProgressIndicator());
          }
        });
  }

  Future<void> _onclick(BuildContext context, int productId) async {
    switch (productId) {
      case 1:
        Navigator.push(
          context,
          MaterialPageRoute(builder: (context) => YoutubeView()),
        );
        break;
      case 2:
        Navigator.push(
          context,
          MaterialPageRoute(builder: (context) => WebsocketChatView()),
        );
        break;
      default:
        Product selectedProd = await ProductManager.readProduct(productId);
        if (null != selectedProd) {
          Navigator.push(
            context,
            MaterialPageRoute(builder: (context) => ProductView(productId)),
          );
        }
        break;
    }
  }
}
