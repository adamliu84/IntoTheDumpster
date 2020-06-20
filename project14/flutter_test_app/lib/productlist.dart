import 'package:flutter/cupertino.dart';
import 'package:flutter/material.dart';
import 'package:flutter_test_app/manager/product.dart';

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
                  onTap: () async => {await this._onclick(e.productId)},
                )));
            return ListView(
              children: productListTiles.toList(),
            );
          } else {
            return Center(child: CircularProgressIndicator());
          }
        });
  }

  Future<void> _onclick(int productId) async {
    Product selectedProd = await ProductManager.readProduct(productId);
    if (null != selectedProd) {
      print(selectedProd.productName);
    }
  }
}
