import 'dart:async';
import 'package:csv/csv.dart';
import 'package:flutter/services.dart';

class ProductManager {
  static List<Product> _productListing;

  static Future<void> _getData() async {
    try {
      final String content =
          await rootBundle.loadString('assets/data/product.csv');
      List<dynamic> rowsAsListOfValues =
          const CsvToListConverter().convert(content, eol: "\n");
      var products = <Product>[];
      rowsAsListOfValues.forEach((element) {
        products.add(new Product(element[0], element[1], element[2]));
      });
      ProductManager._productListing = products;
    } catch (e) {
      return;
    }
  }

  static Future<List<Product>> listProducts() async {
    //TODOs: How to create best practice Singleton in flutter
    if (null == ProductManager._productListing) {
      await _getData();
    }
    return ProductManager._productListing;
  }

  static Future<Product> readProduct(int productId) async {
    return (await listProducts()).toList().firstWhere(
        (element) => element.productId == productId,
        orElse: () => null);
  }
}

class Product {
  int productId;
  String productName;
  int productPrice;
  Product(int productId, String productName, int productPrice) {
    this.productId = productId;
    this.productName = productName;
    this.productPrice = productPrice;
  }
}
