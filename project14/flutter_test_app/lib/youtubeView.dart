import 'package:flutter/material.dart';
import 'package:flutter_test_app/youtubePlayerView.dart';

class YoutubeView extends StatefulWidget {
  @override
  _YoutubeViewState createState() => _YoutubeViewState();
}

class _YoutubeViewState extends State<YoutubeView> {
  var _starScore = 0;

  void _minusStar() {
    this.setState(() {
      _starScore = _starScore - 1;
    });
  }

  @override
  Widget build(BuildContext context) {
    var youtubeStarContainer = Container(
      padding: const EdgeInsets.all(32),
      child: Column(
        children: [
          Container(child: YoutubePlayerView(videoId: 'drnBMAEA3AM')),
          Container(
            child: Row(
              children: <Widget>[
                Icon(
                  Icons.star,
                  color: Colors.red[500],
                ),
                Text(_starScore.toString()),
              ],
            ),
          ),
          RaisedButton(
            onPressed: () => {
              this.setState(() {
                _starScore = _starScore + 1;
              })
            },
            child: Text('Add Stars!'),
          ),
          RaisedButton(
            onPressed: (_starScore > 0) ? () => this._minusStar() : null,
            child: Text('Minus Stars!'),
          ),
        ],
      ),
    );

    return Scaffold(
      appBar: AppBar(
        title: Text("The hill are alive!"),
      ),
      body: Center(
        child: youtubeStarContainer,
      ),
    );
  }
}
