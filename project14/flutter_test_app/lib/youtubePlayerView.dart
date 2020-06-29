import 'package:flutter/material.dart';
import 'package:youtube_player_flutter/youtube_player_flutter.dart';

class YoutubePlayerView extends StatefulWidget {
  final String videoId;

  YoutubePlayerView({this.videoId});

  @override
  _YoutubePlayerViewState createState() => _YoutubePlayerViewState();
}

class _YoutubePlayerViewState extends State<YoutubePlayerView> {
  YoutubePlayerController _youtubePlayerController;

  @override
  void initState() {
    super.initState();
    print(widget.videoId);
    this._youtubePlayerController = YoutubePlayerController(
      initialVideoId: widget.videoId,
      flags: YoutubePlayerFlags(
        autoPlay: true,
        mute: false,
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      child: YoutubePlayer(
        controller: _youtubePlayerController,
        showVideoProgressIndicator: true,
        onReady: () {
          print("Video ready!");
        },
      ),
    );
  }
}
