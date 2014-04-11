# What Song

A toy application to take a playlist URL, grab its streaming audio, and identify the song using [Echoprint](http://echoprint.me/) and [Echonest.](http://developer.echonest.com/docs/v4/song.html#identify)

## Todo

Plenty to do:
  * Refactor to use a `ReaderWriter` and provide some appropriate logging, as well as setting the environment
  * Have SBT build a proper application
  * Parse the Playlist response properly, rather than just looking at the first one
  * Parse the JSON response from Echonest properly, taking multiple track responses into account
  * Connect it to Twitter
  * Big one: re-implement Echoprint rather than calling an external application
  * This application feels like it could work well with Scalaz-stream
  * The requests using Finagle doesn't feel right
  * Rewrite it in Haskell :)
