var http = require('http');

var _RGBboy$elm_http_server$Native_HttpServer = function() {

  function listen (port, settings) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {

      var server = http.createServer();

      server.on('listening', function () {
        console.log('server listening on ', server.address());
        callback(_elm_lang$core$Native_Scheduler.succeed(server));
      });

      server.on('request', function (req, res) {
        var request = {
          request: req,
          response: res
        };
        _elm_lang$core$Native_Scheduler.rawSpawn(settings.onRequest(request));
      });

      server.on('close', function () {
        console.log('server closed');
        _elm_lang$core$Native_Scheduler.rawSpawn(settings.onClose());
      });

      server.listen(port);

      return function () {
        server.close();
      };
    });
  }

  function reply (request, string) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {

      request.response.end(string);

      callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Maybe$Nothing));
    });
  }

  function close (server) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {

      server.close();

      callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Maybe$Nothing));
    });
  }

  return {
    listen: F2(listen),
    reply: F2(reply),
    close: close
  };

}();
