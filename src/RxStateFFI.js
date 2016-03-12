/* global exports */
"use strict";

// module RxState

var Rx = require('rx');

exports.newChannel = function(val) {
  var subject = new Rx.BehaviorSubject(val);

  // var subscription = subject.subscribe(
  //   function (x) {
  //     console.log(x);
  //   },
  //   function (err) {
  //       console.log('Error: ' + err);
  //   },
  //   function () {
  //       console.log('Completed');
  //   });
  //
  // subject.onNext(42);

  return subject;
}

exports.send = function(val) {
  return function(subject) {
    return function() {
      subject.onNext(val);
    }
  }
}

exports.subscribe = function (ob) {
  return function(f) {
    return function() {
      return ob.subscribe(function(value) {
        f(value)();
      });
    };
  };
}

exports.foldp = function scan(f) {
  return function(seed) {
    return function(ob) {
      return ob.scan(function(acc, value) {
        return f(value)(acc);
      }, seed);
    };
  };
}
