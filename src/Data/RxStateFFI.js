/* global exports */
"use strict";

// module Data.RxState

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

exports.merge = function (ob) {
  return function(other) {
    return ob.merge(other);
  };
}

exports.filter = function (p){
  return function(ob){
    return ob.filter(p);
  };
}

exports._map = function (f) {
  return function(ob) {
    return ob.map(f);
  };
}

exports.flatMap = function (ob) {
  return function(f) {
    return ob.flatMap(f);
  };
}

exports.take = function (n) {
  return function(ob) {
    return ob.take(n);
  };
}

exports.just = Rx.Observable.just;

exports.combineLatest = function (f) {
  return function(ob1) {
    return function(ob2) {
      return ob1.combineLatest(ob2, function (x, y) {
        return f(x)(y);
      });
    };
  };
}
