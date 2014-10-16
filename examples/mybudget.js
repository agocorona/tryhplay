// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = f;
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f != __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            t.x = f();
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;        
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsQuerySelectorAll(elem, query) {
  var els = [0],
      len, nl, i;

  if (!elem || typeof elem.querySelectorAll !== 'function') {
    return els;
  }

  nl = elem.querySelectorAll(query);
  len = nl.length;

  for (i=len-1; i >= 0; --i) {
    els = [1, [0, nl[i]], els];
  }

  return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=function(_1,_2,_){var _3=jsCreateTextNode(toJSStr(E(_1))),_4=_3,_5=jsAppendChild(_4,E(_2)[1]);return [0,_4];},_6=new T(function(){return B(unCStr("id"));}),_7=0,_8=function(_9,_a,_b,_c){return new F(function(){return A(_9,[new T(function(){return function(_){var _d=jsSetAttr(E(_a)[1],toJSStr(E(_b)),toJSStr(E(_c)));return _7;};})]);});},_e=function(_f){return E(_f);},_g=function(_h,_i,_j,_){var _k=E(_i),_l=B(A(_h,[_j,_])),_m=_l,_n=B(A(_8,[_e,_m,_k[1],_k[2],_])),_o=_n;return _m;},_p=function(_q,_r){while(1){var _s=(function(_t,_u){var _v=E(_u);if(!_v[0]){return E(_t);}else{_q=function(_w,_){return new F(function(){return _g(_t,_v[1],_w,_);});};_r=_v[2];return null;}})(_q,_r);if(_s!=null){return _s;}}},_x=new T(function(){return B(unCStr("span"));}),_y=function(_z,_A,_){var _B=jsCreateElem(toJSStr(E(_z))),_C=_B,_D=jsAppendChild(_C,E(_A)[1]);return [0,_C];},_E=function(_w,_){return new F(function(){return _y(_x,_w,_);});},_F=function(_G,_H,_){return [0,_7,_G];},_I=function(_J,_){return [0,_J,_J];},_K=[0,_],_L=function(_M,_N,_){var _O=B(A(_M,[_])),_P=_O;return new F(function(){return A(_N,[_]);});},_Q=function(_R,_S,_){return new F(function(){return _L(_R,_S,_);});},_T=new T(function(){return B(unCStr("base"));}),_U=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_V=new T(function(){return B(unCStr("IOException"));}),_W=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_T,_U,_V],_X=[0],_Y=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_W,_X],_Z=function(_10){return E(_Y);},_11=function(_12){return E(E(_12)[1]);},_13=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_14=new T(function(){return B(err(_13));}),_15=function(_16,_17,_18){var _19=new T(function(){var _1a=B(A(_16,[_18])),_1b=B(A(_17,[new T(function(){var _1c=E(_19);return _1c[0]==0?E(_14):E(_1c[1]);})])),_1d=hs_eqWord64(_1a[1],_1b[1]),_1e=_1d;if(!E(_1e)){var _1f=[0];}else{var _1g=hs_eqWord64(_1a[2],_1b[2]),_1h=_1g,_1f=E(_1h)==0?[0]:[1,_18];}var _1i=_1f,_1j=_1i;return _1j;});return E(_19);},_1k=function(_1l){var _1m=E(_1l);return new F(function(){return _15(B(_11(_1m[1])),_Z,_1m[2]);});},_1n=new T(function(){return B(unCStr(": "));}),_1o=[0,41],_1p=new T(function(){return B(unCStr(" ("));}),_1q=function(_1r,_1s){var _1t=E(_1r);return _1t[0]==0?E(_1s):[1,_1t[1],new T(function(){return B(_1q(_1t[2],_1s));})];},_1u=new T(function(){return B(unCStr("already exists"));}),_1v=new T(function(){return B(unCStr("does not exist"));}),_1w=new T(function(){return B(unCStr("protocol error"));}),_1x=new T(function(){return B(unCStr("failed"));}),_1y=new T(function(){return B(unCStr("invalid argument"));}),_1z=new T(function(){return B(unCStr("inappropriate type"));}),_1A=new T(function(){return B(unCStr("hardware fault"));}),_1B=new T(function(){return B(unCStr("unsupported operation"));}),_1C=new T(function(){return B(unCStr("timeout"));}),_1D=new T(function(){return B(unCStr("resource vanished"));}),_1E=new T(function(){return B(unCStr("interrupted"));}),_1F=new T(function(){return B(unCStr("resource busy"));}),_1G=new T(function(){return B(unCStr("resource exhausted"));}),_1H=new T(function(){return B(unCStr("end of file"));}),_1I=new T(function(){return B(unCStr("illegal operation"));}),_1J=new T(function(){return B(unCStr("permission denied"));}),_1K=new T(function(){return B(unCStr("user error"));}),_1L=new T(function(){return B(unCStr("unsatisified constraints"));}),_1M=new T(function(){return B(unCStr("system error"));}),_1N=function(_1O,_1P){switch(E(_1O)){case 0:return new F(function(){return _1q(_1u,_1P);});break;case 1:return new F(function(){return _1q(_1v,_1P);});break;case 2:return new F(function(){return _1q(_1F,_1P);});break;case 3:return new F(function(){return _1q(_1G,_1P);});break;case 4:return new F(function(){return _1q(_1H,_1P);});break;case 5:return new F(function(){return _1q(_1I,_1P);});break;case 6:return new F(function(){return _1q(_1J,_1P);});break;case 7:return new F(function(){return _1q(_1K,_1P);});break;case 8:return new F(function(){return _1q(_1L,_1P);});break;case 9:return new F(function(){return _1q(_1M,_1P);});break;case 10:return new F(function(){return _1q(_1w,_1P);});break;case 11:return new F(function(){return _1q(_1x,_1P);});break;case 12:return new F(function(){return _1q(_1y,_1P);});break;case 13:return new F(function(){return _1q(_1z,_1P);});break;case 14:return new F(function(){return _1q(_1A,_1P);});break;case 15:return new F(function(){return _1q(_1B,_1P);});break;case 16:return new F(function(){return _1q(_1C,_1P);});break;case 17:return new F(function(){return _1q(_1D,_1P);});break;default:return new F(function(){return _1q(_1E,_1P);});}},_1Q=[0,125],_1R=new T(function(){return B(unCStr("{handle: "));}),_1S=function(_1T,_1U,_1V,_1W,_1X,_1Y){var _1Z=new T(function(){var _20=new T(function(){return B(_1N(_1U,new T(function(){var _21=E(_1W);return _21[0]==0?E(_1Y):B(_1q(_1p,new T(function(){return B(_1q(_21,[1,_1o,_1Y]));})));})));}),_22=E(_1V);return _22[0]==0?E(_20):B(_1q(_22,new T(function(){return B(_1q(_1n,_20));})));}),_23=E(_1X);if(!_23[0]){var _24=E(_1T);if(!_24[0]){return E(_1Z);}else{var _25=E(_24[1]);return _25[0]==0?B(_1q(_1R,new T(function(){return B(_1q(_25[1],[1,_1Q,new T(function(){return B(_1q(_1n,_1Z));})]));}))):B(_1q(_1R,new T(function(){return B(_1q(_25[1],[1,_1Q,new T(function(){return B(_1q(_1n,_1Z));})]));})));}}else{return new F(function(){return _1q(_23[1],new T(function(){return B(_1q(_1n,_1Z));}));});}},_26=function(_27){var _28=E(_27);return new F(function(){return _1S(_28[1],_28[2],_28[3],_28[4],_28[6],_X);});},_29=function(_2a,_2b){var _2c=E(_2a);return new F(function(){return _1S(_2c[1],_2c[2],_2c[3],_2c[4],_2c[6],_2b);});},_2d=[0,44],_2e=[0,93],_2f=[0,91],_2g=function(_2h,_2i,_2j){var _2k=E(_2i);return _2k[0]==0?B(unAppCStr("[]",_2j)):[1,_2f,new T(function(){return B(A(_2h,[_2k[1],new T(function(){var _2l=function(_2m){var _2n=E(_2m);return _2n[0]==0?E([1,_2e,_2j]):[1,_2d,new T(function(){return B(A(_2h,[_2n[1],new T(function(){return B(_2l(_2n[2]));})]));})];};return B(_2l(_2k[2]));})]));})];},_2o=function(_2p,_2q){return new F(function(){return _2g(_29,_2p,_2q);});},_2r=function(_2s,_2t,_2u){var _2v=E(_2t);return new F(function(){return _1S(_2v[1],_2v[2],_2v[3],_2v[4],_2v[6],_2u);});},_2w=[0,_2r,_26,_2o],_2x=new T(function(){return [0,_Z,_2w,_2y,_1k];}),_2y=function(_2z){return [0,_2x,_2z];},_2A=[0],_2B=7,_2C=function(_2D){return [0,_2A,_2B,_X,_2D,_2A,_2A];},_2E=function(_2F,_){return new F(function(){return die(new T(function(){return B(_2y(new T(function(){return B(_2C(_2F));})));}));});},_2G=function(_2H,_){return new F(function(){return _2E(_2H,_);});},_2I=function(_2J,_){return new F(function(){return _2G(_2J,_);});},_2K=function(_S,_){return new F(function(){return _2I(_S,_);});},_2L=function(_2M,_2N,_){var _2O=B(A(_2M,[_])),_2P=_2O;return new F(function(){return A(_2N,[_2P,_]);});},_2Q=function(_2R,_){return _2R;},_2S=[0,_2L,_Q,_2Q,_2K],_2T=function(_2U){return E(E(_2U)[1]);},_2V=function(_2W,_2X,_2Y,_2Z){return new F(function(){return A(_2T,[_2W,new T(function(){return B(A(_2X,[_2Z]));}),function(_30){return new F(function(){return A(_2Y,[new T(function(){return E(E(_30)[1]);}),new T(function(){return E(E(_30)[2]);})]);});}]);});},_31=function(_32,_33,_34,_35){return new F(function(){return A(_2T,[_32,new T(function(){return B(A(_33,[_35]));}),function(_36){return new F(function(){return A(_34,[new T(function(){return E(E(_36)[2]);})]);});}]);});},_37=function(_38,_39,_3a,_3b){return new F(function(){return _31(_38,_39,_3a,_3b);});},_3c=function(_3d){return E(E(_3d)[4]);},_3e=function(_3f,_3g){return function(_3h){return E(new T(function(){return B(A(_3c,[_3f,_3g]));}));};},_3i=function(_3j){return E(E(_3j)[3]);},_3k=function(_3l){return [0,function(_39,_3a,_3b){return new F(function(){return _2V(_3l,_39,_3a,_3b);});},function(_39,_3a,_3b){return new F(function(){return _37(_3l,_39,_3a,_3b);});},function(_3m,_3n){return new F(function(){return A(new T(function(){return B(_3i(_3l));}),[[0,_3m,_3n]]);});},function(_3b){return new F(function(){return _3e(_3l,_3b);});}];},_3o=new T(function(){return B(_3k(_2S));}),_3p=[0,112],_3q=function(_3r,_3s){var _3t=jsShowI(_3r),_3u=_3t;return new F(function(){return _1q(fromJSStr(_3u),_3s);});},_3v=[0,41],_3w=[0,40],_3x=function(_3y,_3z,_3A){return _3z>=0?B(_3q(_3z,_3A)):_3y<=6?B(_3q(_3z,_3A)):[1,_3w,new T(function(){var _3B=jsShowI(_3z),_3C=_3B;return B(_1q(fromJSStr(_3C),[1,_3v,_3A]));})];},_3D=function(_3E,_3F,_3G,_3H){var _3I=E(_3F);return new F(function(){return A(_3I[1],[new T(function(){var _3J=E(_3E);return E(_3G);}),function(_3K){var _3L=new T(function(){return E(E(_3K)[2]);});return new F(function(){return A(_3I[2],[new T(function(){return B(A(_3H,[new T(function(){var _3M=E(new T(function(){var _3N=E(_3E);return [0,_];})),_3O=E(_3K);return [0,_3O[1],new T(function(){return [0,E(_3L)[1]+1|0];}),_3O[3],_3O[4],_3O[5],_3O[6]];})]));}),new T(function(){return B(A(_3I[3],[[1,_3p,new T(function(){return B(_1q(B(_3x(0,E(_3L)[1],_X)),new T(function(){return E(E(_3K)[1]);})));})]]));})]);});}]);});},_3P=new T(function(){return B(_3D(_K,_3o,_I,_F));}),_3Q=function(_3R,_3S,_3T,_){var _3U=B(A(_3P,[_3T,_])),_3V=_3U,_3W=E(_3V),_3X=_3W[1],_3Y=E(_3W[2]),_3Z=_3Y[2],_40=E(_3Y[4]),_41=B(A(_3R,[[0,_3Y[1],_3Z,_3Y[3],[0,function(_42,_){var _43=B(A(_3R,[new T(function(){var _44=E(_42);return [0,_44[1],_3Z,_44[3],_44[4],_44[5],_44[6]];}),_])),_45=_43;return [0,[0,_2Q,E(E(_45)[1])[2]],_42];},[1,[0,_3S,_3X],_40[2]]],_3Y[5],_3Y[6]],_])),_46=_41,_47=E(_46),_48=_47[2],_49=E(_47[1]),_4a=_49[1],_4b=new T(function(){return B(_p(_E,[1,[0,_6,_3X],_X]));}),_4c=E(_49[2]);if(!_4c[0]){return !E(E(_3T)[5])?[0,[0,function(_4d,_){var _4e=B(A(_4a,[_4d,_])),_4f=_4e,_4g=B(A(_4b,[_4d,_])),_4h=_4g;return _4d;},_2A],new T(function(){var _4i=E(_48);return [0,_4i[1],_4i[2],_4i[3],_40,_4i[5],_4i[6]];})]:[0,[0,_4a,_2A],new T(function(){var _4j=E(_48);return [0,_4j[1],_4j[2],_4j[3],_40,_4j[5],_4j[6]];})];}else{var _4k=B(A(_3S,[_4c[1],new T(function(){var _4l=E(_48);return [0,_4l[1],_4l[2],_4l[3],_40,_4l[5],_4l[6]];}),_])),_4m=_4k,_4n=E(_4m),_4o=_4n[2],_4p=E(_4n[1]),_4q=_4p[1],_4r=_4p[2];return !E(E(_3T)[5])?[0,[0,function(_4s,_){var _4t=B(A(_4a,[_4s,_])),_4u=_4t,_4v=B(A(_4b,[_4s,_])),_4w=_4v,_4x=B(A(_4q,[_4w,_])),_4y=_4x;return _4s;},_4r],_4o]:[0,[0,function(_4z,_){var _4A=B(A(_4a,[_4z,_])),_4B=_4A,_4C=B(A(_4q,[_4z,_])),_4D=_4C;return _4z;},_4r],_4o];}},_4E=[1,_7],_4F=new T(function(){return B(unCStr("h1"));}),_4G=function(_4H,_4I){return function(_4J,_){var _4K=jsCreateElem(toJSStr(E(_4F))),_4L=_4K,_4M=jsAppendChild(_4L,E(_4J)[1]),_4N=[0,_4L],_4O=B(A(new T(function(){return B(A(_4H,[_4I]));}),[_4N,_])),_4P=_4O;return _4N;};},_4Q=new T(function(){return B(unCStr("Personal Budget"));}),_4R=new T(function(){return B(_4G(_0,_4Q));}),_4S=[0,_4R,_4E],_4T=function(_4U,_){return [0,_4S,_4U];},_4V=new T(function(){return [0,"(function(){return document.head;})"];}),_4W=function(_4X){var _4Y=B(A(_4X,[_])),_4Z=_4Y;return E(_4Z);},_50=function(_51){return new F(function(){return _4W(function(_){var _=0;return new F(function(){return eval(E(_51)[1]);});});});},_52=new T(function(){return B(_50(_4V));}),_53=[0,0],_54=false,_55=2,_56=[1],_57=[0,_2Q,_2A],_58=function(_59,_){return [0,_57,_59];},_5a=new T(function(){return B(unCStr("noid"));}),_5b=function(_5c,_5d,_){return [0,_57,_5d];},_5e=[0,_5b,_5a],_5f=[1,_5e,_X],_5g=[0,_58,_5f],_5h=[0,_X,_53,_55,_5g,_54,_56],_5i=function(_){var _=0,_5j=newMVar(),_5k=_5j,_=putMVar(_5k,_5h);return [0,_5k];},_5l=new T(function(){return B(_4W(_5i));}),_5m=0,_5n=new T(function(){return B(unCStr("Edit"));}),_5o=[8,_],_5p=new T(function(){return B(unCStr("value"));}),_5q=new T(function(){return B(unCStr("type"));}),_5r=new T(function(){return B(unCStr("input"));}),_5s=function(_5t,_){var _5u=jsCreateElem(toJSStr(E(_5r))),_5v=_5u,_5w=jsAppendChild(_5v,E(_5t)[1]);return [0,_5v];},_5x=new T(function(){return B(unCStr("keydown"));}),_5y=new T(function(){return B(unCStr("mousemove"));}),_5z=new T(function(){return B(unCStr("blur"));}),_5A=new T(function(){return B(unCStr("focus"));}),_5B=new T(function(){return B(unCStr("change"));}),_5C=new T(function(){return B(unCStr("unload"));}),_5D=new T(function(){return B(unCStr("load"));}),_5E=new T(function(){return B(unCStr("keyup"));}),_5F=new T(function(){return B(unCStr("keypress"));}),_5G=new T(function(){return B(unCStr("mouseup"));}),_5H=new T(function(){return B(unCStr("mousedown"));}),_5I=new T(function(){return B(unCStr("dblclick"));}),_5J=new T(function(){return B(unCStr("click"));}),_5K=new T(function(){return B(unCStr("mouseout"));}),_5L=new T(function(){return B(unCStr("mouseover"));}),_5M=function(_5N){switch(E(_5N)[0]){case 0:return E(_5D);case 1:return E(_5C);case 2:return E(_5B);case 3:return E(_5A);case 4:return E(_5z);case 5:return E(_5y);case 6:return E(_5L);case 7:return E(_5K);case 8:return E(_5J);case 9:return E(_5I);case 10:return E(_5H);case 11:return E(_5G);case 12:return E(_5F);case 13:return E(_5E);default:return E(_5x);}},_5O=function(_5P,_5Q){while(1){var _5R=E(_5P);if(!_5R[0]){return E(_5Q)[0]==0?true:false;}else{var _5S=E(_5Q);if(!_5S[0]){return false;}else{if(E(_5R[1])[1]!=E(_5S[1])[1]){return false;}else{_5P=_5R[2];_5Q=_5S[2];continue;}}}}},_5T=[0],_5U=new T(function(){return B(unCStr("Onload"));}),_5V=[0,_5U,_5T],_5W=new T(function(){return B(unCStr("OnLoad"));}),_5X=[0,_5W,_5T],_5Y=function(_){var _=0,_5Z=newMVar(),_60=_5Z,_=putMVar(_60,_5X);return [0,_60];},_61=new T(function(){return B(_4W(_5Y));}),_62=function(_63,_64,_){var _65=B(A(_63,[_])),_66=_65;return new F(function(){return die(_64);});},_67=function(_68,_69,_6a,_){return new F(function(){return _62(function(_){var _=putMVar(_69,_68);return _7;},_6a,_);});},_6b=function(_6c,_){var _6d=0,_6e=_6d;if(!E(_6e)){return new F(function(){return (function(_){var _6f=E(_61)[1],_6g=takeMVar(_6f),_6h=_6g,_6i=jsCatch(function(_){return new F(function(){return (function(_){return _6c;})();});},function(_w,_){return new F(function(){return _67(_6h,_6f,_w,_);});}),_6j=_6i,_=putMVar(_6f,_6j);return _7;})();});}else{var _6k=E(_61)[1],_6l=takeMVar(_6k),_6m=_6l,_6n=jsCatch(function(_){return _6c;},function(_w,_){return new F(function(){return _67(_6m,_6k,_w,_);});}),_6o=_6n,_=putMVar(_6k,_6o);return _7;}},_6p=function(_6q,_){var _6r=B(_6b(_5V,_)),_6s=_6r;return [0,[0,_2Q,[1,_6s]],_6q];},_6t=function(_){var _6u=E(_61)[1],_6v=takeMVar(_6u),_6w=_6v,_=putMVar(_6u,_6w);return _6w;},_6x=function(_6y,_){var _6z=0,_6A=_6z;if(!E(_6A)){var _6B=B(_6t()),_6C=_6B;return [0,[0,_2Q,[1,_6C]],_6y];}else{var _6D=E(_61)[1],_6E=takeMVar(_6D),_6F=_6E,_=putMVar(_6D,_6F);return [0,[0,_2Q,[1,_6F]],_6y];}},_6G=[0,_2Q,_2A],_6H=true,_6I=function(_6J,_6K,_){var _6L=B(A(_6J,[new T(function(){var _6M=E(_6K);return [0,_6M[1],_6M[2],_6M[3],_6M[4],_6H,_6M[6]];}),_])),_6N=_6L;return [0,new T(function(){return E(E(_6N)[1]);}),new T(function(){var _6O=E(E(_6N)[2]);return [0,_6O[1],_6O[2],_6O[3],_6O[4],new T(function(){return E(E(_6K)[5]);}),_6O[6]];})];},_6P=function(_6Q,_){var _6R=0,_6S=_6R;if(!E(_6S)){return new F(function(){return (function(_){var _6T=E(_61)[1],_6U=takeMVar(_6T),_6V=_6U,_6W=jsCatch(function(_){return new F(function(){return (function(_){return _6Q;})();});},function(_w,_){return new F(function(){return _67(_6V,_6T,_w,_);});}),_6X=_6W,_=putMVar(_6T,_6X);return _7;})();});}else{var _6Y=E(_61)[1],_6Z=takeMVar(_6Y),_70=_6Z,_71=jsCatch(function(_){return _6Q;},function(_w,_){return new F(function(){return _67(_70,_6Y,_w,_);});}),_72=_71,_=putMVar(_6Y,_72);return _7;}},_73=new T(function(){return B(unCStr("true"));}),_74=new T(function(){return [0,"keydown"];}),_75=new T(function(){return [0,"mousemove"];}),_76=new T(function(){return [0,"blur"];}),_77=new T(function(){return [0,"focus"];}),_78=new T(function(){return [0,"change"];}),_79=new T(function(){return [0,"unload"];}),_7a=new T(function(){return [0,"load"];}),_7b=new T(function(){return [0,"keyup"];}),_7c=new T(function(){return [0,"keypress"];}),_7d=new T(function(){return [0,"mouseup"];}),_7e=new T(function(){return [0,"mousedown"];}),_7f=new T(function(){return [0,"dblclick"];}),_7g=new T(function(){return [0,"click"];}),_7h=new T(function(){return [0,"mouseout"];}),_7i=new T(function(){return [0,"mouseover"];}),_7j=function(_7k){switch(E(_7k)[0]){case 0:return E(_7a);case 1:return E(_79);case 2:return E(_78);case 3:return E(_77);case 4:return E(_76);case 5:return E(_75);case 6:return E(_7i);case 7:return E(_7h);case 8:return E(_7g);case 9:return E(_7f);case 10:return E(_7e);case 11:return E(_7d);case 12:return E(_7c);case 13:return E(_7b);default:return E(_74);}},_7l=function(_7m,_7n,_7o){return function(_7p,_){var _7q=B(A(_7m,[_7p,_])),_7r=_7q,_7s=E(_7r),_7t=_7s[1],_7u=E(new T(function(){return B(_5M(_7n));})),_7v=jsGetAttr(_7t,toJSStr(_7u)),_7w=_7v;if(!B(_5O(fromJSStr(_7w),_73))){var _7x=E(_7o),_7y=jsSetCB(_7t,E(new T(function(){return B(_7j(_7n));}))[1],E([0,_7o])[1]),_7z=_7y,_7A=B(A(_8,[_e,_7s,_7u,_73,_])),_7B=_7A;return _7s;}else{return _7s;}};},_7C=2,_7D=function(_7E,_7F,_){return [0,_57,_7F];},_7G=function(_7H,_7I,_7J,_7K,_){var _7L=B(A(_7J,[_7K,_])),_7M=_7L,_7N=E(_7M),_7O=E(_7N[1]),_7P=_7O[1];return [0,[0,function(_7Q,_){var _7R=jsFind(toJSStr(E(_7H))),_7S=_7R,_7T=E(_7S);if(!_7T[0]){return _7Q;}else{var _7U=_7T[1];switch(E(_7I)){case 0:var _7V=B(A(_7P,[_7U,_])),_7W=_7V;return _7Q;case 1:var _7X=E(_7U),_7Y=_7X[1],_7Z=jsGetChildren(_7Y),_80=_7Z,_81=E(_80);if(!_81[0]){var _82=B(A(_7P,[_7X,_])),_83=_82;return _7Q;}else{var _84=jsCreateElem(toJSStr(E(_x))),_85=_84,_86=jsAddChildBefore(_85,_7Y,E(_81[1])[1]),_87=B(A(_7P,[[0,_85],_])),_88=_87;return _7Q;}break;default:var _89=E(_7U),_8a=jsClearChildren(_89[1]),_8b=B(A(_7P,[_89,_])),_8c=_8b;return _7Q;}}},_7O[2]],_7N[2]];},_8d=function(_8e){var _8f=E(_8e);if(!_8f[0]){return E(_7D);}else{var _8g=E(_8f[1]);return function(_8h){return function(_8i,_8j){return new F(function(){return _3Q(function(_w,_){return new F(function(){return _7G(_8g[2],_7C,new T(function(){return B(A(_8g[1],[_8h]));}),_w,_);});},new T(function(){return B(_8d(_8f[2]));}),_8i,_8j);});};};}},_8k=new T(function(){return [0,"(function(){return document.body;})"];}),_8l=new T(function(){return B(_50(_8k));}),_8m=function(_8n,_8o){var _8p=new T(function(){return B(_5M(_8o));}),_8q=[0,_8p,_5T];return function(_8r,_){var _8s=E(_8r),_8t=E(_8s[4]),_8u=B(A(_8n,[_8s,_])),_8v=_8u,_8w=E(_8v),_8x=E(_8w[1]),_8y=_8x[1];return [0,[0,new T(function(){var _8z=function(_){var _8A=B(A(_8l,[_])),_8B=_8A,_8C=E(_5l)[1],_8D=takeMVar(_8C),_8E=_8D,_8F=B(_3Q(_8t[1],new T(function(){return B(_8d(_8t[2]));}),_8E,_)),_8G=_8F,_8H=E(_8G),_=putMVar(_8C,_8H[2]),_8I=B(A(E(_8H[1])[1],[[0,_8B],_])),_8J=_8I;return _7;},_8K=E(_8o);switch(_8K[0]){case 0:var _8L=B(_7l(_8y,_8K,function(_){var _8M=B(_6P(_8q,_)),_8N=_8M;return new F(function(){return _8z(_);});}));break;case 1:var _8L=B(_7l(_8y,_8K,function(_){var _8O=B(_6P(_8q,_)),_8P=_8O;return new F(function(){return _8z(_);});}));break;case 2:var _8L=B(_7l(_8y,_8K,function(_){var _8Q=B(_6P(_8q,_)),_8R=_8Q;return new F(function(){return _8z(_);});}));break;case 3:var _8L=B(_7l(_8y,_8K,function(_){var _8S=B(_6P(_8q,_)),_8T=_8S;return new F(function(){return _8z(_);});}));break;case 4:var _8L=B(_7l(_8y,_8K,function(_){var _8U=B(_6P(_8q,_)),_8V=_8U;return new F(function(){return _8z(_);});}));break;case 5:var _8L=B(_7l(_8y,_8K,function(_8W,_){var _8X=B(_6P([0,_8p,[2,E(_8W)]],_)),_8Y=_8X;return new F(function(){return _8z(_);});}));break;case 6:var _8L=B(_7l(_8y,_8K,function(_8Z,_){var _90=B(_6P([0,_8p,[2,E(_8Z)]],_)),_91=_90;return new F(function(){return _8z(_);});}));break;case 7:var _8L=B(_7l(_8y,_8K,_8z));break;case 8:var _8L=B(_7l(_8y,_8K,function(_92,_93,_){var _94=B(_6P([0,_8p,[1,_92,E(_93)]],_)),_95=_94;return new F(function(){return _8z(_);});}));break;case 9:var _8L=B(_7l(_8y,_8K,function(_96,_97,_){var _98=B(_6P([0,_8p,[1,_96,E(_97)]],_)),_99=_98;return new F(function(){return _8z(_);});}));break;case 10:var _8L=B(_7l(_8y,_8K,function(_9a,_9b,_){var _9c=B(_6P([0,_8p,[1,_9a,E(_9b)]],_)),_9d=_9c;return new F(function(){return _8z(_);});}));break;case 11:var _8L=B(_7l(_8y,_8K,function(_9e,_9f,_){var _9g=B(_6P([0,_8p,[1,_9e,E(_9f)]],_)),_9h=_9g;return new F(function(){return _8z(_);});}));break;case 12:var _8L=B(_7l(_8y,_8K,function(_9i,_){var _9j=B(_6P([0,_8p,[3,_9i]],_)),_9k=_9j;return new F(function(){return _8z(_);});}));break;case 13:var _8L=B(_7l(_8y,_8K,function(_9l,_){var _9m=B(_6P([0,_8p,[3,_9l]],_)),_9n=_9m;return new F(function(){return _8z(_);});}));break;default:var _8L=B(_7l(_8y,_8K,function(_9o,_){var _9p=B(_6P([0,_8p,[3,_9o]],_)),_9q=_9p;return new F(function(){return _8z(_);});}));}return _8L;}),_8x[2]],_8w[2]];};},_9r=function(_9s,_9t){return function(_8i,_8j){return new F(function(){return _6I(function(_w,_){return new F(function(){return _3Q(_6p,function(_9u,_w,_){return new F(function(){return (function(_w,_){return new F(function(){return _3Q(new T(function(){return B(_8m(function(_9v,_){return [0,[0,_9s,_4E],_9v];},_9t));}),function(_9w){return function(_w,_){return new F(function(){return _3Q(_6x,function(_9x){var _9y=E(_9x);return new F(function(){return (function(_9z,_9A){return function(_9B,_){return !E(new T(function(){return B(_5O(new T(function(){return B(_5M(_9t));}),_9z));}))?[0,_6G,_9B]:[0,[0,_2Q,[1,[0,_9z,_9A]]],_9B];};})(_9y[1],_9y[2]);});},_w,_);});};},_w,_);});})(_w,_);});},_w,_);});},_8i,_8j);});};},_9C=new T(function(){return B(unCStr("submit"));}),_9D=function(_9E,_9F){return function(_8i,_8j){return new F(function(){return _6I(function(_w,_){return new F(function(){return _3Q(new T(function(){return B(_9r(function(_9G,_){var _9H=B(_5s(_9G,_)),_9I=_9H,_9J=B(A(_8,[_e,_9I,_5q,_9C,_])),_9K=_9J,_9L=B(A(_8,[_e,_9I,_5p,_9F,_])),_9M=_9L;return _9I;},_5o));}),function(_9N,_w,_){return new F(function(){return (function(_9O,_){return [0,[0,_2Q,[1,_9E]],_9O];})(_w,_);});},_w,_);});},_8i,_8j);});};},_9P=new T(function(){return B(_9D(_5m,_5n));}),_9Q=1,_9R=new T(function(){return B(unCStr("View Entries"));}),_9S=new T(function(){return B(_9D(_9Q,_9R));}),_9T=2,_9U=new T(function(){return B(unCStr("Preview expenses"));}),_9V=new T(function(){return B(_9D(_9T,_9U));}),_9W=new T(function(){return B(unCStr("hr"));}),_9X=function(_9Y,_){var _9Z=jsCreateElem(toJSStr(E(_9W))),_a0=_9Z,_a1=jsAppendChild(_a0,E(_9Y)[1]);return [0,_a0];},_a2=function(_a3,_){var _a4=B(A(_9P,[_a3,_])),_a5=_a4,_a6=E(_a5),_a7=E(_a6[1]),_a8=B(A(_9S,[_a6[2],_])),_a9=_a8,_aa=E(_a9),_ab=E(_aa[1]),_ac=B(A(_9V,[_aa[2],_])),_ad=_ac,_ae=E(_ad),_af=E(_ae[1]);return [0,[0,function(_ag,_){var _ah=B(A(_a7[1],[_ag,_])),_ai=_ah,_aj=B(A(_ab[1],[_ag,_])),_ak=_aj,_al=B(A(_af[1],[_ag,_])),_am=_al,_an=B(_9X(_ag,_)),_ao=_an;return _ag;},new T(function(){var _ap=E(_a7[2]);if(!_ap[0]){var _aq=E(_ab[2]),_ar=_aq[0]==0?E(_af[2]):E(_aq);}else{var _ar=E(_ap);}return _ar;})],_ae[2]];},_as=function(_at,_){return _at;},_au=[0,50],_av=[0,1850],_aw=[0,2000],_ax=[0,_au,_au,_au,_av,_aw],_ay=[1,_ax],_az=function(_aA,_aB){var _aC=hs_leWord64(_aA,_aB),_aD=_aC;return E(_aD)==0?false:true;},_aE=function(_aF,_aG,_aH,_aI){var _aJ=hs_eqWord64(_aF,_aH),_aK=_aJ;if(!E(_aK)){var _aL=hs_leWord64(_aF,_aH),_aM=_aL;return E(_aM)==0?false:true;}else{return new F(function(){return _az(_aG,_aI);});}},_aN=function(_aO,_aP){var _aQ=E(_aO),_aR=_aQ[1],_aS=_aQ[2],_aT=E(_aP),_aU=_aT[1],_aV=_aT[2],_aW=hs_eqWord64(_aR,_aU),_aX=_aW;if(!E(_aX)){return !B(_aE(_aR,_aS,_aU,_aV))?2:0;}else{var _aY=hs_eqWord64(_aS,_aV),_aZ=_aY;return E(_aZ)==0?!B(_aE(_aR,_aS,_aU,_aV))?2:0:1;}},_b0=function(_b1,_b2){while(1){var _b3=E(_b1),_b4=E(_b2);if(!_b4[0]){switch(B(_aN(_b3,_b4[2]))){case 0:_b1=_b3;_b2=_b4[4];continue;case 1:return [1,_b4[3]];default:_b1=_b3;_b2=_b4[5];continue;}}else{return [0];}}},_b5=new T(function(){return B(unCStr("Prelude.undefined"));}),_b6=new T(function(){return B(err(_b5));}),_b7=new T(function(){return E(_b6);}),_b8=function(_b9,_ba,_bb,_bc){var _bd=E(_ba),_be=_bd[1],_bf=_bd[3];return new F(function(){return A(_be,[new T(function(){return B(A(_be,[_bb,function(_bg){return new F(function(){return A(_bf,[new T(function(){var _bh=E(_b9);return E(E(_bg)[6]);})]);});}]));}),function(_bi){var _bj=B(_b0(new T(function(){return B(A(_bc,[_b7]));}),_bi));return _bj[0]==0?E(new T(function(){return B(A(_bf,[_2A]));})):B(A(_bf,[[1,_bj[1]]]));}]);});},_bk=new T(function(){return B(unCStr("ghc-prim"));}),_bl=new T(function(){return B(unCStr("GHC.Types"));}),_bm=new T(function(){return B(unCStr("Double"));}),_bn=[0,I_fromBits([2568654869,2026863713]),I_fromBits([3333976055,2507330877]),_bk,_bl,_bm],_bo=[0,I_fromBits([2568654869,2026863713]),I_fromBits([3333976055,2507330877]),_bn,_X],_bp=function(_bq){return E(_bo);},_br=new T(function(){return B(unCStr("GHC.Tuple"));}),_bs=new T(function(){return B(unCStr("(,,,,)"));}),_bt=[0,I_fromBits([277338040,1071834873]),I_fromBits([3942927863,1873345003]),_bk,_br,_bs],_bu=[0,I_fromBits([277338040,1071834873]),I_fromBits([3942927863,1873345003]),_bt,_X],_bv=function(_bw){return E(_bu);},_bx=function(_by){var _bz=E(_by);if(!_bz[0]){return [0];}else{return new F(function(){return _1q(_bz[1],new T(function(){return B(_bx(_bz[2]));}));});}},_bA=function(_bB,_bC){var _bD=E(_bB);if(!_bD){return [0,_X,_bC];}else{var _bE=E(_bC);if(!_bE[0]){return [0,_X,_X];}else{var _bF=new T(function(){var _bG=B(_bA(_bD-1|0,_bE[2]));return [0,_bG[1],_bG[2]];});return [0,[1,_bE[1],new T(function(){return E(E(_bF)[1]);})],new T(function(){return E(E(_bF)[2]);})];}}},_bH=[0,120],_bI=[0,48],_bJ=function(_bK){var _bL=new T(function(){var _bM=B(_bA(8,new T(function(){var _bN=md5(toJSStr(E(_bK))),_bO=_bN;return fromJSStr(_bO);})));return [0,_bM[1],_bM[2]];}),_bP=parseInt([0,toJSStr([1,_bI,[1,_bH,new T(function(){return E(E(_bL)[1]);})]])]),_bQ=_bP,_bR=new T(function(){var _bS=B(_bA(8,new T(function(){return E(E(_bL)[2]);})));return [0,_bS[1],_bS[2]];}),_bT=parseInt([0,toJSStr([1,_bI,[1,_bH,new T(function(){return E(E(_bR)[1]);})]])]),_bU=_bT,_bV=hs_mkWord64(_bQ,_bU),_bW=_bV,_bX=parseInt([0,toJSStr([1,_bI,[1,_bH,new T(function(){return E(B(_bA(8,new T(function(){return E(E(_bR)[2]);})))[1]);})]])]),_bY=_bX,_bZ=hs_mkWord64(_bY,_bY),_c0=_bZ;return [0,_bW,_c0];},_c1=function(_c2,_c3){var _c4=E(_c3);return _c4[0]==0?[0]:[1,new T(function(){return B(A(_c2,[_c4[1]]));}),new T(function(){return B(_c1(_c2,_c4[2]));})];},_c5=function(_c6,_c7){var _c8=jsShowI(_c6),_c9=_c8,_ca=md5(_c9),_cb=_ca;return new F(function(){return _1q(fromJSStr(_cb),new T(function(){var _cc=jsShowI(_c7),_cd=_cc,_ce=md5(_cd),_cf=_ce;return fromJSStr(_cf);}));});},_cg=function(_ch){var _ci=E(_ch);return new F(function(){return _c5(_ci[1],_ci[2]);});},_cj=function(_ck){var _cl=E(_ck);if(!_cl[0]){return [0];}else{var _cm=E(_cl[1]);return [1,[0,_cm[1],_cm[2]],new T(function(){return B(_cj(_cl[2]));})];}},_cn=function(_co,_cp){return function(_cq){return E(new T(function(){var _cr=B(A(_co,[_b6])),_cs=E(_cr[3]),_ct=_cs[1],_cu=_cs[2],_cv=B(_1q(_cr[4],[1,new T(function(){return B(A(_cp,[_b6]));}),_X]));if(!_cv[0]){var _cw=[0,_ct,_cu,_cs,_X];}else{var _cx=B(_bJ(new T(function(){return B(_bx(B(_c1(_cg,[1,[0,_ct,_cu],new T(function(){return B(_cj(_cv));})]))));}))),_cw=[0,_cx[1],_cx[2],_cs,_cv];}var _cy=_cw,_cz=_cy;return _cz;}));};},_cA=new T(function(){return B(_cn(_bv,_bp));}),_cB=function(_cC){var _cD=E(_cC);if(!_cD[0]){return [0];}else{var _cE=E(_cD[1]);return [1,[0,_cE[1],_cE[2]],new T(function(){return B(_cB(_cD[2]));})];}},_cF=function(_cG,_cH){return function(_cI){return E(new T(function(){var _cJ=B(A(_cG,[_b6])),_cK=E(_cJ[3]),_cL=_cK[1],_cM=_cK[2],_cN=B(_1q(_cJ[4],[1,new T(function(){return B(A(_cH,[_b6]));}),_X]));if(!_cN[0]){var _cO=[0,_cL,_cM,_cK,_X];}else{var _cP=B(_bJ(new T(function(){return B(_bx(B(_c1(_cg,[1,[0,_cL,_cM],new T(function(){return B(_cB(_cN));})]))));}))),_cO=[0,_cP[1],_cP[2],_cK,_cN];}var _cQ=_cO,_cR=_cQ;return _cR;}));};},_cS=new T(function(){return B(_cF(_cA,_bp));}),_cT=function(_cU){var _cV=E(_cU);if(!_cV[0]){return [0];}else{var _cW=E(_cV[1]);return [1,[0,_cW[1],_cW[2]],new T(function(){return B(_cT(_cV[2]));})];}},_cX=function(_cY,_cZ){return function(_d0){return E(new T(function(){var _d1=B(A(_cY,[_b6])),_d2=E(_d1[3]),_d3=_d2[1],_d4=_d2[2],_d5=B(_1q(_d1[4],[1,new T(function(){return B(A(_cZ,[_b6]));}),_X]));if(!_d5[0]){var _d6=[0,_d3,_d4,_d2,_X];}else{var _d7=B(_bJ(new T(function(){return B(_bx(B(_c1(_cg,[1,[0,_d3,_d4],new T(function(){return B(_cT(_d5));})]))));}))),_d6=[0,_d7[1],_d7[2],_d2,_d5];}var _d8=_d6,_d9=_d8;return _d9;}));};},_da=new T(function(){return B(_cX(_cS,_bp));}),_db=function(_dc){var _dd=E(_dc);if(!_dd[0]){return [0];}else{var _de=E(_dd[1]);return [1,[0,_de[1],_de[2]],new T(function(){return B(_db(_dd[2]));})];}},_df=function(_dg,_dh){return function(_di){return E(new T(function(){var _dj=B(A(_dg,[_b6])),_dk=E(_dj[3]),_dl=_dk[1],_dm=_dk[2],_dn=B(_1q(_dj[4],[1,new T(function(){return B(A(_dh,[_b6]));}),_X]));if(!_dn[0]){var _do=[0,_dl,_dm,_dk,_X];}else{var _dp=B(_bJ(new T(function(){return B(_bx(B(_c1(_cg,[1,[0,_dl,_dm],new T(function(){return B(_db(_dn));})]))));}))),_do=[0,_dp[1],_dp[2],_dk,_dn];}var _dq=_do,_dr=_dq;return _dr;}));};},_ds=new T(function(){return B(_df(_da,_bp));}),_dt=function(_du){var _dv=E(_du);if(!_dv[0]){return [0];}else{var _dw=E(_dv[1]);return [1,[0,_dw[1],_dw[2]],new T(function(){return B(_dt(_dv[2]));})];}},_dx=function(_dy,_dz){return function(_dA){return E(new T(function(){var _dB=B(A(_dy,[_b6])),_dC=E(_dB[3]),_dD=_dC[1],_dE=_dC[2],_dF=B(_1q(_dB[4],[1,new T(function(){return B(A(_dz,[_b6]));}),_X]));if(!_dF[0]){var _dG=[0,_dD,_dE,_dC,_X];}else{var _dH=B(_bJ(new T(function(){return B(_bx(B(_c1(_cg,[1,[0,_dD,_dE],new T(function(){return B(_dt(_dF));})]))));}))),_dG=[0,_dH[1],_dH[2],_dC,_dF];}var _dI=_dG,_dJ=_dI;return _dJ;}));};},_dK=new T(function(){return B(_dx(_ds,_bp));}),_dL=new T(function(){return B(_b8(_K,_3o,_I,_dK));}),_dM=function(_dN,_){var _dO=B(A(_dL,[_dN,_])),_dP=_dO;return [0,[0,_as,new T(function(){var _dQ=E(E(_dP)[1]);return _dQ[0]==0?E(_ay):E(_dQ);})],new T(function(){return E(E(_dP)[2]);})];},_dR=function(_dS,_){return [0,[0,_2Q,[1,_dS]],_dS];},_dT=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_dU=new T(function(){return B(err(_dT));}),_dV=function(_dW,_dX,_dY,_dZ){var _e0=E(_dZ);if(!_e0[0]){var _e1=_e0[1],_e2=E(_dY);if(!_e2[0]){var _e3=_e2[1],_e4=_e2[2],_e5=_e2[3];if(_e3<=(imul(3,_e1)|0)){return [0,(1+_e3|0)+_e1|0,E(E(_dW)),_dX,E(_e2),E(_e0)];}else{var _e6=E(_e2[4]);if(!_e6[0]){var _e7=_e6[1],_e8=E(_e2[5]);if(!_e8[0]){var _e9=_e8[1],_ea=_e8[2],_eb=_e8[3],_ec=_e8[4];if(_e9>=(imul(2,_e7)|0)){var _ed=function(_ee){var _ef=E(_e8[5]);return _ef[0]==0?[0,(1+_e3|0)+_e1|0,E(_ea),_eb,E([0,(1+_e7|0)+_ee|0,E(_e4),_e5,E(_e6),E(_ec)]),E([0,(1+_e1|0)+_ef[1]|0,E(E(_dW)),_dX,E(_ef),E(_e0)])]:[0,(1+_e3|0)+_e1|0,E(_ea),_eb,E([0,(1+_e7|0)+_ee|0,E(_e4),_e5,E(_e6),E(_ec)]),E([0,1+_e1|0,E(E(_dW)),_dX,E(_56),E(_e0)])];},_eg=E(_ec);return _eg[0]==0?B(_ed(_eg[1])):B(_ed(0));}else{return [0,(1+_e3|0)+_e1|0,E(_e4),_e5,E(_e6),E([0,(1+_e1|0)+_e9|0,E(E(_dW)),_dX,E(_e8),E(_e0)])];}}else{return E(_dU);}}else{return E(_dU);}}}else{return [0,1+_e1|0,E(E(_dW)),_dX,E(_56),E(_e0)];}}else{var _eh=E(_dY);if(!_eh[0]){var _ei=_eh[1],_ej=_eh[2],_ek=_eh[3],_el=_eh[5],_em=E(_eh[4]);if(!_em[0]){var _en=_em[1],_eo=E(_el);if(!_eo[0]){var _ep=_eo[1],_eq=_eo[2],_er=_eo[3],_es=_eo[4];if(_ep>=(imul(2,_en)|0)){var _et=function(_eu){var _ev=E(_eo[5]);return _ev[0]==0?[0,1+_ei|0,E(_eq),_er,E([0,(1+_en|0)+_eu|0,E(_ej),_ek,E(_em),E(_es)]),E([0,1+_ev[1]|0,E(E(_dW)),_dX,E(_ev),E(_56)])]:[0,1+_ei|0,E(_eq),_er,E([0,(1+_en|0)+_eu|0,E(_ej),_ek,E(_em),E(_es)]),E([0,1,E(E(_dW)),_dX,E(_56),E(_56)])];},_ew=E(_es);return _ew[0]==0?B(_et(_ew[1])):B(_et(0));}else{return [0,1+_ei|0,E(_ej),_ek,E(_em),E([0,1+_ep|0,E(E(_dW)),_dX,E(_eo),E(_56)])];}}else{return [0,3,E(_ej),_ek,E(_em),E([0,1,E(E(_dW)),_dX,E(_56),E(_56)])];}}else{var _ex=E(_el);return _ex[0]==0?[0,3,E(_ex[2]),_ex[3],E([0,1,E(_ej),_ek,E(_56),E(_56)]),E([0,1,E(E(_dW)),_dX,E(_56),E(_56)])]:[0,2,E(E(_dW)),_dX,E(_eh),E(_56)];}}else{return [0,1,E(E(_dW)),_dX,E(_56),E(_56)];}}},_ey=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_ez=new T(function(){return B(err(_ey));}),_eA=function(_eB,_eC,_eD,_eE){var _eF=E(_eD);if(!_eF[0]){var _eG=_eF[1],_eH=E(_eE);if(!_eH[0]){var _eI=_eH[1],_eJ=_eH[2],_eK=_eH[3];if(_eI<=(imul(3,_eG)|0)){return [0,(1+_eG|0)+_eI|0,E(E(_eB)),_eC,E(_eF),E(_eH)];}else{var _eL=E(_eH[4]);if(!_eL[0]){var _eM=_eL[1],_eN=_eL[2],_eO=_eL[3],_eP=_eL[4],_eQ=E(_eH[5]);if(!_eQ[0]){var _eR=_eQ[1];if(_eM>=(imul(2,_eR)|0)){var _eS=function(_eT){var _eU=E(_eB),_eV=E(_eL[5]);return _eV[0]==0?[0,(1+_eG|0)+_eI|0,E(_eN),_eO,E([0,(1+_eG|0)+_eT|0,E(_eU),_eC,E(_eF),E(_eP)]),E([0,(1+_eR|0)+_eV[1]|0,E(_eJ),_eK,E(_eV),E(_eQ)])]:[0,(1+_eG|0)+_eI|0,E(_eN),_eO,E([0,(1+_eG|0)+_eT|0,E(_eU),_eC,E(_eF),E(_eP)]),E([0,1+_eR|0,E(_eJ),_eK,E(_56),E(_eQ)])];},_eW=E(_eP);return _eW[0]==0?B(_eS(_eW[1])):B(_eS(0));}else{return [0,(1+_eG|0)+_eI|0,E(_eJ),_eK,E([0,(1+_eG|0)+_eM|0,E(E(_eB)),_eC,E(_eF),E(_eL)]),E(_eQ)];}}else{return E(_ez);}}else{return E(_ez);}}}else{return [0,1+_eG|0,E(E(_eB)),_eC,E(_eF),E(_56)];}}else{var _eX=E(_eE);if(!_eX[0]){var _eY=_eX[1],_eZ=_eX[2],_f0=_eX[3],_f1=_eX[5],_f2=E(_eX[4]);if(!_f2[0]){var _f3=_f2[1],_f4=_f2[2],_f5=_f2[3],_f6=_f2[4],_f7=E(_f1);if(!_f7[0]){var _f8=_f7[1];if(_f3>=(imul(2,_f8)|0)){var _f9=function(_fa){var _fb=E(_eB),_fc=E(_f2[5]);return _fc[0]==0?[0,1+_eY|0,E(_f4),_f5,E([0,1+_fa|0,E(_fb),_eC,E(_56),E(_f6)]),E([0,(1+_f8|0)+_fc[1]|0,E(_eZ),_f0,E(_fc),E(_f7)])]:[0,1+_eY|0,E(_f4),_f5,E([0,1+_fa|0,E(_fb),_eC,E(_56),E(_f6)]),E([0,1+_f8|0,E(_eZ),_f0,E(_56),E(_f7)])];},_fd=E(_f6);return _fd[0]==0?B(_f9(_fd[1])):B(_f9(0));}else{return [0,1+_eY|0,E(_eZ),_f0,E([0,1+_f3|0,E(E(_eB)),_eC,E(_56),E(_f2)]),E(_f7)];}}else{return [0,3,E(_f4),_f5,E([0,1,E(E(_eB)),_eC,E(_56),E(_56)]),E([0,1,E(_eZ),_f0,E(_56),E(_56)])];}}else{var _fe=E(_f1);return _fe[0]==0?[0,3,E(_eZ),_f0,E([0,1,E(E(_eB)),_eC,E(_56),E(_56)]),E(_fe)]:[0,2,E(E(_eB)),_eC,E(_56),E(_eX)];}}else{return [0,1,E(E(_eB)),_eC,E(_56),E(_56)];}}},_ff=function(_fg,_fh,_fi){var _fj=E(_fg),_fk=E(_fi);if(!_fk[0]){var _fl=_fk[2],_fm=_fk[3],_fn=_fk[4],_fo=_fk[5];switch(B(_aN(_fj,_fl))){case 0:return new F(function(){return _dV(_fl,_fm,B(_ff(_fj,_fh,_fn)),_fo);});break;case 1:return [0,_fk[1],E(_fj),_fh,E(_fn),E(_fo)];default:return new F(function(){return _eA(_fl,_fm,_fn,B(_ff(_fj,_fh,_fo)));});}}else{return [0,1,E(_fj),_fh,E(_56),E(_56)];}},_fp=function(_fq,_fr){var _fs=B(A(_fq,[_fr]));return function(_ft,_){var _fu=B(A(_fs,[_])),_fv=_fu;return [0,[0,_2Q,[1,_fv]],_ft];};},_fw=function(_fx,_fy){return new F(function(){return _fp(E(_fx)[2],_fy);});},_fz=2,_fA=1,_fB=4,_fC=3,_fD=0,_fE=new T(function(){return B(unCStr("class"));}),_fF=new T(function(){return B(unCStr("label1"));}),_fG=new T(function(){return B(unCStr("base"));}),_fH=new T(function(){return B(unCStr("Control.Exception.Base"));}),_fI=new T(function(){return B(unCStr("PatternMatchFail"));}),_fJ=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_fG,_fH,_fI],_fK=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_fJ,_X],_fL=function(_fM){return E(_fK);},_fN=function(_fO){var _fP=E(_fO);return new F(function(){return _15(B(_11(_fP[1])),_fL,_fP[2]);});},_fQ=function(_fR){return E(E(_fR)[1]);},_fS=function(_fT,_fU){return new F(function(){return _1q(E(_fT)[1],_fU);});},_fV=function(_fW,_fX){return new F(function(){return _2g(_fS,_fW,_fX);});},_fY=function(_fZ,_g0,_g1){return new F(function(){return _1q(E(_g0)[1],_g1);});},_g2=[0,_fY,_fQ,_fV],_g3=new T(function(){return [0,_fL,_g2,_g4,_fN];}),_g4=function(_g5){return [0,_g3,_g5];},_g6=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_g7=function(_g8,_g9){return new F(function(){return die(new T(function(){return B(A(_g9,[_g8]));}));});},_ga=function(_gb,_gc){var _gd=E(_gc);if(!_gd[0]){return [0,_X,_X];}else{var _ge=_gd[1];if(!B(A(_gb,[_ge]))){return [0,_X,_gd];}else{var _gf=new T(function(){var _gg=B(_ga(_gb,_gd[2]));return [0,_gg[1],_gg[2]];});return [0,[1,_ge,new T(function(){return E(E(_gf)[1]);})],new T(function(){return E(E(_gf)[2]);})];}}},_gh=[0,32],_gi=[0,10],_gj=[1,_gi,_X],_gk=function(_gl){return E(E(_gl)[1])==124?false:true;},_gm=function(_gn,_go){var _gp=B(_ga(_gk,B(unCStr(_gn)))),_gq=_gp[1],_gr=function(_gs,_gt){return new F(function(){return _1q(_gs,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_1q(_go,new T(function(){return B(_1q(_gt,_gj));})));})));}));});},_gu=E(_gp[2]);if(!_gu[0]){return new F(function(){return _gr(_gq,_X);});}else{return E(E(_gu[1])[1])==124?B(_gr(_gq,[1,_gh,_gu[2]])):B(_gr(_gq,_X));}},_gv=function(_gw){return new F(function(){return _g7([0,new T(function(){return B(_gm(_gw,_g6));})],_g4);});},_gx=new T(function(){return B(_gv("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus"));}),_gy=function(_gz,_gA){while(1){var _gB=(function(_gC,_gD){var _gE=E(_gC);switch(_gE[0]){case 0:var _gF=E(_gD);if(!_gF[0]){return [0];}else{_gz=B(A(_gE[1],[_gF[1]]));_gA=_gF[2];return null;}break;case 1:var _gG=B(A(_gE[1],[_gD])),_gH=_gD;_gz=_gG;_gA=_gH;return null;case 2:return [0];case 3:return [1,[0,_gE[1],_gD],new T(function(){return B(_gy(_gE[2],_gD));})];default:return E(_gE[1]);}})(_gz,_gA);if(_gB!=null){return _gB;}}},_gI=function(_gJ,_gK){var _gL=new T(function(){var _gM=E(_gK);if(_gM[0]==3){var _gN=[3,_gM[1],new T(function(){return B(_gI(_gJ,_gM[2]));})];}else{var _gO=E(_gJ);if(_gO[0]==2){var _gP=E(_gM);}else{var _gQ=E(_gM);if(_gQ[0]==2){var _gR=E(_gO);}else{var _gS=new T(function(){var _gT=E(_gQ);if(_gT[0]==4){var _gU=[1,function(_gV){return [4,new T(function(){return B(_1q(B(_gy(_gO,_gV)),_gT[1]));})];}];}else{var _gW=E(_gO);if(_gW[0]==1){var _gX=_gW[1],_gY=E(_gT);if(!_gY[0]){var _gZ=[1,function(_h0){return new F(function(){return _gI(B(A(_gX,[_h0])),_gY);});}];}else{var _gZ=[1,function(_h1){return new F(function(){return _gI(B(A(_gX,[_h1])),new T(function(){return B(A(_gY[1],[_h1]));}));});}];}var _h2=_gZ;}else{var _h3=E(_gT);if(!_h3[0]){var _h4=E(_gx);}else{var _h4=[1,function(_h5){return new F(function(){return _gI(_gW,new T(function(){return B(A(_h3[1],[_h5]));}));});}];}var _h2=_h4;}var _gU=_h2;}return _gU;}),_h6=E(_gO);switch(_h6[0]){case 1:var _h7=E(_gQ);if(_h7[0]==4){var _h8=[1,function(_h9){return [4,new T(function(){return B(_1q(B(_gy(B(A(_h6[1],[_h9])),_h9)),_h7[1]));})];}];}else{var _h8=E(_gS);}var _ha=_h8;break;case 4:var _hb=_h6[1],_hc=E(_gQ);switch(_hc[0]){case 0:var _hd=[1,function(_he){return [4,new T(function(){return B(_1q(_hb,new T(function(){return B(_gy(_hc,_he));})));})];}];break;case 1:var _hd=[1,function(_hf){return [4,new T(function(){return B(_1q(_hb,new T(function(){return B(_gy(B(A(_hc[1],[_hf])),_hf));})));})];}];break;default:var _hd=[4,new T(function(){return B(_1q(_hb,_hc[1]));})];}var _ha=_hd;break;default:var _ha=E(_gS);}var _gR=_ha;}var _gP=_gR;}var _gN=_gP;}return _gN;}),_hg=E(_gJ);switch(_hg[0]){case 0:var _hh=E(_gK);return _hh[0]==0?[0,function(_hi){return new F(function(){return _gI(B(A(_hg[1],[_hi])),new T(function(){return B(A(_hh[1],[_hi]));}));});}]:E(_gL);case 3:return [3,_hg[1],new T(function(){return B(_gI(_hg[2],_gK));})];default:return E(_gL);}},_hj=function(_hk,_hl){return E(_hk)[1]!=E(_hl)[1];},_hm=function(_hn,_ho){return E(_hn)[1]==E(_ho)[1];},_hp=[0,_hm,_hj],_hq=function(_hr){return E(E(_hr)[1]);},_hs=function(_ht,_hu,_hv){while(1){var _hw=E(_hu);if(!_hw[0]){return E(_hv)[0]==0?true:false;}else{var _hx=E(_hv);if(!_hx[0]){return false;}else{if(!B(A(_hq,[_ht,_hw[1],_hx[1]]))){return false;}else{_hu=_hw[2];_hv=_hx[2];continue;}}}}},_hy=function(_hz,_hA,_hB){return !B(_hs(_hz,_hA,_hB))?true:false;},_hC=function(_hD){return [0,function(_hE,_hF){return new F(function(){return _hs(_hD,_hE,_hF);});},function(_hE,_hF){return new F(function(){return _hy(_hD,_hE,_hF);});}];},_hG=new T(function(){return B(_hC(_hp));}),_hH=function(_hI,_hJ){var _hK=E(_hI);switch(_hK[0]){case 0:return [0,function(_hL){return new F(function(){return _hH(B(A(_hK[1],[_hL])),_hJ);});}];case 1:return [1,function(_hM){return new F(function(){return _hH(B(A(_hK[1],[_hM])),_hJ);});}];case 2:return [2];case 3:return new F(function(){return _gI(B(A(_hJ,[_hK[1]])),new T(function(){return B(_hH(_hK[2],_hJ));}));});break;default:var _hN=function(_hO){var _hP=E(_hO);if(!_hP[0]){return [0];}else{var _hQ=E(_hP[1]);return new F(function(){return _1q(B(_gy(B(A(_hJ,[_hQ[1]])),_hQ[2])),new T(function(){return B(_hN(_hP[2]));}));});}},_hR=B(_hN(_hK[1]));return _hR[0]==0?[2]:[4,_hR];}},_hS=[2],_hT=function(_hU){return [3,_hU,_hS];},_hV=function(_hW,_hX){var _hY=E(_hW);if(!_hY){return new F(function(){return A(_hX,[_7]);});}else{return [0,function(_hZ){return E(new T(function(){return B(_hV(_hY-1|0,_hX));}));}];}},_i0=function(_i1,_i2,_i3){return [1,function(_i4){return new F(function(){return A(function(_i5,_i6,_i7){while(1){var _i8=(function(_i9,_ia,_ib){var _ic=E(_i9);switch(_ic[0]){case 0:var _id=E(_ia);if(!_id[0]){return E(_i2);}else{_i5=B(A(_ic[1],[_id[1]]));_i6=_id[2];var _ie=_ib+1|0;_i7=_ie;return null;}break;case 1:var _if=B(A(_ic[1],[_ia])),_ig=_ia,_ie=_ib;_i5=_if;_i6=_ig;_i7=_ie;return null;case 2:return E(_i2);case 3:return function(_ih){return new F(function(){return _hV(_ib,function(_ii){return E(new T(function(){return B(_hH(_ic,_ih));}));});});};default:return function(_8i){return new F(function(){return _hH(_ic,_8i);});};}})(_i5,_i6,_i7);if(_i8!=null){return _i8;}}},[new T(function(){return B(A(_i1,[_hT]));}),_i4,0,_i3]);});}];},_ij=[6],_ik=new T(function(){return B(unCStr("valDig: Bad base"));}),_il=new T(function(){return B(err(_ik));}),_im=function(_in,_io){var _ip=function(_iq,_ir){var _is=E(_iq);if(!_is[0]){return function(_it){return new F(function(){return A(_it,[new T(function(){return B(A(_ir,[_X]));})]);});};}else{var _iu=E(_is[1])[1],_iv=function(_iw){return function(_ix){return [0,function(_iy){return E(new T(function(){return B(A(new T(function(){return B(_ip(_is[2],function(_iz){return new F(function(){return A(_ir,[[1,_iw,_iz]]);});}));}),[_ix]));}));}];};};switch(E(E(_in)[1])){case 8:if(48>_iu){return function(_iA){return new F(function(){return A(_iA,[new T(function(){return B(A(_ir,[_X]));})]);});};}else{if(_iu>55){return function(_iB){return new F(function(){return A(_iB,[new T(function(){return B(A(_ir,[_X]));})]);});};}else{return new F(function(){return _iv([0,_iu-48|0]);});}}break;case 10:if(48>_iu){return function(_iC){return new F(function(){return A(_iC,[new T(function(){return B(A(_ir,[_X]));})]);});};}else{if(_iu>57){return function(_iD){return new F(function(){return A(_iD,[new T(function(){return B(A(_ir,[_X]));})]);});};}else{return new F(function(){return _iv([0,_iu-48|0]);});}}break;case 16:var _iE=new T(function(){if(97>_iu){if(65>_iu){var _iF=[0];}else{if(_iu>70){var _iG=[0];}else{var _iG=[1,[0,(_iu-65|0)+10|0]];}var _iF=_iG;}var _iH=_iF;}else{if(_iu>102){if(65>_iu){var _iI=[0];}else{if(_iu>70){var _iJ=[0];}else{var _iJ=[1,[0,(_iu-65|0)+10|0]];}var _iI=_iJ;}var _iK=_iI;}else{var _iK=[1,[0,(_iu-97|0)+10|0]];}var _iH=_iK;}return _iH;});if(48>_iu){var _iL=E(_iE);if(!_iL[0]){return function(_iM){return new F(function(){return A(_iM,[new T(function(){return B(A(_ir,[_X]));})]);});};}else{return new F(function(){return _iv(_iL[1]);});}}else{if(_iu>57){var _iN=E(_iE);if(!_iN[0]){return function(_iO){return new F(function(){return A(_iO,[new T(function(){return B(A(_ir,[_X]));})]);});};}else{return new F(function(){return _iv(_iN[1]);});}}else{return new F(function(){return _iv([0,_iu-48|0]);});}}break;default:return E(_il);}}};return [1,function(_iP){return new F(function(){return A(_ip,[_iP,_e,function(_iQ){var _iR=E(_iQ);return _iR[0]==0?[2]:B(A(_io,[_iR]));}]);});}];},_iS=[0,10],_iT=[0,1],_iU=[0,2147483647],_iV=function(_iW,_iX){while(1){var _iY=E(_iW);if(!_iY[0]){var _iZ=_iY[1],_j0=E(_iX);if(!_j0[0]){var _j1=_j0[1],_j2=addC(_iZ,_j1);if(!E(_j2[2])){return [0,_j2[1]];}else{_iW=[1,I_fromInt(_iZ)];_iX=[1,I_fromInt(_j1)];continue;}}else{_iW=[1,I_fromInt(_iZ)];_iX=_j0;continue;}}else{var _j3=E(_iX);if(!_j3[0]){_iW=_iY;_iX=[1,I_fromInt(_j3[1])];continue;}else{return [1,I_add(_iY[1],_j3[1])];}}}},_j4=new T(function(){return B(_iV(_iU,_iT));}),_j5=function(_j6){var _j7=E(_j6);if(!_j7[0]){var _j8=E(_j7[1]);return _j8==(-2147483648)?E(_j4):[0, -_j8];}else{return [1,I_negate(_j7[1])];}},_j9=[0,10],_ja=[0,0],_jb=function(_jc){return [0,_jc];},_jd=function(_je,_jf){while(1){var _jg=E(_je);if(!_jg[0]){var _jh=_jg[1],_ji=E(_jf);if(!_ji[0]){var _jj=_ji[1];if(!(imul(_jh,_jj)|0)){return [0,imul(_jh,_jj)|0];}else{_je=[1,I_fromInt(_jh)];_jf=[1,I_fromInt(_jj)];continue;}}else{_je=[1,I_fromInt(_jh)];_jf=_ji;continue;}}else{var _jk=E(_jf);if(!_jk[0]){_je=_jg;_jf=[1,I_fromInt(_jk[1])];continue;}else{return [1,I_mul(_jg[1],_jk[1])];}}}},_jl=function(_jm,_jn,_jo){while(1){var _jp=E(_jo);if(!_jp[0]){return E(_jn);}else{var _jq=B(_iV(B(_jd(_jn,_jm)),B(_jb(E(_jp[1])[1]))));_jo=_jp[2];_jn=_jq;continue;}}},_jr=function(_js){var _jt=new T(function(){return B(_gI(B(_gI([0,function(_ju){if(E(E(_ju)[1])==45){return new F(function(){return _im(_iS,function(_jv){return new F(function(){return A(_js,[[1,new T(function(){return B(_j5(B(_jl(_j9,_ja,_jv))));})]]);});});});}else{return [2];}}],[0,function(_jw){if(E(E(_jw)[1])==43){return new F(function(){return _im(_iS,function(_jx){return new F(function(){return A(_js,[[1,new T(function(){return B(_jl(_j9,_ja,_jx));})]]);});});});}else{return [2];}}])),new T(function(){return B(_im(_iS,function(_jy){return new F(function(){return A(_js,[[1,new T(function(){return B(_jl(_j9,_ja,_jy));})]]);});}));})));});return new F(function(){return _gI([0,function(_jz){return E(E(_jz)[1])==101?E(_jt):[2];}],[0,function(_jA){return E(E(_jA)[1])==69?E(_jt):[2];}]);});},_jB=function(_jC){return new F(function(){return A(_jC,[_2A]);});},_jD=function(_jE){return new F(function(){return A(_jE,[_2A]);});},_jF=function(_jG){return [0,function(_jH){return E(E(_jH)[1])==46?E(new T(function(){return B(_im(_iS,function(_jI){return new F(function(){return A(_jG,[[1,_jI]]);});}));})):[2];}];},_jJ=function(_jK){return new F(function(){return _im(_iS,function(_jL){return new F(function(){return _i0(_jF,_jB,function(_jM){return new F(function(){return _i0(_jr,_jD,function(_jN){return new F(function(){return A(_jK,[[5,[1,_jL,_jM,_jN]]]);});});});});});});});},_jO=function(_jP,_jQ,_jR){while(1){var _jS=E(_jR);if(!_jS[0]){return false;}else{if(!B(A(_hq,[_jP,_jQ,_jS[1]]))){_jR=_jS[2];continue;}else{return true;}}}},_jT=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_jU=function(_jV){return new F(function(){return _jO(_hp,_jV,_jT);});},_jW=[0,8],_jX=[0,16],_jY=function(_jZ){return [0,function(_k0){return E(E(_k0)[1])==48?E([0,function(_k1){switch(E(E(_k1)[1])){case 79:return E(new T(function(){return B(_im(_jW,function(_k2){return new F(function(){return A(_jZ,[[5,[0,_jW,_k2]]]);});}));}));case 88:return E(new T(function(){return B(_im(_jX,function(_k3){return new F(function(){return A(_jZ,[[5,[0,_jX,_k3]]]);});}));}));case 111:return E(new T(function(){return B(_im(_jW,function(_k4){return new F(function(){return A(_jZ,[[5,[0,_jW,_k4]]]);});}));}));case 120:return E(new T(function(){return B(_im(_jX,function(_k5){return new F(function(){return A(_jZ,[[5,[0,_jX,_k5]]]);});}));}));default:return [2];}}]):[2];}];},_k6=function(_k7){return [0,function(_k8){switch(E(E(_k8)[1])){case 79:return E(new T(function(){return B(A(_k7,[_jW]));}));case 88:return E(new T(function(){return B(A(_k7,[_jX]));}));case 111:return E(new T(function(){return B(A(_k7,[_jW]));}));case 120:return E(new T(function(){return B(A(_k7,[_jX]));}));default:return [2];}}];},_k9=function(_ka){return new F(function(){return A(_ka,[_iS]);});},_kb=function(_kc){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_3x(9,_kc,_X));}))));});},_kd=function(_ke){var _kf=E(_ke);return _kf[0]==0?E(_kf[1]):I_toInt(_kf[1]);},_kg=function(_kh,_ki){var _kj=E(_kh);if(!_kj[0]){var _kk=_kj[1],_kl=E(_ki);return _kl[0]==0?_kk<=_kl[1]:I_compareInt(_kl[1],_kk)>=0;}else{var _km=_kj[1],_kn=E(_ki);return _kn[0]==0?I_compareInt(_km,_kn[1])<=0:I_compare(_km,_kn[1])<=0;}},_ko=function(_kp){return [2];},_kq=function(_kr){var _ks=E(_kr);if(!_ks[0]){return E(_ko);}else{var _kt=_ks[1],_ku=E(_ks[2]);return _ku[0]==0?E(_kt):function(_kv){return new F(function(){return _gI(B(A(_kt,[_kv])),new T(function(){return B(A(new T(function(){return B(_kq(_ku));}),[_kv]));}));});};}},_kw=new T(function(){return B(unCStr("NUL"));}),_kx=function(_ky){return [2];},_kz=function(_kA){return new F(function(){return _kx(_kA);});},_kB=function(_kC,_kD){var _kE=function(_kF,_kG){var _kH=E(_kF);if(!_kH[0]){return function(_kI){return new F(function(){return A(_kI,[_kC]);});};}else{var _kJ=E(_kG);return _kJ[0]==0?E(_kx):E(_kH[1])[1]!=E(_kJ[1])[1]?E(_kz):function(_kK){return [0,function(_kL){return E(new T(function(){return B(A(new T(function(){return B(_kE(_kH[2],_kJ[2]));}),[_kK]));}));}];};}};return [1,function(_kM){return new F(function(){return A(_kE,[_kC,_kM,_kD]);});}];},_kN=[0,0],_kO=function(_kP){return new F(function(){return _kB(_kw,function(_kQ){return E(new T(function(){return B(A(_kP,[_kN]));}));});});},_kR=new T(function(){return B(unCStr("STX"));}),_kS=[0,2],_kT=function(_kU){return new F(function(){return _kB(_kR,function(_kV){return E(new T(function(){return B(A(_kU,[_kS]));}));});});},_kW=new T(function(){return B(unCStr("ETX"));}),_kX=[0,3],_kY=function(_kZ){return new F(function(){return _kB(_kW,function(_l0){return E(new T(function(){return B(A(_kZ,[_kX]));}));});});},_l1=new T(function(){return B(unCStr("EOT"));}),_l2=[0,4],_l3=function(_l4){return new F(function(){return _kB(_l1,function(_l5){return E(new T(function(){return B(A(_l4,[_l2]));}));});});},_l6=new T(function(){return B(unCStr("ENQ"));}),_l7=[0,5],_l8=function(_l9){return new F(function(){return _kB(_l6,function(_la){return E(new T(function(){return B(A(_l9,[_l7]));}));});});},_lb=new T(function(){return B(unCStr("ACK"));}),_lc=[0,6],_ld=function(_le){return new F(function(){return _kB(_lb,function(_lf){return E(new T(function(){return B(A(_le,[_lc]));}));});});},_lg=new T(function(){return B(unCStr("BEL"));}),_lh=[0,7],_li=function(_lj){return new F(function(){return _kB(_lg,function(_lk){return E(new T(function(){return B(A(_lj,[_lh]));}));});});},_ll=new T(function(){return B(unCStr("BS"));}),_lm=[0,8],_ln=function(_lo){return new F(function(){return _kB(_ll,function(_lp){return E(new T(function(){return B(A(_lo,[_lm]));}));});});},_lq=new T(function(){return B(unCStr("HT"));}),_lr=[0,9],_ls=function(_lt){return new F(function(){return _kB(_lq,function(_lu){return E(new T(function(){return B(A(_lt,[_lr]));}));});});},_lv=new T(function(){return B(unCStr("LF"));}),_lw=[0,10],_lx=function(_ly){return new F(function(){return _kB(_lv,function(_lz){return E(new T(function(){return B(A(_ly,[_lw]));}));});});},_lA=new T(function(){return B(unCStr("VT"));}),_lB=[0,11],_lC=function(_lD){return new F(function(){return _kB(_lA,function(_lE){return E(new T(function(){return B(A(_lD,[_lB]));}));});});},_lF=new T(function(){return B(unCStr("FF"));}),_lG=[0,12],_lH=function(_lI){return new F(function(){return _kB(_lF,function(_lJ){return E(new T(function(){return B(A(_lI,[_lG]));}));});});},_lK=new T(function(){return B(unCStr("CR"));}),_lL=[0,13],_lM=function(_lN){return new F(function(){return _kB(_lK,function(_lO){return E(new T(function(){return B(A(_lN,[_lL]));}));});});},_lP=new T(function(){return B(unCStr("SI"));}),_lQ=[0,15],_lR=function(_lS){return new F(function(){return _kB(_lP,function(_lT){return E(new T(function(){return B(A(_lS,[_lQ]));}));});});},_lU=new T(function(){return B(unCStr("DLE"));}),_lV=[0,16],_lW=function(_lX){return new F(function(){return _kB(_lU,function(_lY){return E(new T(function(){return B(A(_lX,[_lV]));}));});});},_lZ=new T(function(){return B(unCStr("DC1"));}),_m0=[0,17],_m1=function(_m2){return new F(function(){return _kB(_lZ,function(_m3){return E(new T(function(){return B(A(_m2,[_m0]));}));});});},_m4=new T(function(){return B(unCStr("DC2"));}),_m5=[0,18],_m6=function(_m7){return new F(function(){return _kB(_m4,function(_m8){return E(new T(function(){return B(A(_m7,[_m5]));}));});});},_m9=new T(function(){return B(unCStr("DC3"));}),_ma=[0,19],_mb=function(_mc){return new F(function(){return _kB(_m9,function(_md){return E(new T(function(){return B(A(_mc,[_ma]));}));});});},_me=new T(function(){return B(unCStr("DC4"));}),_mf=[0,20],_mg=function(_mh){return new F(function(){return _kB(_me,function(_mi){return E(new T(function(){return B(A(_mh,[_mf]));}));});});},_mj=new T(function(){return B(unCStr("NAK"));}),_mk=[0,21],_ml=function(_mm){return new F(function(){return _kB(_mj,function(_mn){return E(new T(function(){return B(A(_mm,[_mk]));}));});});},_mo=new T(function(){return B(unCStr("SYN"));}),_mp=[0,22],_mq=function(_mr){return new F(function(){return _kB(_mo,function(_ms){return E(new T(function(){return B(A(_mr,[_mp]));}));});});},_mt=new T(function(){return B(unCStr("ETB"));}),_mu=[0,23],_mv=function(_mw){return new F(function(){return _kB(_mt,function(_mx){return E(new T(function(){return B(A(_mw,[_mu]));}));});});},_my=new T(function(){return B(unCStr("CAN"));}),_mz=[0,24],_mA=function(_mB){return new F(function(){return _kB(_my,function(_mC){return E(new T(function(){return B(A(_mB,[_mz]));}));});});},_mD=new T(function(){return B(unCStr("EM"));}),_mE=[0,25],_mF=function(_mG){return new F(function(){return _kB(_mD,function(_mH){return E(new T(function(){return B(A(_mG,[_mE]));}));});});},_mI=new T(function(){return B(unCStr("SUB"));}),_mJ=[0,26],_mK=function(_mL){return new F(function(){return _kB(_mI,function(_mM){return E(new T(function(){return B(A(_mL,[_mJ]));}));});});},_mN=new T(function(){return B(unCStr("ESC"));}),_mO=[0,27],_mP=function(_mQ){return new F(function(){return _kB(_mN,function(_mR){return E(new T(function(){return B(A(_mQ,[_mO]));}));});});},_mS=new T(function(){return B(unCStr("FS"));}),_mT=[0,28],_mU=function(_mV){return new F(function(){return _kB(_mS,function(_mW){return E(new T(function(){return B(A(_mV,[_mT]));}));});});},_mX=new T(function(){return B(unCStr("GS"));}),_mY=[0,29],_mZ=function(_n0){return new F(function(){return _kB(_mX,function(_n1){return E(new T(function(){return B(A(_n0,[_mY]));}));});});},_n2=new T(function(){return B(unCStr("RS"));}),_n3=[0,30],_n4=function(_n5){return new F(function(){return _kB(_n2,function(_n6){return E(new T(function(){return B(A(_n5,[_n3]));}));});});},_n7=new T(function(){return B(unCStr("US"));}),_n8=[0,31],_n9=function(_na){return new F(function(){return _kB(_n7,function(_nb){return E(new T(function(){return B(A(_na,[_n8]));}));});});},_nc=new T(function(){return B(unCStr("SP"));}),_nd=[0,32],_ne=function(_nf){return new F(function(){return _kB(_nc,function(_ng){return E(new T(function(){return B(A(_nf,[_nd]));}));});});},_nh=new T(function(){return B(unCStr("DEL"));}),_ni=[0,127],_nj=function(_nk){return new F(function(){return _kB(_nh,function(_nl){return E(new T(function(){return B(A(_nk,[_ni]));}));});});},_nm=[1,_nj,_X],_nn=[1,_ne,_nm],_no=[1,_n9,_nn],_np=[1,_n4,_no],_nq=[1,_mZ,_np],_nr=[1,_mU,_nq],_ns=[1,_mP,_nr],_nt=[1,_mK,_ns],_nu=[1,_mF,_nt],_nv=[1,_mA,_nu],_nw=[1,_mv,_nv],_nx=[1,_mq,_nw],_ny=[1,_ml,_nx],_nz=[1,_mg,_ny],_nA=[1,_mb,_nz],_nB=[1,_m6,_nA],_nC=[1,_m1,_nB],_nD=[1,_lW,_nC],_nE=[1,_lR,_nD],_nF=[1,_lM,_nE],_nG=[1,_lH,_nF],_nH=[1,_lC,_nG],_nI=[1,_lx,_nH],_nJ=[1,_ls,_nI],_nK=[1,_ln,_nJ],_nL=[1,_li,_nK],_nM=[1,_ld,_nL],_nN=[1,_l8,_nM],_nO=[1,_l3,_nN],_nP=[1,_kY,_nO],_nQ=[1,_kT,_nP],_nR=[1,_kO,_nQ],_nS=new T(function(){return B(unCStr("SOH"));}),_nT=[0,1],_nU=function(_nV){return new F(function(){return _kB(_nS,function(_nW){return E(new T(function(){return B(A(_nV,[_nT]));}));});});},_nX=new T(function(){return B(unCStr("SO"));}),_nY=[0,14],_nZ=function(_o0){return new F(function(){return _kB(_nX,function(_o1){return E(new T(function(){return B(A(_o0,[_nY]));}));});});},_o2=function(_o3){return new F(function(){return _i0(_nU,_nZ,_o3);});},_o4=[1,_o2,_nR],_o5=new T(function(){return B(_kq(_o4));}),_o6=[0,1114111],_o7=[0,34],_o8=[0,_o7,_6H],_o9=[0,39],_oa=[0,_o9,_6H],_ob=[0,92],_oc=[0,_ob,_6H],_od=[0,_lh,_6H],_oe=[0,_lm,_6H],_of=[0,_lG,_6H],_og=[0,_lw,_6H],_oh=[0,_lL,_6H],_oi=[0,_lr,_6H],_oj=[0,_lB,_6H],_ok=[0,_kN,_6H],_ol=[0,_nT,_6H],_om=[0,_kS,_6H],_on=[0,_kX,_6H],_oo=[0,_l2,_6H],_op=[0,_l7,_6H],_oq=[0,_lc,_6H],_or=[0,_lh,_6H],_os=[0,_lm,_6H],_ot=[0,_lr,_6H],_ou=[0,_lw,_6H],_ov=[0,_lB,_6H],_ow=[0,_lG,_6H],_ox=[0,_lL,_6H],_oy=[0,_nY,_6H],_oz=[0,_lQ,_6H],_oA=[0,_lV,_6H],_oB=[0,_m0,_6H],_oC=[0,_m5,_6H],_oD=[0,_ma,_6H],_oE=[0,_mf,_6H],_oF=[0,_mk,_6H],_oG=[0,_mp,_6H],_oH=[0,_mu,_6H],_oI=[0,_mz,_6H],_oJ=[0,_mE,_6H],_oK=[0,_mJ,_6H],_oL=[0,_mO,_6H],_oM=[0,_mT,_6H],_oN=[0,_mY,_6H],_oO=[0,_n3,_6H],_oP=[0,_n8,_6H],_oQ=function(_oR){return new F(function(){return _gI([0,function(_oS){switch(E(E(_oS)[1])){case 34:return E(new T(function(){return B(A(_oR,[_o8]));}));case 39:return E(new T(function(){return B(A(_oR,[_oa]));}));case 92:return E(new T(function(){return B(A(_oR,[_oc]));}));case 97:return E(new T(function(){return B(A(_oR,[_od]));}));case 98:return E(new T(function(){return B(A(_oR,[_oe]));}));case 102:return E(new T(function(){return B(A(_oR,[_of]));}));case 110:return E(new T(function(){return B(A(_oR,[_og]));}));case 114:return E(new T(function(){return B(A(_oR,[_oh]));}));case 116:return E(new T(function(){return B(A(_oR,[_oi]));}));case 118:return E(new T(function(){return B(A(_oR,[_oj]));}));default:return [2];}}],new T(function(){return B(_gI(B(_i0(_k6,_k9,function(_oT){return new F(function(){return _im(_oT,function(_oU){var _oV=B(_jl(new T(function(){return B(_jb(E(_oT)[1]));}),_ja,_oU));return !B(_kg(_oV,_o6))?[2]:B(A(_oR,[[0,new T(function(){var _oW=B(_kd(_oV));if(_oW>>>0>1114111){var _oX=B(_kb(_oW));}else{var _oX=[0,_oW];}var _oY=_oX,_oZ=_oY;return _oZ;}),_6H]]));});});})),new T(function(){return B(_gI([0,function(_p0){return E(E(_p0)[1])==94?E([0,function(_p1){switch(E(E(_p1)[1])){case 64:return E(new T(function(){return B(A(_oR,[_ok]));}));case 65:return E(new T(function(){return B(A(_oR,[_ol]));}));case 66:return E(new T(function(){return B(A(_oR,[_om]));}));case 67:return E(new T(function(){return B(A(_oR,[_on]));}));case 68:return E(new T(function(){return B(A(_oR,[_oo]));}));case 69:return E(new T(function(){return B(A(_oR,[_op]));}));case 70:return E(new T(function(){return B(A(_oR,[_oq]));}));case 71:return E(new T(function(){return B(A(_oR,[_or]));}));case 72:return E(new T(function(){return B(A(_oR,[_os]));}));case 73:return E(new T(function(){return B(A(_oR,[_ot]));}));case 74:return E(new T(function(){return B(A(_oR,[_ou]));}));case 75:return E(new T(function(){return B(A(_oR,[_ov]));}));case 76:return E(new T(function(){return B(A(_oR,[_ow]));}));case 77:return E(new T(function(){return B(A(_oR,[_ox]));}));case 78:return E(new T(function(){return B(A(_oR,[_oy]));}));case 79:return E(new T(function(){return B(A(_oR,[_oz]));}));case 80:return E(new T(function(){return B(A(_oR,[_oA]));}));case 81:return E(new T(function(){return B(A(_oR,[_oB]));}));case 82:return E(new T(function(){return B(A(_oR,[_oC]));}));case 83:return E(new T(function(){return B(A(_oR,[_oD]));}));case 84:return E(new T(function(){return B(A(_oR,[_oE]));}));case 85:return E(new T(function(){return B(A(_oR,[_oF]));}));case 86:return E(new T(function(){return B(A(_oR,[_oG]));}));case 87:return E(new T(function(){return B(A(_oR,[_oH]));}));case 88:return E(new T(function(){return B(A(_oR,[_oI]));}));case 89:return E(new T(function(){return B(A(_oR,[_oJ]));}));case 90:return E(new T(function(){return B(A(_oR,[_oK]));}));case 91:return E(new T(function(){return B(A(_oR,[_oL]));}));case 92:return E(new T(function(){return B(A(_oR,[_oM]));}));case 93:return E(new T(function(){return B(A(_oR,[_oN]));}));case 94:return E(new T(function(){return B(A(_oR,[_oO]));}));case 95:return E(new T(function(){return B(A(_oR,[_oP]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_o5,[function(_p2){return new F(function(){return A(_oR,[[0,_p2,_6H]]);});}]));})));})));}));});},_p3=function(_p4){return new F(function(){return A(_p4,[_7]);});},_p5=function(_p6){var _p7=E(_p6);if(!_p7[0]){return E(_p3);}else{var _p8=_p7[2],_p9=E(E(_p7[1])[1]);switch(_p9){case 9:return function(_pa){return [0,function(_pb){return E(new T(function(){return B(A(new T(function(){return B(_p5(_p8));}),[_pa]));}));}];};case 10:return function(_pc){return [0,function(_pd){return E(new T(function(){return B(A(new T(function(){return B(_p5(_p8));}),[_pc]));}));}];};case 11:return function(_pe){return [0,function(_pf){return E(new T(function(){return B(A(new T(function(){return B(_p5(_p8));}),[_pe]));}));}];};case 12:return function(_pg){return [0,function(_ph){return E(new T(function(){return B(A(new T(function(){return B(_p5(_p8));}),[_pg]));}));}];};case 13:return function(_pi){return [0,function(_pj){return E(new T(function(){return B(A(new T(function(){return B(_p5(_p8));}),[_pi]));}));}];};case 32:return function(_pk){return [0,function(_pl){return E(new T(function(){return B(A(new T(function(){return B(_p5(_p8));}),[_pk]));}));}];};case 160:return function(_pm){return [0,function(_pn){return E(new T(function(){return B(A(new T(function(){return B(_p5(_p8));}),[_pm]));}));}];};default:var _po=u_iswspace(_p9),_pp=_po;return E(_pp)==0?E(_p3):function(_pq){return [0,function(_pr){return E(new T(function(){return B(A(new T(function(){return B(_p5(_p8));}),[_pq]));}));}];};}}},_ps=function(_pt){var _pu=new T(function(){return B(_ps(_pt));}),_pv=[1,function(_pw){return new F(function(){return A(_p5,[_pw,function(_px){return E([0,function(_py){return E(E(_py)[1])==92?E(_pu):[2];}]);}]);});}];return new F(function(){return _gI([0,function(_pz){return E(E(_pz)[1])==92?E([0,function(_pA){var _pB=E(E(_pA)[1]);switch(_pB){case 9:return E(_pv);case 10:return E(_pv);case 11:return E(_pv);case 12:return E(_pv);case 13:return E(_pv);case 32:return E(_pv);case 38:return E(_pu);case 160:return E(_pv);default:var _pC=u_iswspace(_pB),_pD=_pC;return E(_pD)==0?[2]:E(_pv);}}]):[2];}],[0,function(_pE){var _pF=E(_pE);return E(_pF[1])==92?E(new T(function(){return B(_oQ(_pt));})):B(A(_pt,[[0,_pF,_54]]));}]);});},_pG=function(_pH,_pI){return new F(function(){return _ps(function(_pJ){var _pK=E(_pJ),_pL=E(_pK[1]);if(E(_pL[1])==34){if(!E(_pK[2])){return E(new T(function(){return B(A(_pI,[[1,new T(function(){return B(A(_pH,[_X]));})]]));}));}else{return new F(function(){return _pG(function(_pM){return new F(function(){return A(_pH,[[1,_pL,_pM]]);});},_pI);});}}else{return new F(function(){return _pG(function(_pN){return new F(function(){return A(_pH,[[1,_pL,_pN]]);});},_pI);});}});});},_pO=new T(function(){return B(unCStr("_\'"));}),_pP=function(_pQ){var _pR=u_iswalnum(_pQ),_pS=_pR;return E(_pS)==0?B(_jO(_hp,[0,_pQ],_pO)):true;},_pT=function(_pU){return new F(function(){return _pP(E(_pU)[1]);});},_pV=new T(function(){return B(unCStr(",;()[]{}`"));}),_pW=function(_pX){return new F(function(){return A(_pX,[_X]);});},_pY=function(_pZ,_q0){var _q1=function(_q2){var _q3=E(_q2);if(!_q3[0]){return E(_pW);}else{var _q4=_q3[1];return !B(A(_pZ,[_q4]))?E(_pW):function(_q5){return [0,function(_q6){return E(new T(function(){return B(A(new T(function(){return B(_q1(_q3[2]));}),[function(_q7){return new F(function(){return A(_q5,[[1,_q4,_q7]]);});}]));}));}];};}};return [1,function(_q8){return new F(function(){return A(_q1,[_q8,_q0]);});}];},_q9=new T(function(){return B(unCStr(".."));}),_qa=new T(function(){return B(unCStr("::"));}),_qb=new T(function(){return B(unCStr("->"));}),_qc=[0,64],_qd=[1,_qc,_X],_qe=[0,126],_qf=[1,_qe,_X],_qg=new T(function(){return B(unCStr("=>"));}),_qh=[1,_qg,_X],_qi=[1,_qf,_qh],_qj=[1,_qd,_qi],_qk=[1,_qb,_qj],_ql=new T(function(){return B(unCStr("<-"));}),_qm=[1,_ql,_qk],_qn=[0,124],_qo=[1,_qn,_X],_qp=[1,_qo,_qm],_qq=[1,_ob,_X],_qr=[1,_qq,_qp],_qs=[0,61],_qt=[1,_qs,_X],_qu=[1,_qt,_qr],_qv=[1,_qa,_qu],_qw=[1,_q9,_qv],_qx=function(_qy){return new F(function(){return _gI([1,function(_qz){return E(_qz)[0]==0?E(new T(function(){return B(A(_qy,[_ij]));})):[2];}],new T(function(){return B(_gI([0,function(_qA){return E(E(_qA)[1])==39?E([0,function(_qB){var _qC=E(_qB);switch(E(_qC[1])){case 39:return [2];case 92:return E(new T(function(){return B(_oQ(function(_qD){var _qE=E(_qD);return new F(function(){return (function(_qF,_qG){var _qH=new T(function(){return B(A(_qy,[[0,_qF]]));});return !E(_qG)?E(E(_qF)[1])==39?[2]:[0,function(_qI){return E(E(_qI)[1])==39?E(_qH):[2];}]:[0,function(_qJ){return E(E(_qJ)[1])==39?E(_qH):[2];}];})(_qE[1],_qE[2]);});}));}));default:return [0,function(_qK){return E(E(_qK)[1])==39?E(new T(function(){return B(A(_qy,[[0,_qC]]));})):[2];}];}}]):[2];}],new T(function(){return B(_gI([0,function(_qL){return E(E(_qL)[1])==34?E(new T(function(){return B(_pG(_e,_qy));})):[2];}],new T(function(){return B(_gI([0,function(_qM){return !B(_jO(_hp,_qM,_pV))?[2]:B(A(_qy,[[2,[1,_qM,_X]]]));}],new T(function(){return B(_gI([0,function(_qN){if(!B(_jO(_hp,_qN,_jT))){return [2];}else{return new F(function(){return _pY(_jU,function(_qO){var _qP=[1,_qN,_qO];return !B(_jO(_hG,_qP,_qw))?B(A(_qy,[[4,_qP]])):B(A(_qy,[[2,_qP]]));});});}}],new T(function(){return B(_gI([0,function(_qQ){var _qR=E(_qQ),_qS=_qR[1],_qT=u_iswalpha(_qS),_qU=_qT;if(!E(_qU)){if(E(_qS)==95){return new F(function(){return _pY(_pT,function(_qV){return new F(function(){return A(_qy,[[3,[1,_qR,_qV]]]);});});});}else{return [2];}}else{return new F(function(){return _pY(_pT,function(_qW){return new F(function(){return A(_qy,[[3,[1,_qR,_qW]]]);});});});}}],new T(function(){return B(_i0(_jY,_jJ,_qy));})));})));})));})));})));}));});},_qX=function(_qY){return [1,function(_qZ){return new F(function(){return A(_p5,[_qZ,function(_r0){return E(new T(function(){return B(_qx(_qY));}));}]);});}];},_r1=[0,0],_r2=function(_r3,_r4){return new F(function(){return _qX(function(_r5){var _r6=E(_r5);if(_r6[0]==2){var _r7=E(_r6[1]);return _r7[0]==0?[2]:E(E(_r7[1])[1])==40?E(_r7[2])[0]==0?E(new T(function(){return B(A(_r3,[_r1,function(_r8){return new F(function(){return _qX(function(_r9){var _ra=E(_r9);if(_ra[0]==2){var _rb=E(_ra[1]);return _rb[0]==0?[2]:E(E(_rb[1])[1])==41?E(_rb[2])[0]==0?E(new T(function(){return B(A(_r4,[_r8]));})):[2]:[2];}else{return [2];}});});}]));})):[2]:[2];}else{return [2];}});});},_rc=function(_rd,_re,_rf){var _rg=function(_rh,_ri){return new F(function(){return _gI(B(_qX(function(_rj){var _rk=E(_rj);if(_rk[0]==4){var _rl=E(_rk[1]);if(!_rl[0]){return new F(function(){return A(_rd,[_rk,_rh,_ri]);});}else{return E(E(_rl[1])[1])==45?E(_rl[2])[0]==0?E([1,function(_rm){return new F(function(){return A(_p5,[_rm,function(_rn){return E(new T(function(){return B(_qx(function(_ro){return new F(function(){return A(_rd,[_ro,_rh,function(_rp){return new F(function(){return A(_ri,[new T(function(){return [0, -E(_rp)[1]];})]);});}]);});}));}));}]);});}]):B(A(_rd,[_rk,_rh,_ri])):B(A(_rd,[_rk,_rh,_ri]));}}else{return new F(function(){return A(_rd,[_rk,_rh,_ri]);});}})),new T(function(){return B(_r2(_rg,_ri));}));});};return new F(function(){return _rg(_re,_rf);});},_rq=new T(function(){return B(unCStr("NaN"));}),_rr=new T(function(){return B(unCStr("Infinity"));}),_rs=function(_rt,_ru){return [2];},_rv=function(_rw,_rx){return new F(function(){return _rs(_rw,_rx);});},_ry=new T(function(){return [0,1/0];}),_rz=function(_rA,_rB){return new F(function(){return A(_rB,[_ry]);});},_rC=function(_rD,_rE){return new F(function(){return A(_rE,[_ry]);});},_rF=new T(function(){return [0,0/0];}),_rG=function(_rH,_rI){return new F(function(){return A(_rI,[_rF]);});},_rJ=[0,1024],_rK=[0,-1021],_rL=new T(function(){return [0,0/0];}),_rM=new T(function(){return [0,-1/0];}),_rN=new T(function(){return [0,1/0];}),_rO=[0,0],_rP=function(_rQ,_rR){while(1){var _rS=E(_rQ);if(!_rS[0]){_rQ=[1,I_fromInt(_rS[1])];continue;}else{var _rT=E(_rR);if(!_rT[0]){_rQ=_rS;_rR=[1,I_fromInt(_rT[1])];continue;}else{return new F(function(){return I_fromRat(_rS[1],_rT[1]);});}}}},_rU=function(_rV,_rW){var _rX=E(_rV);if(!_rX[0]){var _rY=_rX[1],_rZ=E(_rW);return _rZ[0]==0?_rY==_rZ[1]:I_compareInt(_rZ[1],_rY)==0?true:false;}else{var _s0=_rX[1],_s1=E(_rW);return _s1[0]==0?I_compareInt(_s0,_s1[1])==0?true:false:I_compare(_s0,_s1[1])==0?true:false;}},_s2=function(_s3,_s4){var _s5=E(_s3);if(!_s5[0]){var _s6=_s5[1],_s7=E(_s4);return _s7[0]==0?_s6<_s7[1]:I_compareInt(_s7[1],_s6)>0;}else{var _s8=_s5[1],_s9=E(_s4);return _s9[0]==0?I_compareInt(_s8,_s9[1])<0:I_compare(_s8,_s9[1])<0;}},_sa=function(_sb,_sc){return !B(_rU(_sc,_rO))?[0,B(_rP(_sb,_sc))]:!B(_rU(_sb,_rO))?!B(_s2(_sb,_rO))?E(_rN):E(_rM):E(_rL);},_sd=function(_se,_sf){while(1){var _sg=E(_se);if(!_sg[0]){return E(_sf);}else{_se=_sg[2];var _sh=_sf+1|0;_sf=_sh;continue;}}},_si=[0,1],_sj=new T(function(){return B(unCStr("base"));}),_sk=new T(function(){return B(unCStr("GHC.Exception"));}),_sl=new T(function(){return B(unCStr("ArithException"));}),_sm=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_sj,_sk,_sl],_sn=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_sm,_X],_so=function(_sp){return E(_sn);},_sq=function(_sr){var _ss=E(_sr);return new F(function(){return _15(B(_11(_ss[1])),_so,_ss[2]);});},_st=new T(function(){return B(unCStr("arithmetic underflow"));}),_su=new T(function(){return B(unCStr("arithmetic overflow"));}),_sv=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_sw=new T(function(){return B(unCStr("denormal"));}),_sx=new T(function(){return B(unCStr("divide by zero"));}),_sy=new T(function(){return B(unCStr("loss of precision"));}),_sz=function(_sA){switch(E(_sA)){case 0:return E(_su);case 1:return E(_st);case 2:return E(_sy);case 3:return E(_sx);case 4:return E(_sw);default:return E(_sv);}},_sB=function(_sC){return new F(function(){return _1q(_st,_sC);});},_sD=function(_sC){return new F(function(){return _1q(_su,_sC);});},_sE=function(_sC){return new F(function(){return _1q(_sv,_sC);});},_sF=function(_sC){return new F(function(){return _1q(_sw,_sC);});},_sG=function(_sC){return new F(function(){return _1q(_sx,_sC);});},_sH=function(_sC){return new F(function(){return _1q(_sy,_sC);});},_sI=function(_sJ){switch(E(_sJ)){case 0:return E(_sD);case 1:return E(_sB);case 2:return E(_sH);case 3:return E(_sG);case 4:return E(_sF);default:return E(_sE);}},_sK=function(_sL,_sM){return new F(function(){return _2g(_sI,_sL,_sM);});},_sN=function(_sO,_sP){switch(E(_sP)){case 0:return E(_sD);case 1:return E(_sB);case 2:return E(_sH);case 3:return E(_sG);case 4:return E(_sF);default:return E(_sE);}},_sQ=[0,_sN,_sz,_sK],_sR=new T(function(){return [0,_so,_sQ,_sS,_sq];}),_sS=function(_sC){return [0,_sR,_sC];},_sT=3,_sU=new T(function(){return B(_g7(_sT,_sS));}),_sV=[0,0],_sW=function(_sX,_sY){while(1){var _sZ=E(_sX);if(!_sZ[0]){var _t0=E(_sZ[1]);if(_t0==(-2147483648)){_sX=[1,I_fromInt(-2147483648)];continue;}else{var _t1=E(_sY);if(!_t1[0]){return [0,_t0%_t1[1]];}else{_sX=[1,I_fromInt(_t0)];_sY=_t1;continue;}}}else{var _t2=_sZ[1],_t3=E(_sY);return _t3[0]==0?[0,I_toInt(I_rem(_t2,I_fromInt(_t3[1])))]:[1,I_rem(_t2,_t3[1])];}}},_t4=function(_t5,_t6){return !B(_rU(_t6,_sV))?B(_sW(_t5,_t6)):E(_sU);},_t7=function(_t8,_t9){while(1){if(!B(_rU(_t9,_sV))){var _ta=_t9,_tb=B(_t4(_t8,_t9));_t8=_ta;_t9=_tb;continue;}else{return E(_t8);}}},_tc=new T(function(){return B(_iV(_iU,_iT));}),_td=function(_te){var _tf=E(_te);if(!_tf[0]){var _tg=E(_tf[1]);return _tg==(-2147483648)?E(_tc):_tg<0?[0, -_tg]:E(_tf);}else{var _th=_tf[1];return I_compareInt(_th,0)>=0?E(_tf):[1,I_negate(_th)];}},_ti=function(_tj,_tk){while(1){var _tl=E(_tj);if(!_tl[0]){var _tm=E(_tl[1]);if(_tm==(-2147483648)){_tj=[1,I_fromInt(-2147483648)];continue;}else{var _tn=E(_tk);if(!_tn[0]){return [0,quot(_tm,_tn[1])];}else{_tj=[1,I_fromInt(_tm)];_tk=_tn;continue;}}}else{var _to=_tl[1],_tp=E(_tk);return _tp[0]==0?[0,I_toInt(I_quot(_to,I_fromInt(_tp[1])))]:[1,I_quot(_to,_tp[1])];}}},_tq=5,_tr=new T(function(){return B(_g7(_tq,_sS));}),_ts=function(_tt,_tu){if(!B(_rU(_tu,_sV))){var _tv=B(_t7(B(_td(_tt)),B(_td(_tu))));return !B(_rU(_tv,_sV))?[0,B(_ti(_tt,_tv)),B(_ti(_tu,_tv))]:E(_sU);}else{return E(_tr);}},_tw=new T(function(){return B(unCStr("Negative exponent"));}),_tx=new T(function(){return B(err(_tw));}),_ty=[0,2],_tz=function(_tA,_tB){while(1){var _tC=E(_tA);if(!_tC[0]){var _tD=_tC[1],_tE=E(_tB);if(!_tE[0]){var _tF=_tE[1],_tG=subC(_tD,_tF);if(!E(_tG[2])){return [0,_tG[1]];}else{_tA=[1,I_fromInt(_tD)];_tB=[1,I_fromInt(_tF)];continue;}}else{_tA=[1,I_fromInt(_tD)];_tB=_tE;continue;}}else{var _tH=E(_tB);if(!_tH[0]){_tA=_tC;_tB=[1,I_fromInt(_tH[1])];continue;}else{return [1,I_sub(_tC[1],_tH[1])];}}}},_tI=function(_tJ,_tK,_tL){while(1){if(!B(_rU(B(_sW(_tK,_ty)),_sV))){if(!B(_rU(_tK,_si))){var _tM=B(_jd(_tJ,_tJ)),_tN=B(_ti(B(_tz(_tK,_si)),_ty)),_tO=B(_jd(_tJ,_tL));_tJ=_tM;_tK=_tN;_tL=_tO;continue;}else{return new F(function(){return _jd(_tJ,_tL);});}}else{var _tM=B(_jd(_tJ,_tJ)),_tN=B(_ti(_tK,_ty));_tJ=_tM;_tK=_tN;continue;}}},_tP=function(_tQ,_tR){while(1){if(!B(_rU(B(_sW(_tR,_ty)),_sV))){if(!B(_rU(_tR,_si))){return new F(function(){return _tI(B(_jd(_tQ,_tQ)),B(_ti(B(_tz(_tR,_si)),_ty)),_tQ);});}else{return E(_tQ);}}else{var _tS=B(_jd(_tQ,_tQ)),_tT=B(_ti(_tR,_ty));_tQ=_tS;_tR=_tT;continue;}}},_tU=function(_tV,_tW){return !B(_s2(_tW,_sV))?!B(_rU(_tW,_sV))?B(_tP(_tV,_tW)):E(_si):E(_tx);},_tX=[0,1],_tY=[0,0],_tZ=[0,-1],_u0=function(_u1){var _u2=E(_u1);if(!_u2[0]){var _u3=_u2[1];return _u3>=0?E(_u3)==0?E(_tY):E(_iT):E(_tZ);}else{var _u4=I_compareInt(_u2[1],0);return _u4<=0?E(_u4)==0?E(_tY):E(_tZ):E(_iT);}},_u5=function(_u6,_u7,_u8){while(1){var _u9=E(_u8);if(!_u9[0]){if(!B(_s2(_u6,_ja))){return [0,B(_jd(_u7,B(_tU(_j9,_u6)))),_si];}else{var _ua=B(_tU(_j9,B(_j5(_u6))));return new F(function(){return _ts(B(_jd(_u7,B(_u0(_ua)))),B(_td(_ua)));});}}else{var _ub=B(_tz(_u6,_tX)),_uc=B(_iV(B(_jd(_u7,_j9)),B(_jb(E(_u9[1])[1]))));_u8=_u9[2];_u6=_ub;_u7=_uc;continue;}}},_ud=function(_ue,_uf){var _ug=E(_ue);if(!_ug[0]){var _uh=_ug[1],_ui=E(_uf);return _ui[0]==0?_uh>=_ui[1]:I_compareInt(_ui[1],_uh)<=0;}else{var _uj=_ug[1],_uk=E(_uf);return _uk[0]==0?I_compareInt(_uj,_uk[1])>=0:I_compare(_uj,_uk[1])>=0;}},_ul=function(_um){var _un=E(_um);if(!_un[0]){return new F(function(){return _ts(B(_jd(B(_jl(new T(function(){return B(_jb(E(_un[1])[1]));}),_ja,_un[2])),_tX)),_tX);});}else{var _uo=_un[1],_up=_un[3],_uq=E(_un[2]);if(!_uq[0]){var _ur=E(_up);if(!_ur[0]){return new F(function(){return _ts(B(_jd(B(_jl(_j9,_ja,_uo)),_tX)),_tX);});}else{var _us=_ur[1];if(!B(_ud(_us,_ja))){var _ut=B(_tU(_j9,B(_j5(_us))));return new F(function(){return _ts(B(_jd(B(_jl(_j9,_ja,_uo)),B(_u0(_ut)))),B(_td(_ut)));});}else{return new F(function(){return _ts(B(_jd(B(_jd(B(_jl(_j9,_ja,_uo)),B(_tU(_j9,_us)))),_tX)),_tX);});}}}else{var _uu=_uq[1],_uv=E(_up);if(!_uv[0]){return new F(function(){return _u5(_ja,B(_jl(_j9,_ja,_uo)),_uu);});}else{return new F(function(){return _u5(_uv[1],B(_jl(_j9,_ja,_uo)),_uu);});}}}},_uw=function(_ux,_uy){while(1){var _uz=E(_uy);if(!_uz[0]){return [0];}else{if(!B(A(_ux,[_uz[1]]))){return E(_uz);}else{_uy=_uz[2];continue;}}}},_uA=function(_uB,_uC){var _uD=E(_uB);if(!_uD[0]){var _uE=_uD[1],_uF=E(_uC);return _uF[0]==0?_uE>_uF[1]:I_compareInt(_uF[1],_uE)<0;}else{var _uG=_uD[1],_uH=E(_uC);return _uH[0]==0?I_compareInt(_uG,_uH[1])>0:I_compare(_uG,_uH[1])>0;}},_uI=[0,0],_uJ=function(_uK,_uL){return E(_uK)[1]==E(_uL)[1];},_uM=function(_o3){return new F(function(){return _uJ(_uI,_o3);});},_uN=[0,E(_ja),E(_si)],_uO=[1,_uN],_uP=function(_o3){return new F(function(){return _uJ(_uI,_o3);});},_uQ=[0,E(_ja),E(_si)],_uR=[1,_uQ],_uS=[0,-2147483648],_uT=[0,2147483647],_uU=function(_uV,_uW,_uX){var _uY=E(_uX);if(!_uY[0]){return [1,new T(function(){var _uZ=B(_ul(_uY));return [0,E(_uZ[1]),E(_uZ[2])];})];}else{var _v0=E(_uY[3]);if(!_v0[0]){return [1,new T(function(){var _v1=B(_ul(_uY));return [0,E(_v1[1]),E(_v1[2])];})];}else{var _v2=_v0[1];if(!B(_uA(_v2,_uT))){if(!B(_s2(_v2,_uS))){var _v3=function(_v4){var _v5=_v4+B(_kd(_v2))|0;return _v5<=(E(_uW)[1]+3|0)?_v5>=(E(_uV)[1]-3|0)?[1,new T(function(){var _v6=B(_ul(_uY));return [0,E(_v6[1]),E(_v6[2])];})]:E(_uR):[0];},_v7=B(_uw(_uP,_uY[1]));if(!_v7[0]){var _v8=E(_uY[2]);if(!_v8[0]){return E(_uO);}else{var _v9=B(_ga(_uM,_v8[1]));if(!E(_v9[2])[0]){return E(_uO);}else{return new F(function(){return _v3( -B(_sd(_v9[1],0)));});}}}else{return new F(function(){return _v3(B(_sd(_v7,0)));});}}else{return [0];}}else{return [0];}}}},_va=function(_vb){var _vc=E(_vb);switch(_vc[0]){case 3:var _vd=_vc[1];return !B(_5O(_vd,_rr))?!B(_5O(_vd,_rq))?E(_rs):E(_rG):E(_rC);case 5:var _ve=B(_uU(_rK,_rJ,_vc[1]));return _ve[0]==0?E(_rz):function(_vf,_vg){return new F(function(){return A(_vg,[new T(function(){var _vh=E(_ve[1]);return B(_sa(_vh[1],_vh[2]));})]);});};default:return E(_rv);}},_vi=function(_rw,_rx){return new F(function(){return _rc(_va,_rw,_rx);});},_vj=function(_vk,_vl){var _vm=function(_vn,_vo){return new F(function(){return _qX(function(_vp){var _vq=E(_vp);if(_vq[0]==2){var _vr=E(_vq[1]);if(!_vr[0]){return [2];}else{var _vs=_vr[2];switch(E(E(_vr[1])[1])){case 44:return E(_vs)[0]==0?!E(_vn)?[2]:E(new T(function(){return B(A(_vk,[_r1,function(_vt){return new F(function(){return _vm(_6H,function(_vu){return new F(function(){return A(_vo,[[1,_vt,_vu]]);});});});}]));})):[2];case 93:return E(_vs)[0]==0?E(new T(function(){return B(A(_vo,[_X]));})):[2];default:return [2];}}}else{return [2];}});});},_vv=function(_vw){return new F(function(){return _gI(B(_qX(function(_vx){var _vy=E(_vx);if(_vy[0]==2){var _vz=E(_vy[1]);return _vz[0]==0?[2]:E(E(_vz[1])[1])==91?E(_vz[2])[0]==0?E(new T(function(){return B(_gI(B(_vm(_54,_vw)),new T(function(){return B(A(_vk,[_r1,function(_vA){return new F(function(){return _vm(_6H,function(_vB){return new F(function(){return A(_vw,[[1,_vA,_vB]]);});});});}]));})));})):[2]:[2];}else{return [2];}})),new T(function(){return B(_r2(function(_vC,_vD){return new F(function(){return _vv(_vD);});},_vw));}));});};return new F(function(){return _vv(_vl);});},_vE=function(_vF,_vG){return new F(function(){return _vj(_vi,_vG);});},_vH=new T(function(){return B(_vj(_vi,_hT));}),_vI=function(_rx){return new F(function(){return _gy(_vH,_rx);});},_vJ=function(_vK){return function(_8i){return new F(function(){return _gy(new T(function(){return B(_rc(_va,_vK,_hT));}),_8i);});};},_vL=[0,_vJ,_vI,_vi,_vE],_vM=new T(function(){return B(unCStr("Travel"));}),_vN=function(_vO){var _vP=jsShow(E(_vO)[1]),_vQ=_vP;return new F(function(){return fromJSStr(_vQ);});},_vR=function(_vS){return function(_8i){return new F(function(){return _1q(new T(function(){return B(_vN(_vS));}),_8i);});};},_vT=[0,45],_vU=function(_vV,_vW,_vX){var _vY=new T(function(){return B(A(_vV,[[0, -_vX]]));}),_vZ=new T(function(){if(E(_vW)[1]<=6){var _w0=function(_w1){return [1,_vT,new T(function(){return B(A(_vY,[_w1]));})];};}else{var _w0=function(_w2){return [1,_3w,[1,_vT,new T(function(){return B(A(_vY,[[1,_3v,_w2]]));})]];};}var _w3=_w0;return _w3;});if(_vX>=0){var _w4=isDoubleNegativeZero(_vX),_w5=_w4;return E(_w5)==0?B(A(_vV,[[0,_vX]])):E(_vZ);}else{return E(_vZ);}},_w6=[0,0],_w7=function(_w8){return new F(function(){return A(_vU,[_vR,_w6,E(_w8)[1],_X]);});},_w9=[0,0],_wa=function(_wb){return new F(function(){return _vU(_vR,_w9,E(_wb)[1]);});},_wc=function(_wd,_we){return new F(function(){return _2g(_wa,_wd,_we);});},_wf=function(_wg,_wh){return new F(function(){return _vU(_vR,_wg,E(_wh)[1]);});},_wi=[0,_wf,_w7,_wc],_wj=function(_wk,_wl,_wm,_){var _wn=B(_y(_wk,_wm,_)),_wo=_wn,_wp=B(A(_wl,[_wo,_])),_wq=_wp;return _wo;},_wr=new T(function(){return B(unCStr("()"));}),_ws=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_bk,_br,_wr],_wt=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_ws,_X],_wu=function(_wv){return E(_wt);},_ww=new T(function(){return B(unCStr("haste-perch-0.1.0.4"));}),_wx=new T(function(){return B(unCStr("Haste.Perch"));}),_wy=new T(function(){return B(unCStr("PerchM"));}),_wz=[0,I_fromBits([3855559683,1906498183]),I_fromBits([3763257881,2457147146]),_ww,_wx,_wy],_wA=[0,I_fromBits([3855559683,1906498183]),I_fromBits([3763257881,2457147146]),_wz,_X],_wB=function(_wC){return E(_wA);},_wD=new T(function(){return B(_dx(_wB,_wu));}),_wE=new T(function(){return B(unCStr("onclick"));}),_wF=new T(function(){return B(unCStr("checked"));}),_wG=[0,_wF,_X],_wH=[1,_wG,_X],_wI=new T(function(){return B(unCStr("input"));}),_wJ=function(_wK,_){return new F(function(){return _y(_wI,_wK,_);});},_wL=function(_wM,_wN,_wO,_wP,_wQ){var _wR=new T(function(){var _wS=new T(function(){return B(_p(_wJ,[1,[0,_5q,_wN],[1,[0,_6,_wM],[1,[0,_5p,_wO],_X]]]));});return !E(_wP)?E(_wS):B(_p(_wS,_wH));}),_wT=E(_wQ);return _wT[0]==0?E(_wR):B(_p(_wR,[1,[0,_wE,_wT[1]],_X]));},_wU=new T(function(){return B(unCStr("href"));}),_wV=[0,97],_wW=[1,_wV,_X],_wX=function(_wY,_){return new F(function(){return _y(_wW,_wY,_);});},_wZ=function(_x0,_x1){return function(_x2,_){var _x3=B(A(new T(function(){return B(_p(_wX,[1,[0,_wU,_x0],_X]));}),[_x2,_])),_x4=_x3,_x5=B(A(_x1,[_x4,_])),_x6=_x5;return _x4;};},_x7=function(_x8){return new F(function(){return _wZ(_x8,function(_w,_){return new F(function(){return _0(_x8,_w,_);});});});},_x9=new T(function(){return B(unCStr("option"));}),_xa=function(_xb,_){return new F(function(){return _y(_x9,_xb,_);});},_xc=new T(function(){return B(unCStr("selected"));}),_xd=[0,_xc,_X],_xe=[1,_xd,_X],_xf=function(_xg,_xh,_xi){var _xj=function(_xk,_){var _xl=B(A(new T(function(){return B(_p(_xa,[1,[0,_5p,_xg],_X]));}),[_xk,_])),_xm=_xl,_xn=B(A(_xh,[_xm,_])),_xo=_xn;return _xm;};return !E(_xi)?E(_xj):B(_p(_xj,_xe));},_xp=function(_xq,_xr){return new F(function(){return _xf(_xq,function(_w,_){return new F(function(){return _0(_xq,_w,_);});},_xr);});},_xs=new T(function(){return B(unCStr("method"));}),_xt=new T(function(){return B(unCStr("action"));}),_xu=new T(function(){return B(unCStr("UTF-8"));}),_xv=new T(function(){return B(unCStr("acceptCharset"));}),_xw=[0,_xv,_xu],_xx=new T(function(){return B(unCStr("form"));}),_xy=function(_xz,_){return new F(function(){return _y(_xx,_xz,_);});},_xA=function(_xB,_xC,_xD){return function(_xE,_){var _xF=B(A(new T(function(){return B(_p(_xy,[1,_xw,[1,[0,_xt,_xB],[1,[0,_xs,_xC],_X]]]));}),[_xE,_])),_xG=_xF,_xH=B(A(_xD,[_xG,_])),_xI=_xH;return _xG;};},_xJ=new T(function(){return B(unCStr("select"));}),_xK=function(_xL,_){return new F(function(){return _y(_xJ,_xL,_);});},_xM=function(_xN,_xO){return function(_xP,_){var _xQ=B(A(new T(function(){return B(_p(_xK,[1,[0,_6,_xN],_X]));}),[_xP,_])),_xR=_xQ,_xS=B(A(_xO,[_xR,_])),_xT=_xS;return _xR;};},_xU=new T(function(){return B(unCStr("textarea"));}),_xV=function(_xW,_){return new F(function(){return _y(_xU,_xW,_);});},_xX=function(_xY,_xZ){return function(_y0,_){var _y1=B(A(new T(function(){return B(_p(_xV,[1,[0,_6,_xY],_X]));}),[_y0,_])),_y2=_y1,_y3=B(_0(_xZ,_y2,_)),_y4=_y3;return _y2;};},_y5=new T(function(){return B(unCStr("color:red"));}),_y6=new T(function(){return B(unCStr("style"));}),_y7=[0,_y6,_y5],_y8=[1,_y7,_X],_y9=[0,98],_ya=[1,_y9,_X],_yb=function(_yc){return new F(function(){return _p(function(_yd,_){var _ye=B(_y(_ya,_yd,_)),_yf=_ye,_yg=B(A(_yc,[_yf,_])),_yh=_yg;return _yf;},_y8);});},_yi=function(_yj,_yk,_){var _yl=E(_yj);if(!_yl[0]){return _yk;}else{var _ym=B(A(_yl[1],[_yk,_])),_yn=_ym,_yo=B(_yi(_yl[2],_yk,_)),_yp=_yo;return _yk;}},_yq=function(_yr,_ys,_yt,_){var _yu=B(A(_yr,[_yt,_])),_yv=_yu,_yw=B(A(_ys,[_yt,_])),_yx=_yw;return _yt;},_yy=[0,_2Q,_yq,_yi],_yz=[0,_yy,_wD,_0,_0,_wj,_yb,_wZ,_x7,_wL,_xX,_xM,_xf,_xp,_xA,_p],_yA=[0,_2S,_e],_yB=function(_yC,_yD,_){var _yE=jsGet(_yC,toJSStr(E(_yD))),_yF=_yE;return new T(function(){return fromJSStr(_yF);});},_yG=function(_yH){return E(E(_yH)[1]);},_yI=function(_yJ){return E(E(_yJ)[1]);},_yK=function(_yL){return E(E(_yL)[2]);},_yM=function(_yN,_yO){var _yP=new T(function(){return B(_yI(_yN));});return function(_yQ){return new F(function(){return A(new T(function(){return B(_2T(_yP));}),[new T(function(){return B(A(_yK,[_yN,_yO]));}),function(_yR){return new F(function(){return A(new T(function(){return B(_3i(_yP));}),[[0,_yR,_yQ]]);});}]);});};},_yS=function(_yT,_yU){return new F(function(){return A(_yT,[function(_){return new F(function(){return jsFind(toJSStr(E(_yU)));});}]);});},_yV=[0],_yW=function(_yX){return E(E(_yX)[3]);},_yY=new T(function(){return [0,"value"];}),_yZ=function(_z0){return E(E(_z0)[6]);},_z1=new T(function(){return B(unCStr("[]"));}),_z2=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_bk,_bl,_z1],_z3=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_z2,_X],_z4=function(_z5){return E(_z3);},_z6=new T(function(){return B(unCStr("Char"));}),_z7=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_bk,_bl,_z6],_z8=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_z7,_X],_z9=function(_za){return E(_z8);},_zb=new T(function(){return B(_dx(_z4,_z9));}),_zc=new T(function(){return B(A(_zb,[_b6]));}),_zd=function(_ze){return E(E(_ze)[1]);},_zf=[0,32],_zg=[0,10],_zh=function(_zi){var _zj=E(_zi);if(!_zj[0]){return E(_e);}else{var _zk=_zj[1],_zl=E(_zj[2]);if(!_zl[0]){return new F(function(){return _zm(_zg,_zk);});}else{return function(_zn){return new F(function(){return A(new T(function(){return B(_zm(_zg,_zk));}),[[1,_zf,new T(function(){return B(A(new T(function(){return B(_zh(_zl));}),[_zn]));})]]);});};}}},_zo=new T(function(){return B(unCStr("->"));}),_zp=[1,_zo,_X],_zq=[1,_bl,_zp],_zr=[1,_bk,_zq],_zs=[0,32],_zt=function(_zu){var _zv=E(_zu);if(!_zv[0]){return [0];}else{var _zw=_zv[1],_zx=E(_zv[2]);if(!_zx[0]){return E(_zw);}else{return new F(function(){return _1q(_zw,[1,_zs,new T(function(){return B(_zt(_zx));})]);});}}},_zy=new T(function(){return B(_zt(_zr));}),_zz=new T(function(){var _zA=B(_bJ(_zy));return [0,_zA[1],_zA[2],_bk,_bl,_zo];}),_zB=function(_zC,_zD){var _zE=E(_zC);if(!_zE[0]){return E(_zD);}else{return new F(function(){return A(_zE[1],[new T(function(){return B(_zB(_zE[2],_zD));})]);});}},_zF=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_zG=[1,_wt,_X],_zH=function(_zI){var _zJ=E(_zI);if(!_zJ[0]){return [0];}else{var _zK=E(_zJ[1]);return [1,[0,_zK[1],_zK[2]],new T(function(){return B(_zH(_zJ[2]));})];}},_zL=new T(function(){var _zM=B(_1q(_X,_zG));if(!_zM[0]){var _zN=E(_z2);}else{var _zO=B(_bJ(new T(function(){return B(_bx(B(_c1(_cg,[1,_zF,new T(function(){return B(_zH(_zM));})]))));}))),_zN=E(_z2);}return _zN;}),_zP=[0,40],_zQ=function(_zR){return new F(function(){return _zm(_zg,_zR);});},_zS=[0,8],_zT=new T(function(){return B(unCStr(" -> "));}),_zU=[0,9],_zV=[0,93],_zW=[0,91],_zX=[0,41],_zY=[0,44],_zZ=function(_zR){return [1,_zY,_zR];},_A0=function(_A1,_A2){var _A3=E(_A2);return _A3[0]==0?[0]:[1,_A1,[1,_A3[1],new T(function(){return B(_A0(_A1,_A3[2]));})]];},_zm=function(_A4,_A5){var _A6=E(_A5),_A7=_A6[3],_A8=E(_A6[4]);if(!_A8[0]){return function(_A9){return new F(function(){return _1q(E(_A7)[5],_A9);});};}else{var _Aa=_A8[1],_Ab=new T(function(){var _Ac=E(_A7)[5],_Ad=new T(function(){return B(_zh(_A8));}),_Ae=new T(function(){if(E(_A4)[1]<=9){var _Af=function(_Ag){return new F(function(){return _1q(_Ac,[1,_zf,new T(function(){return B(A(_Ad,[_Ag]));})]);});};}else{var _Af=function(_Ah){return [1,_3w,new T(function(){return B(_1q(_Ac,[1,_zf,new T(function(){return B(A(_Ad,[[1,_3v,_Ah]]));})]));})];};}var _Ai=_Af;return _Ai;}),_Aj=E(_Ac);if(!_Aj[0]){var _Ak=E(_Ae);}else{if(E(E(_Aj[1])[1])==40){var _Al=E(_Aj[2]);if(!_Al[0]){var _Am=E(_Ae);}else{if(E(E(_Al[1])[1])==44){var _An=function(_Ao){return [1,_zP,new T(function(){return B(A(new T(function(){var _Ap=B(_c1(_zQ,_A8));if(!_Ap[0]){var _Aq=E(_e);}else{var _Aq=function(_8i){return new F(function(){return _zB([1,_Ap[1],new T(function(){return B(_A0(_zZ,_Ap[2]));})],_8i);});};}return _Aq;}),[[1,_zX,_Ao]]));})];};}else{var _An=E(_Ae);}var _Ar=_An,_Am=_Ar;}var _As=_Am;}else{var _As=E(_Ae);}var _At=_As,_Ak=_At;}var _Au=_Ak;return _Au;}),_Av=E(_A8[2]);if(!_Av[0]){var _Aw=E(_A7),_Ax=E(_zL),_Ay=hs_eqWord64(_Aw[1],_Ax[1]),_Az=_Ay;if(!E(_Az)){return E(_Ab);}else{var _AA=hs_eqWord64(_Aw[2],_Ax[2]),_AB=_AA;return E(_AB)==0?E(_Ab):function(_AC){return [1,_zW,new T(function(){return B(A(new T(function(){return B(_zm(_w6,_Aa));}),[[1,_zV,_AC]]));})];};}}else{if(!E(_Av[2])[0]){var _AD=E(_A7),_AE=E(_zz),_AF=hs_eqWord64(_AD[1],_AE[1]),_AG=_AF;if(!E(_AG)){return E(_Ab);}else{var _AH=hs_eqWord64(_AD[2],_AE[2]),_AI=_AH;if(!E(_AI)){return E(_Ab);}else{var _AJ=new T(function(){return B(_zm(_zS,_Av[1]));}),_AK=new T(function(){return B(_zm(_zU,_Aa));});return E(_A4)[1]<=8?function(_AL){return new F(function(){return A(_AK,[new T(function(){return B(_1q(_zT,new T(function(){return B(A(_AJ,[_AL]));})));})]);});}:function(_AM){return [1,_3w,new T(function(){return B(A(_AK,[new T(function(){return B(_1q(_zT,new T(function(){return B(A(_AJ,[[1,_3v,_AM]]));})));})]));})];};}}}else{return E(_Ab);}}}},_AN=function(_AO,_AP,_AQ,_AR,_AS,_AT){var _AU=E(_AO),_AV=_AU[1],_AW=_AU[3],_AX=new T(function(){return B(A(_AW,[_yV]));});return new F(function(){return A(_AV,[new T(function(){return B(_yS(_AP,_AT));}),function(_AY){var _AZ=E(_AY);return _AZ[0]==0?E(_AX):B(A(_AV,[new T(function(){return B(A(_AP,[function(_){var _B0=jsGet(E(_AZ[1])[1],E(_yY)[1]),_B1=_B0;return [1,new T(function(){return fromJSStr(_B1);})];}]));}),function(_B2){var _B3=E(_B2);if(!_B3[0]){return E(_AX);}else{var _B4=_B3[1];if(!E(new T(function(){var _B5=B(A(_AQ,[_b7])),_B6=E(_zc),_B7=hs_eqWord64(_B5[1],_B6[1]),_B8=_B7;if(!E(_B8)){var _B9=false;}else{var _Ba=hs_eqWord64(_B5[2],_B6[2]),_Bb=_Ba,_B9=E(_Bb)==0?false:true;}var _Bc=_B9,_Bd=_Bc;return _Bd;}))){var _Be=new T(function(){return B(A(_AW,[[1,_B4,new T(function(){return B(A(new T(function(){return B(_yZ(_AS));}),[new T(function(){return B(A(new T(function(){return B(_yW(_AS));}),[new T(function(){return B(unAppCStr("can\'t read \"",new T(function(){return B(_1q(_B4,new T(function(){return B(unAppCStr("\" as type ",new T(function(){return B(A(_zm,[_w6,B(A(_AQ,[_b7])),_X]));})));})));})));})]));})]));})]]));}),_Bf=B(A(new T(function(){return B(A(_zd,[_AR,_53]));}),[_B4]));if(!_Bf[0]){return E(_Be);}else{var _Bg=E(_Bf[1]);return E(_Bg[2])[0]==0?E(_Bf[2])[0]==0?B(A(_AW,[[2,_Bg[1]]])):E(_Be):E(_Be);}}else{return new F(function(){return A(_AW,[[2,_B4]]);});}}}]));}]);});},_Bh=1,_Bi=function(_Bj){return E(E(_Bj)[9]);},_Bk=function(_Bl,_Bm){return new F(function(){return A(_3i,[_Bl,[0,_Bm,_Bm]]);});},_Bn=function(_Bo){return E(E(_Bo)[2]);},_Bp=function(_Bq,_Br,_Bs){return new F(function(){return A(_3i,[_Bq,[0,_7,_Br]]);});},_Bt=function(_Bu){return E(E(_Bu)[2]);},_Bv=function(_Bw,_Bx,_By,_Bz,_BA){var _BB=new T(function(){return B(_yI(_Bx));}),_BC=new T(function(){return B(_3k(_BB));}),_BD=new T(function(){return B(_3i(_BB));}),_BE=new T(function(){return B(_Bt(_Bz));}),_BF=new T(function(){return B(_Bi(_Bw));});return function(_BG,_BH,_BI){return function(_BJ){return new F(function(){return A(new T(function(){return B(_2T(_BB));}),[new T(function(){var _BK=E(_BG);return _BK[0]==0?B(A(new T(function(){return B(_3D([0,_],_BC,function(_BL){return new F(function(){return _Bk(_BB,_BL);});},function(_BM,_BN){return new F(function(){return _Bp(_BB,_BM,_BN);});}));}),[_BJ])):B(A(new T(function(){return B(_3i(_BB));}),[[0,_BK[1],_BJ]]));}),function(_BO){var _BP=new T(function(){return E(E(_BO)[1]);});return new F(function(){return A(new T(function(){return B(_2T(_BB));}),[new T(function(){var _BQ=new T(function(){return E(E(_BO)[2]);});return B(A(new T(function(){return B(_3i(_BB));}),[[0,_BQ,_BQ]]));}),function(_BR){return new F(function(){return A(new T(function(){return B(_2T(_BB));}),[new T(function(){return B(A(new T(function(){return B(_3i(_BB));}),[[0,_7,new T(function(){var _BS=E(E(_BR)[1]);return [0,_BS[1],_BS[2],_Bh,_BS[4],_BS[5],_BS[6]];})]]));}),function(_BT){return new F(function(){return A(new T(function(){return B(_2T(_BB));}),[new T(function(){return B(A(new T(function(){return B(_AN(_BC,function(_BU){return new F(function(){return _yM(_Bx,_BU);});},_By,_BA,_Bw,_BP));}),[new T(function(){return E(E(_BT)[2]);})]));}),function(_BV){var _BW=E(_BV),_BX=_BW[2],_BY=E(_BW[1]);switch(_BY[0]){case 0:return new F(function(){return A(_BD,[[0,[0,new T(function(){return B(A(_BF,[_BP,_BH,new T(function(){var _BZ=E(_BI);if(!_BZ[0]){var _C0=[0];}else{var _C1=_BZ[1],_C2=B(_15(_By,_zb,_C1)),_C0=_C2[0]==0?B(A(_BE,[_C1])):E(_C2[1]);}return _C0;}),_54,_2A]));}),_2A],_BX]]);});break;case 1:return new F(function(){return A(_BD,[[0,[0,new T(function(){return B(A(new T(function(){return B(_Bn(new T(function(){return B(_yG(_Bw));})));}),[new T(function(){return B(A(_BF,[_BP,_BH,_BY[1],_54,_2A]));}),_BY[2]]));}),_2A],_BX]]);});break;default:var _C3=_BY[1];return new F(function(){return A(_BD,[[0,[0,new T(function(){return B(A(_BF,[_BP,_BH,new T(function(){var _C4=B(_15(_By,_zb,_C3));return _C4[0]==0?B(A(_BE,[_C3])):E(_C4[1]);}),_54,_2A]));}),[1,_C3]],_BX]]);});}}]);});}]);});}]);});}]);});};};},_C5=function(_C6,_C7,_C8,_C9){return new F(function(){return A(_C6,[new T(function(){return function(_){var _Ca=jsSet(E(_C7)[1],toJSStr(E(_C8)),toJSStr(E(_C9)));return _7;};})]);});},_Cb=[0,0],_Cc=new T(function(){return B(unCStr("value"));}),_Cd=new T(function(){return B(unCStr("text"));}),_Ce=new T(function(){return B(_dx(_z4,_z9));}),_Cf=new T(function(){return B(A(_Ce,[_b6]));}),_Cg=new T(function(){return E(_b6);}),_Ch=new T(function(){return B(A(_Ce,[_b6]));}),_Ci=new T(function(){return B(unCStr(" could be found!"));}),_Cj=function(_Ck){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_1q(_Ck,_Ci));}))));});},_Cl=function(_Cm,_Cn,_Co,_Cp){var _Cq=new T(function(){return B(_Bt(_Cm));});return [0,function(_Cr){return new F(function(){return A(new T(function(){return B(_Bv(_yz,_yA,_Co,_Cm,_Cn));}),[[1,_Cp],_Cd,_Cr]);});},function(_Cs,_){var _Ct=E(_Cp),_Cu=jsFind(toJSStr(_Ct)),_Cv=_Cu,_Cw=E(_Cv);return _Cw[0]==0?B(_Cj(_Ct)):B(A(_C5,[_e,_Cw[1],_Cc,new T(function(){var _Cx=B(A(_Co,[_Cs])),_Cy=E(_Ch),_Cz=hs_eqWord64(_Cx[1],_Cy[1]),_CA=_Cz;if(!E(_CA)){var _CB=B(A(_Cq,[_Cs]));}else{var _CC=hs_eqWord64(_Cx[2],_Cy[2]),_CD=_CC,_CB=E(_CD)==0?B(A(_Cq,[_Cs])):E(_Cs);}var _CE=_CB,_CF=_CE;return _CF;}),_]));},function(_){var _CG=E(_Cp),_CH=jsFind(toJSStr(_CG)),_CI=_CH,_CJ=E(_CI);if(!_CJ[0]){return new F(function(){return _Cj(_CG);});}else{var _CK=B(_yB(E(_CJ[1])[1],_Cc,_)),_CL=_CK;return new T(function(){var _CM=B(A(_Co,[_Cg])),_CN=E(_Cf),_CO=hs_eqWord64(_CM[1],_CN[1]),_CP=_CO,_CQ=new T(function(){var _CR=B(A(_zd,[_Cn,_Cb,_CL]));if(!_CR[0]){var _CS=[0];}else{var _CS=E(_CR[2])[0]==0?[1,E(_CR[1])[1]]:[0];}return _CS;});if(!E(_CP)){var _CT=E(_CQ);}else{var _CU=hs_eqWord64(_CM[2],_CN[2]),_CV=_CU,_CT=E(_CV)==0?E(_CQ):E(_CL);}var _CW=_CT,_CX=_CW;return _CX;});}}];},_CY=function(_CZ,_){var _D0=B(A(B(_Cl(_wi,_vL,_bp,_vM))[3],[_])),_D1=_D0;return [0,[0,_2Q,_D1],_CZ];},_D2=new T(function(){return B(unCStr("Food"));}),_D3=function(_D4,_){var _D5=B(A(B(_Cl(_wi,_vL,_bp,_D2))[3],[_])),_D6=_D5;return [0,[0,_2Q,_D6],_D4];},_D7=new T(function(){return B(unCStr("Entertain"));}),_D8=function(_D9,_){var _Da=B(A(B(_Cl(_wi,_vL,_bp,_D7))[3],[_])),_Db=_Da;return [0,[0,_2Q,_Db],_D9];},_Dc=new T(function(){return B(unCStr("Other"));}),_Dd=function(_De,_){var _Df=B(A(B(_Cl(_wi,_vL,_bp,_Dc))[3],[_])),_Dg=_Df;return [0,[0,_2Q,_Dg],_De];},_Dh=new T(function(){return B(unCStr("Income"));}),_Di=function(_Dj,_){var _Dk=B(A(B(_Cl(_wi,_vL,_bp,_Dh))[3],[_])),_Dl=_Dk;return [0,[0,_2Q,_Dl],_Dj];},_Dm=function(_Dn,_Do,_){return new F(function(){return _3Q(_D3,function(_Dp){return function(_8i,_8j){return new F(function(){return _3Q(_D8,function(_Dq){return function(_8i,_8j){return new F(function(){return _3Q(_Dd,function(_Dr){return function(_8i,_8j){return new F(function(){return _3Q(_Di,function(_Ds){return function(_Dt,_){return [0,[0,_2Q,[1,[0,_Dn,_Dp,_Dq,_Dr,_Ds]]],_Dt];};},_8i,_8j);});};},_8i,_8j);});};},_8i,_8j);});};},_Do,_);});},_Du=function(_Dv,_){return new F(function(){return _3Q(_CY,_Dm,_Dv,_);});},_Dw=new T(function(){return B(unCStr("br"));}),_Dx=function(_Dy,_){var _Dz=jsCreateElem(toJSStr(E(_Dw))),_DA=_Dz,_DB=jsAppendChild(_DA,E(_Dy)[1]);return [0,_DA];},_DC=[13,_],_DD=new T(function(){return B(unCStr("AllEntries"));}),_DE=function(_DF,_DG){return function(_8i,_8j){return new F(function(){return _3Q(new T(function(){return B(_8m(new T(function(){return B(A(B(_Cl(_wi,_vL,_bp,new T(function(){switch(E(_DF)){case 0:var _DH=E(_vM);break;case 1:var _DH=E(_D2);break;case 2:var _DH=E(_D7);break;case 3:var _DH=E(_Dc);break;case 4:var _DH=E(_Dh);break;default:var _DH=E(_DD);}return _DH;})))[1],[[1,_DG]]));}),_DC));}),function(_DI,_Dv,_){return new F(function(){return (function(_DJ,_){return [0,[0,_2Q,[1,_DF]],_DJ];})(_Dv,_);});},_8i,_8j);});};},_DK=new T(function(){return B(unCStr("h3"));}),_DL=function(_DM,_DN){return function(_DO,_){var _DP=jsCreateElem(toJSStr(E(_DK))),_DQ=_DP,_DR=jsAppendChild(_DQ,E(_DO)[1]),_DS=[0,_DQ],_DT=B(A(new T(function(){return B(A(_DM,[_DN]));}),[_DS,_])),_DU=_DT;return _DS;};},_DV=new T(function(){return B(unCStr("Preview"));}),_DW=new T(function(){return B(_DL(_0,_DV));}),_DX=new T(function(){return B(unCStr("h4"));}),_DY=function(_DZ,_E0){return function(_E1,_){var _E2=jsCreateElem(toJSStr(E(_DX))),_E3=_E2,_E4=jsAppendChild(_E3,E(_E1)[1]),_E5=[0,_E3],_E6=B(A(new T(function(){return B(A(_DZ,[_E0]));}),[_E5,_])),_E7=_E6;return _E5;};},_E8=new T(function(){return B(unCStr("Recalculate the budget according with priorities and present a chart graph"));}),_E9=new T(function(){return B(_DY(_0,_E8));}),_Ea=new T(function(){var _Eb=B(_Cl(_wi,_vL,_bp,_D7));return [0,_Eb[1],_Eb[2],_Eb[3]];}),_Ec=new T(function(){var _Ed=B(_Cl(_wi,_vL,_bp,_D7));return [0,_Ed[1],_Ed[2],_Ed[3]];}),_Ee=new T(function(){var _Ef=B(_Cl(_wi,_vL,_bp,_Dc));return [0,_Ef[1],_Ef[2],_Ef[3]];}),_Eg=new T(function(){var _Eh=B(_Cl(_wi,_vL,_bp,_D7));return [0,_Eh[1],_Eh[2],_Eh[3]];}),_Ei=new T(function(){var _Ej=B(_Cl(_wi,_vL,_bp,_D7));return [0,_Ej[1],_Ej[2],_Ej[3]];}),_Ek=new T(function(){return [0,"lst2arr"];}),_El=new T(function(){return B(_50(_Ek));}),_Em=function(_En){return E(toJSStr(E(_En)));},_Eo=function(_Ep){return E(E(_Ep)[1]);},_Eq=function(_Er,_Es){return new F(function(){return _4W(function(_){var _=0;return new F(function(){return A(_El,[[1,new T(function(){return B(_Em(_Er));}),[1,new T(function(){return B(_Eo(_Es));}),_X]],_]);});});});},_Et=function(_Eu){var _Ev=E(_Eu);return new F(function(){return _Eq(_Ev[1],_Ev[2]);});},_Ew=new T(function(){return B(unCStr("width: 900px; height: 500px;"));}),_Ex=new T(function(){return B(unCStr("piechart"));}),_Ey=new T(function(){return B(unCStr("id"));}),_Ez=new T(function(){return B(unCStr("style"));}),_EA=new T(function(){return B(unCStr("https://google-developers.appspot.com/chart/interactive/docs/gallery/piechart"));}),_EB=new T(function(){return B(unCStr("href"));}),_EC=new T(function(){return B(unCStr("Please connect to Internet to download the "));}),_ED=new T(function(){return B(unCStr("span"));}),_EE=function(_EF,_EG){return function(_EH,_){var _EI=jsCreateElem(toJSStr(E(_ED))),_EJ=_EI,_EK=jsAppendChild(_EJ,E(_EH)[1]),_EL=[0,_EJ],_EM=B(A(new T(function(){return B(A(_EF,[_EG]));}),[_EL,_])),_EN=_EM;return _EL;};},_EO=new T(function(){return B(_EE(_0,_EC));}),_EP=[0,97],_EQ=[1,_EP,_X],_ER=function(_ES,_ET){return function(_EU,_){var _EV=jsCreateElem(toJSStr(_EQ)),_EW=_EV,_EX=jsAppendChild(_EW,E(_EU)[1]),_EY=[0,_EW],_EZ=B(A(new T(function(){return B(A(_ES,[_ET]));}),[_EY,_])),_F0=_EZ;return _EY;};},_F1=new T(function(){return B(unCStr("Pie Chart graphics"));}),_F2=new T(function(){return B(_ER(_0,_F1));}),_F3=new T(function(){return B(unCStr("from Google"));}),_F4=new T(function(){return B(_EE(_0,_F3));}),_F5=function(_F6,_){var _F7=B(A(_EO,[_F6,_])),_F8=_F7,_F9=B(A(_8,[_e,_F8,_fE,_fF,_])),_Fa=_F9,_Fb=B(A(_F2,[_F6,_])),_Fc=_Fb,_Fd=B(A(_8,[_e,_Fc,_EB,_EA,_])),_Fe=_Fd,_Ff=B(A(_F4,[_F6,_])),_Fg=_Ff,_Fh=B(A(_8,[_e,_Fg,_fE,_fF,_])),_Fi=_Fh;return _F6;},_Fj=new T(function(){return B(unCStr("div"));}),_Fk=function(_Fl,_Fm){return function(_Fn,_){var _Fo=jsCreateElem(toJSStr(E(_Fj))),_Fp=_Fo,_Fq=jsAppendChild(_Fp,E(_Fn)[1]),_Fr=[0,_Fp],_Fs=B(A(new T(function(){return B(A(_Fl,[_Fm]));}),[_Fr,_])),_Ft=_Fs;return _Fr;};},_Fu=function(_Fv){return E(_Fv);},_Fw=new T(function(){return B(_Fk(_Fu,_F5));}),_Fx=function(_Fy,_){var _Fz=B(A(_Fw,[_Fy,_])),_FA=_Fz,_FB=B(A(_8,[_e,_FA,_Ey,_Ex,_])),_FC=_FB,_FD=B(A(_8,[_e,_FA,_Ez,_Ew,_])),_FE=_FD;return _FA;},_FF=[0,_Fx,_4E],_FG=function(_FH,_){return [0,_FF,_FH];},_FI=new T(function(){return [0,"(function (data){var chart = new google.visualization.PieChart(document.getElementById(\'piechart\'));return chart.draw(google.visualization.arrayToDataTable(data), options);})"];}),_FJ=new T(function(){return B(_50(_FI));}),_FK=function(_FL,_FM,_){return new F(function(){return _3Q(_FG,function(_FN,_Dv,_){return new F(function(){return (function(_FO,_){return [0,[0,function(_FP,_){var _FQ=E(_FL),_FR=B(A(_FJ,[B(_4W(function(_){var _=0;return new F(function(){return A(_El,[[1,new T(function(){var _FS=E(_FQ[1]);return B(_4W(function(_){var _=0;return new F(function(){return A(_El,[[1,new T(function(){return B(_Em(_FS[1]));}),[1,new T(function(){return B(_Em(_FS[2]));}),_X]],_]);});}));}),[1,new T(function(){return B(_Et(_FQ[2]));}),[1,new T(function(){return B(_Et(_FQ[3]));}),[1,new T(function(){return B(_Et(_FQ[4]));}),[1,new T(function(){return B(_Et(_FQ[5]));}),[1,new T(function(){return B(_Et(_FQ[6]));}),_X]]]]]],_]);});})),_])),_FT=_FR;return _FP;},_4E],_FO];})(_Dv,_);});},_FM,_);});},_FU=[0,98],_FV=[1,_FU,_X],_FW=function(_FX,_FY){return function(_FZ,_){var _G0=jsCreateElem(toJSStr(_FV)),_G1=_G0,_G2=jsAppendChild(_G1,E(_FZ)[1]),_G3=[0,_G1],_G4=B(A(new T(function(){return B(A(_FX,[_FY]));}),[_G3,_])),_G5=_G4;return _G3;};},_G6=new T(function(){return B(unCStr("No graphics since some quantity is negative"));}),_G7=new T(function(){return B(_FW(_0,_G6));}),_G8=[0,_G7,_4E],_G9=function(_Ga,_){return [0,_G8,_Ga];},_Gb=new T(function(){return B(unCStr("Entertainment"));}),_Gc=new T(function(){return B(unCStr("Type"));}),_Gd=new T(function(){return B(unCStr("Spent"));}),_Ge=[0,_Gc,_Gd],_Gf=function(_Gg,_Gh,_Gi,_Gj,_Gk){if(_Gj<0){return E(_G9);}else{var _Gl=E(_Gh);if(_Gl[1]<0){return E(_G9);}else{var _Gm=E(_Gk);if(_Gm[1]<0){return E(_G9);}else{var _Gn=E(_Gi);if(_Gn[1]<0){return E(_G9);}else{var _Go=E(_Gg);return _Go[1]<0?E(_G9):function(_8i,_8j){return new F(function(){return _FK([0,_Ge,[0,_vM,[0,_Gj]],[0,_D2,_Gl],[0,_Gb,_Gm],[0,_Dc,_Gn],[0,_Dh,_Go]],_8i,_8j);});};}}}}},_Gp=function(_Gq){var _Gr=E(_Gq);return new F(function(){return _Gf(_Gr[1],_Gr[2],_Gr[3],E(_Gr[4])[1],_Gr[5]);});},_Gs=5,_Gt=[1,_Gs],_Gu=[1,_7],_Gv=[0,_2Q,_Gu],_Gw=new T(function(){return B(_EE(_0,_Dh));}),_Gx=new T(function(){return B(_EE(_0,_vM));}),_Gy=new T(function(){return B(_EE(_0,_D2));}),_Gz=new T(function(){return B(_EE(_0,_Gb));}),_GA=new T(function(){return B(_EE(_0,_Dc));}),_GB=function(_GC){var _GD=E(_GC);return function(_8i,_8j){return new F(function(){return _3Q(function(_GE,_){var _GF=B(A(new T(function(){return B(_DE(_fB,_GD[5]));}),[_GE,_])),_GG=_GF,_GH=E(_GG),_GI=E(_GH[1]),_GJ=B(A(new T(function(){return B(_DE(_fD,_GD[1]));}),[_GH[2],_])),_GK=_GJ,_GL=E(_GK),_GM=E(_GL[1]),_GN=B(A(new T(function(){return B(_DE(_fA,_GD[2]));}),[_GL[2],_])),_GO=_GN,_GP=E(_GO),_GQ=E(_GP[1]),_GR=B(A(new T(function(){return B(_DE(_fz,_GD[3]));}),[_GP[2],_])),_GS=_GR,_GT=E(_GS),_GU=E(_GT[1]),_GV=B(A(new T(function(){return B(_DE(_fC,_GD[4]));}),[_GT[2],_])),_GW=_GV,_GX=E(_GW),_GY=E(_GX[1]);return [0,[0,function(_GZ,_){var _H0=B(A(_DW,[_GZ,_])),_H1=_H0,_H2=B(A(_E9,[_GZ,_])),_H3=_H2,_H4=B(A(_Gw,[_GZ,_])),_H5=_H4,_H6=B(A(_8,[_e,_H5,_fE,_fF,_])),_H7=_H6,_H8=B(A(_GI[1],[_GZ,_])),_H9=_H8,_Ha=B(_Dx(_GZ,_)),_Hb=_Ha,_Hc=B(A(_Gx,[_GZ,_])),_Hd=_Hc,_He=B(A(_8,[_e,_Hd,_fE,_fF,_])),_Hf=_He,_Hg=B(A(_GM[1],[_GZ,_])),_Hh=_Hg,_Hi=B(_Dx(_GZ,_)),_Hj=_Hi,_Hk=B(A(_Gy,[_GZ,_])),_Hl=_Hk,_Hm=B(A(_8,[_e,_Hl,_fE,_fF,_])),_Hn=_Hm,_Ho=B(A(_GQ[1],[_GZ,_])),_Hp=_Ho,_Hq=B(_Dx(_GZ,_)),_Hr=_Hq,_Hs=B(A(_Gz,[_GZ,_])),_Ht=_Hs,_Hu=B(A(_8,[_e,_Ht,_fE,_fF,_])),_Hv=_Hu,_Hw=B(A(_GU[1],[_GZ,_])),_Hx=_Hw,_Hy=B(_Dx(_GZ,_)),_Hz=_Hy,_HA=B(A(_GA,[_GZ,_])),_HB=_HA,_HC=B(A(_8,[_e,_HB,_fE,_fF,_])),_HD=_HC,_HE=B(A(_GY[1],[_GZ,_])),_HF=_HE,_HG=B(_Dx(_GZ,_)),_HH=_HG;return _GZ;},new T(function(){var _HI=E(_GI[2]);if(!_HI[0]){var _HJ=E(_GM[2]);if(!_HJ[0]){var _HK=E(_GQ[2]);if(!_HK[0]){var _HL=E(_GU[2]);if(!_HL[0]){var _HM=E(_GY[2]),_HN=_HM[0]==0?E(_Gt):E(_HM);}else{var _HN=E(_HL);}var _HO=_HN;}else{var _HO=E(_HK);}var _HP=_HO;}else{var _HP=E(_HJ);}var _HQ=_HP;}else{var _HQ=E(_HI);}return _HQ;})],_GX[2]];},function(_HR){return function(_8i,_8j){return new F(function(){return _3Q(new T(function(){return E(_HR)==5?function(_HS,_){return [0,[0,_2Q,[1,_GD]],_HS];}:E(_Du);}),function(_HT){var _HU=E(_HT),_HV=_HU[1],_HW=_HU[2],_HX=_HU[3],_HY=_HU[4],_HZ=_HU[5];return function(_8i,_8j){return new F(function(){return _3Q(function(_Dv,_){return new F(function(){return _3Q(_dR,function(_I0){return function(_I1,_){return [0,_Gv,new T(function(){var _I2=E(_I0);return [0,_I2[1],_I2[2],_I2[3],_I2[4],_I2[5],new T(function(){return B(_ff(new T(function(){return B(A(_dK,[_HU]));}),_HU,_I2[6]));})];})];};},_Dv,_);});},function(_I3,_Dv,_){return new F(function(){return (function(_Dv,_){return new F(function(){return _3Q(new T(function(){switch(E(_HR)){case 0:var _I4=new T(function(){return [0,E(_HZ)[1]-E(_HW)[1]-E(_HY)[1]-E(_HV)[1]];}),_I5=function(_8i,_8j){return new F(function(){return _3Q(new T(function(){return B(_fw(_Ea,_I4));}),function(_I6,_Dv,_){return new F(function(){return (function(_I7,_){return [0,[0,_2Q,[1,[0,_HZ,_HW,_HY,_HV,_I4]]],_I7];})(_Dv,_);});},_8i,_8j);});};break;case 1:var _I8=new T(function(){return [0,E(_HZ)[1]-E(_HW)[1]-E(_HY)[1]-E(_HV)[1]];}),_I5=function(_8i,_8j){return new F(function(){return _3Q(new T(function(){return B(_fw(_Ec,_I8));}),function(_I9,_Dv,_){return new F(function(){return (function(_Ia,_){return [0,[0,_2Q,[1,[0,_HZ,_HW,_HY,_HV,_I8]]],_Ia];})(_Dv,_);});},_8i,_8j);});};break;case 2:var _Ib=new T(function(){return [0,E(_HZ)[1]-E(_HW)[1]-E(_HX)[1]-E(_HV)[1]];}),_I5=function(_8i,_8j){return new F(function(){return _3Q(new T(function(){return B(_fw(_Ee,_Ib));}),function(_Ic,_Dv,_){return new F(function(){return (function(_Id,_){return [0,[0,_2Q,[1,[0,_HZ,_HW,_Ib,_HV,_HX]]],_Id];})(_Dv,_);});},_8i,_8j);});};break;case 3:var _Ie=new T(function(){return [0,E(_HZ)[1]-E(_HW)[1]-E(_HY)[1]-E(_HV)[1]];}),_I5=function(_8i,_8j){return new F(function(){return _3Q(new T(function(){return B(_fw(_Eg,_Ie));}),function(_If,_Dv,_){return new F(function(){return (function(_Ig,_){return [0,[0,_2Q,[1,[0,_HZ,_HW,_HY,_HV,_Ie]]],_Ig];})(_Dv,_);});},_8i,_8j);});};break;case 4:var _Ih=new T(function(){return [0,E(_HZ)[1]-E(_HW)[1]-E(_HY)[1]-E(_HV)[1]];}),_I5=function(_8i,_8j){return new F(function(){return _3Q(new T(function(){return B(_fw(_Ei,_Ih));}),function(_Ii,_Dv,_){return new F(function(){return (function(_Ij,_){return [0,[0,_2Q,[1,[0,_HZ,_HW,_HY,_HV,_Ih]]],_Ij];})(_Dv,_);});},_8i,_8j);});};break;default:var _I5=function(_Ik,_){return [0,[0,_2Q,[1,[0,_HZ,_HW,_HY,_HV,_HX]]],_Ik];};}return _I5;}),_Gp,_Dv,_);});})(_Dv,_);});},_8i,_8j);});};},_8i,_8j);});};},_8i,_8j);});};},_Il=function(_Dv,_){return new F(function(){return _3Q(_dM,_GB,_Dv,_);});},_Im=[0,34],_In=new T(function(){return B(unCStr("Entry {"));}),_Io=new T(function(){return B(unCStr("day = "));}),_Ip=new T(function(){return B(unCStr(", "));}),_Iq=new T(function(){return B(unCStr("month = "));}),_Ir=new T(function(){return B(unCStr("year = "));}),_Is=new T(function(){return B(unCStr("description = "));}),_It=new T(function(){return B(unCStr("amount = "));}),_Iu=[0,0],_Iv=new T(function(){return B(unCStr("etype = "));}),_Iw=[0,125],_Ix=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_Iy=new T(function(){return B(err(_Ix));}),_Iz=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_IA=new T(function(){return B(err(_Iz));}),_IB=function(_IC,_ID){while(1){var _IE=E(_IC);if(!_IE[0]){return E(_IA);}else{var _IF=E(_ID);if(!_IF){return E(_IE[1]);}else{_IC=_IE[2];_ID=_IF-1|0;continue;}}}},_IG=new T(function(){return B(unCStr("ACK"));}),_IH=new T(function(){return B(unCStr("BEL"));}),_II=new T(function(){return B(unCStr("BS"));}),_IJ=new T(function(){return B(unCStr("SP"));}),_IK=[1,_IJ,_X],_IL=new T(function(){return B(unCStr("US"));}),_IM=[1,_IL,_IK],_IN=new T(function(){return B(unCStr("RS"));}),_IO=[1,_IN,_IM],_IP=new T(function(){return B(unCStr("GS"));}),_IQ=[1,_IP,_IO],_IR=new T(function(){return B(unCStr("FS"));}),_IS=[1,_IR,_IQ],_IT=new T(function(){return B(unCStr("ESC"));}),_IU=[1,_IT,_IS],_IV=new T(function(){return B(unCStr("SUB"));}),_IW=[1,_IV,_IU],_IX=new T(function(){return B(unCStr("EM"));}),_IY=[1,_IX,_IW],_IZ=new T(function(){return B(unCStr("CAN"));}),_J0=[1,_IZ,_IY],_J1=new T(function(){return B(unCStr("ETB"));}),_J2=[1,_J1,_J0],_J3=new T(function(){return B(unCStr("SYN"));}),_J4=[1,_J3,_J2],_J5=new T(function(){return B(unCStr("NAK"));}),_J6=[1,_J5,_J4],_J7=new T(function(){return B(unCStr("DC4"));}),_J8=[1,_J7,_J6],_J9=new T(function(){return B(unCStr("DC3"));}),_Ja=[1,_J9,_J8],_Jb=new T(function(){return B(unCStr("DC2"));}),_Jc=[1,_Jb,_Ja],_Jd=new T(function(){return B(unCStr("DC1"));}),_Je=[1,_Jd,_Jc],_Jf=new T(function(){return B(unCStr("DLE"));}),_Jg=[1,_Jf,_Je],_Jh=new T(function(){return B(unCStr("SI"));}),_Ji=[1,_Jh,_Jg],_Jj=new T(function(){return B(unCStr("SO"));}),_Jk=[1,_Jj,_Ji],_Jl=new T(function(){return B(unCStr("CR"));}),_Jm=[1,_Jl,_Jk],_Jn=new T(function(){return B(unCStr("FF"));}),_Jo=[1,_Jn,_Jm],_Jp=new T(function(){return B(unCStr("VT"));}),_Jq=[1,_Jp,_Jo],_Jr=new T(function(){return B(unCStr("LF"));}),_Js=[1,_Jr,_Jq],_Jt=new T(function(){return B(unCStr("HT"));}),_Ju=[1,_Jt,_Js],_Jv=[1,_II,_Ju],_Jw=[1,_IH,_Jv],_Jx=[1,_IG,_Jw],_Jy=new T(function(){return B(unCStr("ENQ"));}),_Jz=[1,_Jy,_Jx],_JA=new T(function(){return B(unCStr("EOT"));}),_JB=[1,_JA,_Jz],_JC=new T(function(){return B(unCStr("ETX"));}),_JD=[1,_JC,_JB],_JE=new T(function(){return B(unCStr("STX"));}),_JF=[1,_JE,_JD],_JG=new T(function(){return B(unCStr("SOH"));}),_JH=[1,_JG,_JF],_JI=new T(function(){return B(unCStr("NUL"));}),_JJ=[1,_JI,_JH],_JK=[0,92],_JL=new T(function(){return B(unCStr("\\DEL"));}),_JM=new T(function(){return B(unCStr("\\a"));}),_JN=new T(function(){return B(unCStr("\\\\"));}),_JO=new T(function(){return B(unCStr("\\SO"));}),_JP=new T(function(){return B(unCStr("\\r"));}),_JQ=new T(function(){return B(unCStr("\\f"));}),_JR=new T(function(){return B(unCStr("\\v"));}),_JS=new T(function(){return B(unCStr("\\n"));}),_JT=new T(function(){return B(unCStr("\\t"));}),_JU=new T(function(){return B(unCStr("\\b"));}),_JV=function(_JW,_JX){if(_JW<=127){var _JY=E(_JW);switch(_JY){case 92:return new F(function(){return _1q(_JN,_JX);});break;case 127:return new F(function(){return _1q(_JL,_JX);});break;default:if(_JY<32){var _JZ=E(_JY);switch(_JZ){case 7:return new F(function(){return _1q(_JM,_JX);});break;case 8:return new F(function(){return _1q(_JU,_JX);});break;case 9:return new F(function(){return _1q(_JT,_JX);});break;case 10:return new F(function(){return _1q(_JS,_JX);});break;case 11:return new F(function(){return _1q(_JR,_JX);});break;case 12:return new F(function(){return _1q(_JQ,_JX);});break;case 13:return new F(function(){return _1q(_JP,_JX);});break;case 14:return new F(function(){return _1q(_JO,new T(function(){var _K0=E(_JX);if(!_K0[0]){var _K1=[0];}else{var _K1=E(E(_K0[1])[1])==72?B(unAppCStr("\\&",_K0)):E(_K0);}return _K1;}));});break;default:return new F(function(){return _1q([1,_JK,new T(function(){var _K2=_JZ;return _K2>=0?B(_IB(_JJ,_K2)):E(_Iy);})],_JX);});}}else{return [1,[0,_JY],_JX];}}}else{return [1,_JK,new T(function(){var _K3=jsShowI(_JW),_K4=_K3;return B(_1q(fromJSStr(_K4),new T(function(){var _K5=E(_JX);if(!_K5[0]){var _K6=[0];}else{var _K7=E(_K5[1])[1],_K6=_K7<48?E(_K5):_K7>57?E(_K5):B(unAppCStr("\\&",_K5));}return _K6;})));})];}},_K8=new T(function(){return B(unCStr("\\\""));}),_K9=function(_Ka,_Kb){var _Kc=E(_Ka);if(!_Kc[0]){return E(_Kb);}else{var _Kd=_Kc[2],_Ke=E(E(_Kc[1])[1]);if(_Ke==34){return new F(function(){return _1q(_K8,new T(function(){return B(_K9(_Kd,_Kb));}));});}else{return new F(function(){return _JV(_Ke,new T(function(){return B(_K9(_Kd,_Kb));}));});}}},_Kf=function(_Kg,_Kh,_Ki,_Kj,_Kk,_Kl,_Km){var _Kn=function(_Ko){return new F(function(){return _1q(_Io,new T(function(){return B(_3x(0,E(_Kh)[1],new T(function(){return B(_1q(_Ip,new T(function(){return B(_1q(_Iq,new T(function(){return B(_3x(0,E(_Ki)[1],new T(function(){return B(_1q(_Ip,new T(function(){return B(_1q(_Ir,new T(function(){return B(_3x(0,E(_Kj)[1],new T(function(){return B(_1q(_Ip,new T(function(){return B(_1q(_Is,[1,_Im,new T(function(){return B(_K9(_Kk,[1,_Im,new T(function(){return B(_1q(_Ip,new T(function(){return B(_1q(_It,new T(function(){return B(A(new T(function(){return B(_vU(_vR,_Iu,E(_Kl)[1]));}),[new T(function(){return B(_1q(_Ip,new T(function(){return B(_1q(_Iv,new T(function(){switch(E(_Km)){case 0:var _Kp=B(_1q(_vM,[1,_Iw,_Ko]));break;case 1:var _Kp=B(_1q(_D2,[1,_Iw,_Ko]));break;case 2:var _Kp=B(_1q(_D7,[1,_Iw,_Ko]));break;case 3:var _Kp=B(_1q(_Dc,[1,_Iw,_Ko]));break;case 4:var _Kp=B(_1q(_Dh,[1,_Iw,_Ko]));break;default:var _Kp=B(_1q(_DD,[1,_Iw,_Ko]));}return _Kp;})));})));})]));})));})));})]));})]));})));})));})));})));})));})));})));})));}));});};return _Kg<11?function(_Kq){return new F(function(){return _1q(_In,new T(function(){return B(_Kn(_Kq));}));});}:function(_Kr){return [1,_3w,new T(function(){return B(_1q(_In,new T(function(){return B(_Kn([1,_3v,_Kr]));})));})];};},_Ks=function(_Kt){return [1,new T(function(){var _Ku=E(_Kt);return [0,toJSStr(B(A(_Kf,[0,_Ku[1],_Ku[2],_Ku[3],_Ku[4],_Ku[5],_Ku[6],_X])))];})];},_Kv=function(_Kw){return [3,new T(function(){return B(_c1(_Ks,_Kw));})];},_Kx=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_Ky=new T(function(){return B(err(_Kx));}),_Kz=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_KA=new T(function(){return B(err(_Kz));}),_KB=function(_KC,_KD){return new F(function(){return A(_KD,[_fD]);});},_KE=[0,_vM,_KB],_KF=function(_KG,_KH){return new F(function(){return A(_KH,[_fA]);});},_KI=[0,_D2,_KF],_KJ=function(_KK,_KL){return new F(function(){return A(_KL,[_fz]);});},_KM=[0,_D7,_KJ],_KN=function(_KO,_KP){return new F(function(){return A(_KP,[_fC]);});},_KQ=[0,_Dc,_KN],_KR=function(_KS,_KT){return new F(function(){return A(_KT,[_fB]);});},_KU=[0,_Dh,_KR],_KV=function(_KW,_KX){return new F(function(){return A(_KX,[_Gs]);});},_KY=[0,_DD,_KV],_KZ=[1,_KY,_X],_L0=[1,_KU,_KZ],_L1=[1,_KQ,_L0],_L2=[1,_KM,_L1],_L3=[1,_KI,_L2],_L4=[1,_KE,_L3],_L5=function(_L6,_L7,_L8){var _L9=E(_L6);if(!_L9[0]){return [2];}else{var _La=E(_L9[1]),_Lb=_La[1],_Lc=new T(function(){return B(A(_La[2],[_L7,_L8]));});return new F(function(){return _gI(B(_qX(function(_Ld){var _Le=E(_Ld);switch(_Le[0]){case 3:return !B(_5O(_Lb,_Le[1]))?[2]:E(_Lc);case 4:return !B(_5O(_Lb,_Le[1]))?[2]:E(_Lc);default:return [2];}})),new T(function(){return B(_L5(_L9[2],_L7,_L8));}));});}},_Lf=function(_Lg,_Lh){return new F(function(){return _L5(_L4,_Lg,_Lh);});},_Li=function(_Lj,_Lk,_Ll){var _Lm=function(_Ln,_Lo){return new F(function(){return _gI(B(_qX(function(_Lp){var _Lq=E(_Lp);if(_Lq[0]==4){var _Lr=E(_Lq[1]);if(!_Lr[0]){return new F(function(){return A(_Lj,[_Lq,_Ln,_Lo]);});}else{return E(E(_Lr[1])[1])==45?E(_Lr[2])[0]==0?E([1,function(_Ls){return new F(function(){return A(_p5,[_Ls,function(_Lt){return E(new T(function(){return B(_qx(function(_Lu){return new F(function(){return A(_Lj,[_Lu,_Ln,function(_Lv){return new F(function(){return A(_Lo,[new T(function(){return [0, -E(_Lv)[1]];})]);});}]);});}));}));}]);});}]):B(A(_Lj,[_Lq,_Ln,_Lo])):B(A(_Lj,[_Lq,_Ln,_Lo]));}}else{return new F(function(){return A(_Lj,[_Lq,_Ln,_Lo]);});}})),new T(function(){return B(_r2(_Lm,_Lo));}));});};return new F(function(){return _Lm(_Lk,_Ll);});},_Lw=function(_Lx,_Ly){return [2];},_Lz=function(_rw,_rx){return new F(function(){return _Lw(_rw,_rx);});},_LA=function(_LB){var _LC=E(_LB);return _LC[0]==0?[1,new T(function(){return B(_jl(new T(function(){return B(_jb(E(_LC[1])[1]));}),_ja,_LC[2]));})]:E(_LC[2])[0]==0?E(_LC[3])[0]==0?[1,new T(function(){return B(_jl(_j9,_ja,_LC[1]));})]:[0]:[0];},_LD=function(_LE){var _LF=E(_LE);if(_LF[0]==5){var _LG=B(_LA(_LF[1]));return _LG[0]==0?E(_Lw):function(_LH,_LI){return new F(function(){return A(_LI,[new T(function(){return [0,B(_kd(_LG[1]))];})]);});};}else{return E(_Lz);}},_LJ=function(_LK,_LL){return new F(function(){return _LM(_LL);});},_LN=function(_LO){return new F(function(){return _gI(B(_qX(function(_LP){var _LQ=E(_LP);return _LQ[0]==0?B(A(_LO,[_LQ[1]])):[2];})),new T(function(){return B(_r2(_LR,_LO));}));});},_LR=function(_LS,_LT){return new F(function(){return _LN(_LT);});},_LM=function(_LU){return new F(function(){return _gI(B(_gI(B(_qX(function(_LV){var _LW=E(_LV);return _LW[0]==1?B(A(_LU,[_LW[1]])):[2];})),new T(function(){return B(_vj(_LR,_LU));}))),new T(function(){return B(_r2(_LJ,_LU));}));});},_LX=new T(function(){return B(unCStr("etype"));}),_LY=new T(function(){return B(unCStr("amount"));}),_LZ=new T(function(){return B(unCStr("description"));}),_M0=new T(function(){return B(unCStr("year"));}),_M1=new T(function(){return B(unCStr("month"));}),_M2=new T(function(){return B(unCStr("day"));}),_M3=new T(function(){return B(unCStr("Entry"));}),_M4=function(_M5,_M6){var _M7=function(_M8){return function(_M9){return new F(function(){return _gI(B(A(new T(function(){return B(A(_M5,[_M8]));}),[_M9])),new T(function(){return B(_r2(_M7,_M9));}));});};};return new F(function(){return _M7(_M6);});},_Ma=function(_Mb,_Mc){if(_Mb>11){return [2];}else{return new F(function(){return _qX(function(_Md){var _Me=E(_Md);return _Me[0]==3?!B(_5O(_Me[1],_M3))?[2]:E([1,function(_Mf){return new F(function(){return A(_p5,[_Mf,function(_Mg){return E(new T(function(){return B(_qx(function(_Mh){var _Mi=E(_Mh);if(_Mi[0]==2){var _Mj=E(_Mi[1]);return _Mj[0]==0?[2]:E(E(_Mj[1])[1])==123?E(_Mj[2])[0]==0?E([1,function(_Mk){return new F(function(){return A(_p5,[_Mk,function(_Ml){return E(new T(function(){return B(_qx(function(_Mm){var _Mn=E(_Mm);return _Mn[0]==3?!B(_5O(_Mn[1],_M2))?[2]:E([1,function(_Mo){return new F(function(){return A(_p5,[_Mo,function(_Mp){return E(new T(function(){return B(_qx(function(_Mq){var _Mr=E(_Mq);if(_Mr[0]==2){var _Ms=E(_Mr[1]);return _Ms[0]==0?[2]:E(E(_Ms[1])[1])==61?E(_Ms[2])[0]==0?E(new T(function(){return B(_Li(_LD,_r1,function(_Mt){return new F(function(){return _qX(function(_Mu){var _Mv=E(_Mu);if(_Mv[0]==2){var _Mw=E(_Mv[1]);return _Mw[0]==0?[2]:E(E(_Mw[1])[1])==44?E(_Mw[2])[0]==0?E([1,function(_Mx){return new F(function(){return A(_p5,[_Mx,function(_My){return E(new T(function(){return B(_qx(function(_Mz){var _MA=E(_Mz);return _MA[0]==3?!B(_5O(_MA[1],_M1))?[2]:E([1,function(_MB){return new F(function(){return A(_p5,[_MB,function(_MC){return E(new T(function(){return B(_qx(function(_MD){var _ME=E(_MD);if(_ME[0]==2){var _MF=E(_ME[1]);return _MF[0]==0?[2]:E(E(_MF[1])[1])==61?E(_MF[2])[0]==0?E(new T(function(){return B(_Li(_LD,_r1,function(_MG){return new F(function(){return _qX(function(_MH){var _MI=E(_MH);if(_MI[0]==2){var _MJ=E(_MI[1]);return _MJ[0]==0?[2]:E(E(_MJ[1])[1])==44?E(_MJ[2])[0]==0?E([1,function(_MK){return new F(function(){return A(_p5,[_MK,function(_ML){return E(new T(function(){return B(_qx(function(_MM){var _MN=E(_MM);return _MN[0]==3?!B(_5O(_MN[1],_M0))?[2]:E([1,function(_MO){return new F(function(){return A(_p5,[_MO,function(_MP){return E(new T(function(){return B(_qx(function(_MQ){var _MR=E(_MQ);if(_MR[0]==2){var _MS=E(_MR[1]);return _MS[0]==0?[2]:E(E(_MS[1])[1])==61?E(_MS[2])[0]==0?E(new T(function(){return B(_Li(_LD,_r1,function(_MT){return new F(function(){return _qX(function(_MU){var _MV=E(_MU);if(_MV[0]==2){var _MW=E(_MV[1]);return _MW[0]==0?[2]:E(E(_MW[1])[1])==44?E(_MW[2])[0]==0?E([1,function(_MX){return new F(function(){return A(_p5,[_MX,function(_MY){return E(new T(function(){return B(_qx(function(_MZ){var _N0=E(_MZ);return _N0[0]==3?!B(_5O(_N0[1],_LZ))?[2]:E([1,function(_N1){return new F(function(){return A(_p5,[_N1,function(_N2){return E(new T(function(){return B(_qx(function(_N3){var _N4=E(_N3);if(_N4[0]==2){var _N5=E(_N4[1]);return _N5[0]==0?[2]:E(E(_N5[1])[1])==61?E(_N5[2])[0]==0?E(new T(function(){return B(_LM(function(_N6){return new F(function(){return _qX(function(_N7){var _N8=E(_N7);if(_N8[0]==2){var _N9=E(_N8[1]);return _N9[0]==0?[2]:E(E(_N9[1])[1])==44?E(_N9[2])[0]==0?E([1,function(_Na){return new F(function(){return A(_p5,[_Na,function(_Nb){return E(new T(function(){return B(_qx(function(_Nc){var _Nd=E(_Nc);return _Nd[0]==3?!B(_5O(_Nd[1],_LY))?[2]:E([1,function(_Ne){return new F(function(){return A(_p5,[_Ne,function(_Nf){return E(new T(function(){return B(_qx(function(_Ng){var _Nh=E(_Ng);if(_Nh[0]==2){var _Ni=E(_Nh[1]);return _Ni[0]==0?[2]:E(E(_Ni[1])[1])==61?E(_Ni[2])[0]==0?E(new T(function(){return B(_rc(_va,_r1,function(_Nj){return new F(function(){return _qX(function(_Nk){var _Nl=E(_Nk);if(_Nl[0]==2){var _Nm=E(_Nl[1]);return _Nm[0]==0?[2]:E(E(_Nm[1])[1])==44?E(_Nm[2])[0]==0?E([1,function(_Nn){return new F(function(){return A(_p5,[_Nn,function(_No){return E(new T(function(){return B(_qx(function(_Np){var _Nq=E(_Np);return _Nq[0]==3?!B(_5O(_Nq[1],_LX))?[2]:E([1,function(_Nr){return new F(function(){return A(_p5,[_Nr,function(_Ns){return E(new T(function(){return B(_qx(function(_Nt){var _Nu=E(_Nt);if(_Nu[0]==2){var _Nv=E(_Nu[1]);return _Nv[0]==0?[2]:E(E(_Nv[1])[1])==61?E(_Nv[2])[0]==0?E(new T(function(){return B(A(_M4,[_Lf,_r1,function(_Nw){return new F(function(){return _qX(function(_Nx){var _Ny=E(_Nx);if(_Ny[0]==2){var _Nz=E(_Ny[1]);return _Nz[0]==0?[2]:E(E(_Nz[1])[1])==125?E(_Nz[2])[0]==0?E(new T(function(){return B(A(_Mc,[[0,_Mt,_MG,_MT,_N6,_Nj,_Nw]]));})):[2]:[2];}else{return [2];}});});}]));})):[2]:[2];}else{return [2];}}));}));}]);});}]):[2];}));}));}]);});}]):[2]:[2];}else{return [2];}});});}));})):[2]:[2];}else{return [2];}}));}));}]);});}]):[2];}));}));}]);});}]):[2]:[2];}else{return [2];}});});}));})):[2]:[2];}else{return [2];}}));}));}]);});}]):[2];}));}));}]);});}]):[2]:[2];}else{return [2];}});});}));})):[2]:[2];}else{return [2];}}));}));}]);});}]):[2];}));}));}]);});}]):[2]:[2];}else{return [2];}});});}));})):[2]:[2];}else{return [2];}}));}));}]);});}]):[2];}));}));}]);});}]):[2]:[2];}else{return [2];}});});}));})):[2]:[2];}else{return [2];}}));}));}]);});}]):[2];}));}));}]);});}]):[2]:[2];}else{return [2];}}));}));}]);});}]):[2];});});}},_NA=function(_NB,_NC){return new F(function(){return _Ma(E(_NB)[1],_NC);});},_ND=function(_NE){return [1,function(_NF){return new F(function(){return A(_p5,[_NF,function(_NG){return E([3,_NE,_hS]);}]);});}];},_NH=new T(function(){return B(A(_M4,[_NA,_r1,_ND]));}),_NI=new T(function(){return B(_gv("mybudget.hs:29:3-54|function parseJSON"));}),_NJ=function(_NK){return new F(function(){return fromJSStr(E(_NK)[1]);});},_NL=function(_NM){while(1){var _NN=(function(_NO){var _NP=E(_NO);if(!_NP[0]){return [0];}else{var _NQ=_NP[2],_NR=E(_NP[1]);if(!E(_NR[2])[0]){return [1,_NR[1],new T(function(){return B(_NL(_NQ));})];}else{_NM=_NQ;return null;}}})(_NM);if(_NN!=null){return _NN;}}},_NS=function(_NT){var _NU=E(_NT);return _NU[0]==1?[1,new T(function(){var _NV=B(_NL(B(_gy(_NH,new T(function(){return B(_NJ(_NU[1]));})))));return _NV[0]==0?E(_KA):E(_NV[2])[0]==0?E(_NV[1]):E(_Ky);})]:E(_NI);},_NW=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_NX=[0,_NW],_NY=[1,_X],_NZ=function(_O0){var _O1=E(_O0);if(!_O1[0]){return E(_NY);}else{var _O2=B(_NS(_O1[1]));if(!_O2[0]){return [0,_O2[1]];}else{var _O3=B(_NZ(_O1[2]));return _O3[0]==0?[0,_O3[1]]:[1,[1,_O2[1],_O3[1]]];}}},_O4=function(_O5){var _O6=E(_O5);return _O6[0]==3?B(_NZ(_O6[1])):E(_NX);},_O7=[0,_Ks,_Kv,_NS,_O4],_O8=function(_O9){return E(E(_O9)[2]);},_Oa=function(_Ob,_Oc){return [3,new T(function(){return B(_c1(new T(function(){return B(_O8(_Ob));}),_Oc));})];},_Od=[1,_X],_Oe=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_Of=[0,_Oe],_Og=function(_Oh){return E(E(_Oh)[4]);},_Oi=function(_Oj,_Ok){var _Ol=E(_Ok);if(_Ol[0]==3){var _Om=function(_On){var _Oo=E(_On);if(!_Oo[0]){return E(_Od);}else{var _Op=B(A(new T(function(){return B(_Og(_Oj));}),[_Oo[1]]));if(!_Op[0]){return [0,_Op[1]];}else{var _Oq=B(_Om(_Oo[2]));return _Oq[0]==0?[0,_Oq[1]]:[1,[1,_Op[1],_Oq[1]]];}}};return new F(function(){return _Om(_Ol[1]);});}else{return E(_Of);}},_Or=function(_Os){return [0,new T(function(){return B(_O8(_Os));}),function(_Ot){return new F(function(){return _Oa(_Os,_Ot);});},new T(function(){return B(_Og(_Os));}),function(_Ot){return new F(function(){return _Oi(_Os,_Ot);});}];},_Ou=new T(function(){return B(_Or(_O7));}),_Ov=function(_Ow){return _Ow>0;},_Ox=new T(function(){return [0,"(function(x) {return x === null;})"];}),_Oy=new T(function(){return B(_50(_Ox));}),_Oz=new T(function(){return B(unCStr("No such value"));}),_OA=[0,_Oz],_OB=new T(function(){return B(unCStr("Invalid JSON!"));}),_OC=[0,_OB],_OD=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_OE=function(_OF){return E(E(_OF)[3]);},_OG=function(_OH,_OI,_){var _OJ=B(A(_50,[_OD,E(toJSStr(E(_OI))),_])),_OK=_OJ;return new T(function(){if(!B(_4W(function(_){var _=0,_OL=B(A(_Oy,[E(_OK),_])),_OM=_OL;return new T(function(){return B(_Ov(_OM));});}))){var _ON=String(_OK),_OO=_ON,_OP=jsParseJSON(_OO),_OQ=_OP,_OR=E(_OQ),_OS=_OR[0]==0?E(_OC):B(A(_OE,[_OH,_OR[1]]));}else{var _OS=E(_OA);}return _OS;});},_OT=new T(function(){return B(unCStr("budget"));}),_OU=[1,_X],_OV=[0,_2Q,_OU],_OW=function(_OX,_){var _OY=B(_OG(_Ou,_OT,_)),_OZ=_OY,_P0=E(_OZ);return _P0[0]==0?[0,_OV,_OX]:[0,[0,_2Q,[1,_P0[1]]],_OX];},_P1=function(_P2){return new F(function(){return _3x(0,E(_P2)[1],_X);});},_P3=function(_P4){return function(_8i,_8j){return new F(function(){return _0(new T(function(){return B(_P1(_P4));}),_8i,_8j);});};},_P5=[2,_],_P6=new T(function(){return B(unCStr("size"));}),_P7=new T(function(){return B(unCStr("40"));}),_P8=new T(function(){return B(unCStr("text"));}),_P9=new T(function(){return B(_r2(_LJ,_hT));}),_Pa=new T(function(){return B(_vj(_LR,_hT));}),_Pb=function(_Pc){var _Pd=E(_Pc);return _Pd[0]==1?[3,_Pd[1],_hS]:[2];},_Pe=new T(function(){return B(_qx(_Pb));}),_Pf=function(_Pg){return E(_Pe);},_Ph=function(_Pi){return new F(function(){return A(_p5,[_Pi,_Pf]);});},_Pj=[1,_Ph],_Pk=new T(function(){return B(_gI(_Pj,_Pa));}),_Pl=new T(function(){return B(_gI(_Pk,_P9));}),_Pm=function(_rx){return new F(function(){return _gy(_Pl,_rx);});},_Pn=new T(function(){return B(_LN(_hT));}),_Po=function(_rx){return new F(function(){return _gy(_Pn,_rx);});},_Pp=function(_Pq){return E(_Po);},_Pr=[0,_Pp,_Pm,_LR,_LJ],_Ps=function(_Pt){return E(E(_Pt)[4]);},_Pu=function(_Pv,_Pw,_Px){return new F(function(){return _vj(new T(function(){return B(_Ps(_Pv));}),_Px);});},_Py=function(_Pz){return function(_8i){return new F(function(){return _gy(new T(function(){return B(_vj(new T(function(){return B(_Ps(_Pz));}),_hT));}),_8i);});};},_PA=function(_PB,_PC){return function(_8i){return new F(function(){return _gy(new T(function(){return B(A(_Ps,[_PB,_PC,_hT]));}),_8i);});};},_PD=function(_PE){return [0,function(_rx){return new F(function(){return _PA(_PE,_rx);});},new T(function(){return B(_Py(_PE));}),new T(function(){return B(_Ps(_PE));}),function(_rw,_rx){return new F(function(){return _Pu(_PE,_rw,_rx);});}];},_PF=new T(function(){return B(_PD(_Pr));}),_PG=[0,39],_PH=[1,_PG,_X],_PI=new T(function(){return B(unCStr("\'\\\'\'"));}),_PJ=function(_PK){var _PL=E(E(_PK)[1]);return _PL==39?E(_PI):[1,_PG,new T(function(){return B(_JV(_PL,_PH));})];},_PM=function(_PN,_PO){return [1,_Im,new T(function(){return B(_K9(_PN,[1,_Im,_PO]));})];},_PP=function(_PQ){return new F(function(){return _1q(_PI,_PQ);});},_PR=function(_PS,_PT){var _PU=E(E(_PT)[1]);return _PU==39?E(_PP):function(_PV){return [1,_PG,new T(function(){return B(_JV(_PU,[1,_PG,_PV]));})];};},_PW=[0,_PR,_PJ,_PM],_PX=function(_PY){return E(E(_PY)[3]);},_PZ=function(_Q0,_Q1){return new F(function(){return A(_PX,[_Q0,_Q1,_X]);});},_Q2=function(_Q3,_Q4,_Q5){return new F(function(){return _2g(new T(function(){return B(_PX(_Q3));}),_Q4,_Q5);});},_Q6=function(_Q7){return [0,function(_Q8){return E(new T(function(){return B(_PX(_Q7));}));},function(_PQ){return new F(function(){return _PZ(_Q7,_PQ);});},function(_Q9,_PQ){return new F(function(){return _Q2(_Q7,_Q9,_PQ);});}];},_Qa=new T(function(){return B(_Q6(_PW));}),_Qb=new T(function(){return B(_Bv(_yz,_yA,_zb,_Qa,_PF));}),_Qc=new T(function(){return B(A(_Qb,[_2A,_P8,_2A]));}),_Qd=function(_Qe,_){var _Qf=B(A(_Qc,[_Qe,_])),_Qg=_Qf,_Qh=E(_Qg),_Qi=E(_Qh[1]);return [0,[0,function(_Qj,_){var _Qk=B(A(_Qi[1],[_Qj,_])),_Ql=_Qk,_Qm=B(A(_8,[_e,_Ql,_P6,_P7,_])),_Qn=_Qm;return _Ql;},_Qi[2]],_Qh[2]];},_Qo=new T(function(){return B(_8m(_Qd,_P5));}),_Qp=new T(function(){return B(unCStr("Enter description: "));}),_Qq=new T(function(){return B(_EE(_0,_Qp));}),_Qr=function(_Qs,_){var _Qt=B(A(_Qo,[_Qs,_])),_Qu=_Qt,_Qv=E(_Qu),_Qw=E(_Qv[1]);return [0,[0,function(_Qx,_){var _Qy=B(_Dx(_Qx,_)),_Qz=_Qy,_QA=B(A(_Qq,[_Qx,_])),_QB=_QA,_QC=B(A(_8,[_e,_QB,_fE,_fF,_])),_QD=_QC,_QE=B(A(_Qw[1],[_Qx,_])),_QF=_QE,_QG=B(_Dx(_Qx,_)),_QH=_QG;return _Qx;},_Qw[2]],_Qv[2]];},_QI=new T(function(){return B(unCStr("maxlength"));}),_QJ=new T(function(){return B(unCStr("wrong"));}),_QK=new T(function(){return B(_FW(_0,_QJ));}),_QL=[1,_QK],_QM=function(_QN,_QO,_){return [0,new T(function(){var _QP=E(_QN)[1];return _QP<=1?E(_QL):_QP>=12?E(_QL):[0];}),_QO];},_QQ=[0,52],_QR=[1,_QQ,_X],_QS=[1,_QQ,_X],_QT=[0,50],_QU=[1,_QT,_X],_QV=[1,_QT,_X],_QW=new T(function(){return B(_FW(_0,_QJ));}),_QX=[1,_QW],_QY=function(_QZ,_R0,_){return [0,new T(function(){var _R1=E(_QZ)[1];return _R1<=1?E(_QX):_R1>=31?E(_QX):[0];}),_R0];},_R2=[1,_QT,_X],_R3=[1,_QT,_X],_R4=[0,_2Q,_2A],_R5=[0,_2Q,_4E],_R6=function(_R7,_R8,_){return [0,_R5,_R7];},_R9=function(_Ra,_Rb,_){return [0,[0,_2Q,[1,_Ra]],_Rb];},_Rc=function(_Rd,_Re,_Rf,_){return new F(function(){return _3Q(_Rd,function(_Rg){return E(_Re);},_Rf,_);});},_Rh=function(_Ri,_Rj,_w,_){return new F(function(){return _Rc(_Ri,_Rj,_w,_);});},_Rk=function(_Rl){return function(_Rm,_){return [0,[0,new T(function(){return B(_p(function(_Rn,_){var _Ro=B(_y(_ya,_Rn,_)),_Rp=_Ro,_Rq=B(_0(_Rl,_Rp,_)),_Rr=_Rq;return _Rp;},_y8));}),_2A],_Rm];};},_Rs=[0,_3Q,_Rh,_R9,_Rk],_Rt=[0,_],_Ru=new T(function(){return B(_3D(_Rt,_Rs,_dR,_R6));}),_Rv=new T(function(){return B(_EE(_Fu,_2Q));}),_Rw=function(_Rx,_Ry,_Rz,_){var _RA=B(_3Q(_Ru,function(_RB){return function(_8i,_8j){return new F(function(){return _3Q(function(_RC,_){return [0,[0,function(_RD,_){var _RE=B(A(_Rv,[_RD,_])),_RF=_RE,_RG=B(A(_8,[_e,_RF,_Ey,_RB,_])),_RH=_RG;return _RF;},_4E],_RC];},function(_RI,_w,_){return new F(function(){return (function(_w,_){return new F(function(){return _3Q(_Rx,function(_RJ){return function(_RK,_){var _RL=B(A(new T(function(){return B(A(_Ry,[_RJ]));}),[_RK,_])),_RM=_RL,_RN=E(_RM),_RO=_RN[2],_RP=E(_RN[1]);if(!_RP[0]){var _RQ=E(_RB),_RR=jsFind(toJSStr(_RQ)),_RS=_RR,_RT=E(_RS);if(!_RT[0]){return new F(function(){return _Cj(_RQ);});}else{var _RU=jsClearChildren(E(_RT[1])[1]);return [0,[0,_2Q,[1,_RJ]],_RO];}}else{var _RV=E(_RB),_RW=jsFind(toJSStr(_RV)),_RX=_RW,_RY=E(_RX);if(!_RY[0]){return new F(function(){return _Cj(_RV);});}else{var _RZ=E(_RY[1]),_S0=jsClearChildren(_RZ[1]),_S1=B(A(_p,[function(_S2,_){var _S3=B(_y(_ya,_S2,_)),_S4=_S3,_S5=B(A(_RP[1],[_S4,_])),_S6=_S5;return _S4;},_y8,_RZ,_])),_S7=_S1;return [0,_R4,_RO];}}};},_w,_);});})(_w,_);});},_8i,_8j);});};},new T(function(){var _S8=E(_Rz);return [0,_S8[1],_S8[2],_S8[3],_S8[4],_6H,_S8[6]];}),_)),_S9=_RA;return [0,new T(function(){return E(E(_S9)[1]);}),new T(function(){var _Sa=E(E(_S9)[2]);return [0,_Sa[1],_Sa[2],_Sa[3],_Sa[4],new T(function(){return E(E(_Rz)[5]);}),_Sa[6]];})];},_Sb=new T(function(){return B(A(_Bv,[_yz,_yA,_zb,_Qa,_PF,_2A,_9C]));}),_Sc=new T(function(){return B(unCStr("Ok"));}),_Sd=[1,_Sc],_Se=new T(function(){return B(A(_Sb,[_Sd]));}),_Sf=new T(function(){return B(_8m(_Se,_5o));}),_Sg=function(_rw,_rx){return new F(function(){return _Li(_LD,_rw,_rx);});},_Sh=function(_Si,_Sj){return new F(function(){return _vj(_Sg,_Sj);});},_Sk=new T(function(){return B(_vj(_Sg,_hT));}),_Sl=function(_rx){return new F(function(){return _gy(_Sk,_rx);});},_Sm=function(_Sn){return function(_8i){return new F(function(){return _gy(new T(function(){return B(_Li(_LD,_Sn,_hT));}),_8i);});};},_So=[0,_Sm,_Sl,_Sg,_Sh],_Sp=function(_Sq,_Sr){return new F(function(){return _3x(0,E(_Sq)[1],_Sr);});},_Ss=function(_St,_Su){return new F(function(){return _2g(_Sp,_St,_Su);});},_Sv=function(_Sw,_Sx,_Sy){return new F(function(){return _3x(E(_Sw)[1],E(_Sx)[1],_Sy);});},_Sz=[0,_Sv,_P1,_Ss],_SA=new T(function(){return B(unCStr("Int"));}),_SB=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_bk,_bl,_SA],_SC=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_SB,_X],_SD=function(_SE){return E(_SC);},_SF=new T(function(){return B(_Bv(_yz,_yA,_SD,_Sz,_So));}),_SG=function(_SH,_SI,_SJ){return function(_SK,_){var _SL=B(_Rw(function(_SM,_){var _SN=B(A(new T(function(){return B(A(_SF,[_2A,_P8,[1,_SH]]));}),[_SM,_])),_SO=_SN,_SP=E(_SO),_SQ=E(_SP[1]);return [0,[0,function(_SR,_){var _SS=B(A(_SQ[1],[_SR,_])),_ST=_SS,_SU=B(A(_8,[_e,_ST,_QI,_QU,_])),_SV=_SU,_SW=B(A(_8,[_e,_ST,_P6,_QV,_])),_SX=_SW;return _ST;},_SQ[2]],_SP[2]];},_QY,_SK,_)),_SY=_SL,_SZ=E(_SY),_T0=E(_SZ[1]),_T1=B(_Rw(function(_T2,_){var _T3=B(A(new T(function(){return B(A(_SF,[_2A,_P8,[1,_SI]]));}),[_T2,_])),_T4=_T3,_T5=E(_T4),_T6=E(_T5[1]);return [0,[0,function(_T7,_){var _T8=B(A(_T6[1],[_T7,_])),_T9=_T8,_Ta=B(A(_8,[_e,_T9,_QI,_R2,_])),_Tb=_Ta,_Tc=B(A(_8,[_e,_T9,_P6,_R3,_])),_Td=_Tc;return _T9;},_T6[2]],_T5[2]];},_QM,_SZ[2],_)),_Te=_T1,_Tf=E(_Te),_Tg=E(_Tf[1]),_Th=B(A(new T(function(){return B(A(_SF,[_2A,_P8,[1,_SJ]]));}),[_Tf[2],_])),_Ti=_Th,_Tj=E(_Ti),_Tk=E(_Tj[1]),_Tl=B(A(_Sf,[_Tj[2],_])),_Tm=_Tl,_Tn=E(_Tm);return [0,[0,function(_To,_){var _Tp=B(A(_T0[1],[_To,_])),_Tq=_Tp,_Tr=B(A(_Tg[1],[_To,_])),_Ts=_Tr,_Tt=B(A(_Tk[1],[_To,_])),_Tu=_Tt,_Tv=B(A(_8,[_e,_Tu,_QI,_QR,_])),_Tw=_Tv,_Tx=B(A(_8,[_e,_Tu,_P6,_QS,_])),_Ty=_Tx,_Tz=B(A(E(_Tn[1])[1],[_To,_])),_TA=_Tz,_TB=B(_Dx(_To,_)),_TC=_TB;return _To;},new T(function(){var _TD=E(_T0[2]);if(!_TD[0]){var _TE=[0];}else{var _TF=E(_Tg[2]);if(!_TF[0]){var _TG=[0];}else{var _TH=E(_Tk[2]),_TG=_TH[0]==0?[0]:[1,[0,_TD[1],_TF[1],_TH[1]]];}var _TE=_TG;}return _TE;})],_Tn[2]];};},_TI=function(_TJ){var _TK=jsRound(_TJ),_TL=_TK;return [0,_TL];},_TM=new T(function(){return [0,"(function(){return new Date().getMonth()+1})"];}),_TN=function(_TO,_){var _TP=B(A(_50,[_TM,_])),_TQ=_TP;return [0,[0,_2Q,[1,new T(function(){return B(_TI(_TQ));})]],_TO];},_TR=new T(function(){return [0,"(function(){return new Date().getDate()})"];}),_TS=function(_TT,_){var _TU=B(A(_50,[_TR,_])),_TV=_TU;return [0,[0,_2Q,[1,new T(function(){return B(_TI(_TV));})]],_TT];},_TW=new T(function(){return [0,"(function(){return new Date().getFullYear()})"];}),_TX=function(_TY,_){var _TZ=B(A(_50,[_TW,_])),_U0=_TZ;return [0,[0,_2Q,[1,new T(function(){return B(_TI(_U0));})]],_TY];},_U1=new T(function(){return B(unCStr("autofocus"));}),_U2=new T(function(){return B(unCStr("true"));}),_U3=new T(function(){return B(_Bv(_yz,_yA,_bp,_wi,_vL));}),_U4=new T(function(){return B(A(_U3,[_2A,_P8,_2A]));}),_U5=function(_U6,_){var _U7=B(A(_U4,[_U6,_])),_U8=_U7,_U9=E(_U8),_Ua=E(_U9[1]);return [0,[0,function(_Ub,_){var _Uc=B(A(_Ua[1],[_Ub,_])),_Ud=_Uc,_Ue=B(A(_8,[_e,_Ud,_U1,_U2,_])),_Uf=_Ue;return _Ud;},_Ua[2]],_U9[2]];},_Ug=new T(function(){return B(_8m(_U5,_DC));}),_Uh=new T(function(){return B(unCStr("Enter amount: "));}),_Ui=new T(function(){return B(_EE(_0,_Uh));}),_Uj=function(_Uk,_){var _Ul=B(A(_Ug,[_Uk,_])),_Um=_Ul,_Un=E(_Um),_Uo=E(_Un[1]);return [0,[0,function(_Up,_){var _Uq=B(A(_Ui,[_Up,_])),_Ur=_Uq,_Us=B(A(_8,[_e,_Ur,_fE,_fF,_])),_Ut=_Us,_Uu=B(A(_Uo[1],[_Up,_])),_Uv=_Uu,_Uw=B(_Dx(_Up,_)),_Ux=_Uw;return _Up;},_Uo[2]],_Un[2]];},_Uy=new T(function(){return B(unCStr("Click here to confirm"));}),_Uz=new T(function(){return B(_4G(_0,_Uy));}),_UA=new T(function(){return B(_9r(_Uz,_5o));}),_UB=new T(function(){return B(unCStr("Registered! "));}),_UC=new T(function(){return B(_EE(_0,_UB));}),_UD=function(_UE,_){var _UF=B(A(_UC,[_UE,_])),_UG=_UF,_UH=B(A(_8,[_e,_UG,_fE,_fF,_])),_UI=_UH;return _UG;},_UJ=[0,_UD,_4E],_UK=function(_UL,_){return [0,_UJ,_UL];},_UM=function(_UN){return E(E(_UN)[1]);},_UO=function(_UP,_UQ){var _UR=new T(function(){return B(_yG(_UQ));}),_US=function(_UT){var _UU=E(_UT);if(!_UU[0]){return [0,new T(function(){return B(_UM(_UR));}),_2A];}else{var _UV=E(_UU[1]),_UW=B(_US(_UU[2]));return [0,new T(function(){return B(A(new T(function(){return B(_Bn(_UR));}),[_UV[1],_UW[1]]));}),new T(function(){var _UX=E(_UV[2]);return _UX[0]==0?E(_UW[2]):E(_UX);})];}},_UY=new T(function(){return B(_3i(_UP));});return function(_UZ,_V0){return new F(function(){return A(new T(function(){return B(_2T(_UP));}),[new T(function(){return B(A(new T(function(){return B(_3D([0,_],B(_3k(_UP)),function(_V1){return new F(function(){return _Bk(_UP,_V1);});},function(_V2,_V3){return new F(function(){return _Bp(_UP,_V2,_V3);});}));}),[_V0]));}),function(_V4){return new F(function(){return A(new T(function(){return B(_2T(_UP));}),[new T(function(){var _V5=function(_V6){var _V7=E(_V6);return _V7[0]==0?function(_V8){return new F(function(){return A(_UY,[[0,_X,_V8]]);});}:function(_V9){return new F(function(){return A(new T(function(){return B(_2T(_UP));}),[new T(function(){return B(A(new T(function(){return B(A(_V7[1],[new T(function(){return E(E(_V4)[1]);})]));}),[_V9]));}),function(_Va){return new F(function(){return A(new T(function(){return B(_2T(_UP));}),[new T(function(){return B(A(new T(function(){return B(_V5(_V7[2]));}),[new T(function(){return E(E(_Va)[2]);})]));}),function(_Vb){return new F(function(){return A(_UY,[[0,[1,new T(function(){return E(E(_Va)[1]);}),new T(function(){return E(E(_Vb)[1]);})],new T(function(){return E(E(_Vb)[2]);})]]);});}]);});}]);});};};return B(A(_V5,[_UZ,new T(function(){return E(E(_V4)[2]);})]));}),function(_Vc){var _Vd=new T(function(){var _Ve=B(_US(E(_Vc)[1]));return [0,_Ve[1],_Ve[2]];});return new F(function(){return A(new T(function(){return B(_3i(_UP));}),[[0,[0,new T(function(){return E(E(_Vd)[1]);}),new T(function(){var _Vf=E(E(_Vd)[2]);return _Vf[0]==0?[0]:[1,_Vf[1]];})],new T(function(){return E(E(_Vc)[2]);})]]);});}]);});}]);});};},_Vg=new T(function(){return B(_UO(_2S,_yz));}),_Vh=function(_Vi){switch(E(_Vi)){case 0:return E(_vM);case 1:return E(_D2);case 2:return E(_D7);case 3:return E(_Dc);case 4:return E(_Dh);default:return E(_DD);}},_Vj=function(_Vk){return new F(function(){return _1q(_DD,_Vk);});},_Vl=function(_Vk){return new F(function(){return _1q(_Dh,_Vk);});},_Vm=function(_Vk){return new F(function(){return _1q(_Dc,_Vk);});},_Vn=function(_Vk){return new F(function(){return _1q(_D7,_Vk);});},_Vo=function(_Vk){return new F(function(){return _1q(_D2,_Vk);});},_Vp=function(_Vk){return new F(function(){return _1q(_vM,_Vk);});},_Vq=function(_Vr){switch(E(_Vr)){case 0:return E(_Vp);case 1:return E(_Vo);case 2:return E(_Vn);case 3:return E(_Vm);case 4:return E(_Vl);default:return E(_Vj);}},_Vs=function(_Dv,_Vk){return new F(function(){return _2g(_Vq,_Dv,_Vk);});},_Vt=function(_Vu,_Vv){switch(E(_Vv)){case 0:return E(_Vp);case 1:return E(_Vo);case 2:return E(_Vn);case 3:return E(_Vm);case 4:return E(_Vl);default:return E(_Vj);}},_Vw=[0,_Vt,_Vh,_Vs],_Vx=new T(function(){return B(unCStr("main"));}),_Vy=new T(function(){return B(unCStr("Main"));}),_Vz=new T(function(){return B(unCStr("EntryType"));}),_VA=[0,I_fromBits([1591731410,1888343015]),I_fromBits([1572778742,45233797]),_Vx,_Vy,_Vz],_VB=[0,I_fromBits([1591731410,1888343015]),I_fromBits([1572778742,45233797]),_VA,_X],_VC=function(_VD){return E(_VB);},_VE=function(_VF){return E(E(_VF)[15]);},_VG=function(_VH,_VI,_){return new F(function(){return _yB(E(_VH)[1],_VI,_);});},_VJ=new T(function(){return B(A(_zb,[_b6]));}),_VK=new T(function(){return B(unCStr("name"));}),_VL=new T(function(){return B(unCStr("true"));}),_VM=new T(function(){return B(unCStr("radio"));}),_VN=function(_VO,_VP,_VQ,_VR){var _VS=new T(function(){return B(_yI(_VP));}),_VT=new T(function(){return B(_2T(_VS));}),_VU=new T(function(){return B(_Bt(_VR));});return function(_VV,_VW){return function(_VX){return new F(function(){return A(_VT,[new T(function(){return B(A(new T(function(){return B(_3D([0,_],B(_3k(_VS)),function(_VY){return new F(function(){return _Bk(_VS,_VY);});},function(_VZ,_W0){return new F(function(){return _Bp(_VS,_VZ,_W0);});}));}),[_VX]));}),function(_W1){var _W2=new T(function(){return E(E(_W1)[1]);});return new F(function(){return A(new T(function(){return B(_2T(_VS));}),[new T(function(){var _W3=new T(function(){return E(E(_W1)[2]);});return B(A(new T(function(){return B(_3i(_VS));}),[[0,_W3,_W3]]));}),function(_W4){return new F(function(){return A(new T(function(){return B(_2T(_VS));}),[new T(function(){return B(A(new T(function(){return B(_3i(_VS));}),[[0,_7,new T(function(){var _W5=E(E(_W4)[1]);return [0,_W5[1],_W5[2],_Bh,_W5[4],_W5[5],_W5[6]];})]]));}),function(_W6){return new F(function(){return A(new T(function(){return B(_2T(_VS));}),[new T(function(){return B(A(new T(function(){return B(_yM(_VP,function(_){return new F(function(){return jsFind(toJSStr(E(_W2)));});}));}),[new T(function(){return E(E(_W6)[2]);})]));}),function(_W7){return new F(function(){return A(_VT,[new T(function(){var _W8=E(_W7),_W9=_W8[2],_Wa=E(_W8[1]);if(!_Wa[0]){var _Wb=B(A(new T(function(){return B(_3i(_VS));}),[[0,_X,_W9]]));}else{var _Wb=B(A(_yM,[_VP,function(_){return new F(function(){return _VG(_Wa[1],_wF,_);});},_W9]));}var _Wc=_Wb;return _Wc;}),function(_Wd){var _We=new T(function(){return !B(_5O(E(_Wd)[1],_VL))?[0]:E([1,_VV]);});return new F(function(){return A(new T(function(){return B(_3i(_VS));}),[[0,[0,new T(function(){return B(A(new T(function(){return B(_VE(_VO));}),[new T(function(){return B(A(new T(function(){return B(_Bi(_VO));}),[_W2,_VM,new T(function(){var _Wf=B(A(_VQ,[_VV])),_Wg=E(_VJ),_Wh=hs_eqWord64(_Wf[1],_Wg[1]),_Wi=_Wh;if(!E(_Wi)){var _Wj=B(A(_VU,[_VV]));}else{var _Wk=hs_eqWord64(_Wf[2],_Wg[2]),_Wl=_Wk,_Wj=E(_Wl)==0?B(A(_VU,[_VV])):E(_VV);}var _Wm=_Wj,_Wn=_Wm;return _Wn;}),new T(function(){return E(_We)[0]==0?false:true;}),_2A]));}),[1,[0,_VK,_VW],_X]]));}),new T(function(){var _Wo=E(_We);return _Wo[0]==0?[0]:[1,_Wo[1]];})],new T(function(){return E(E(_Wd)[2]);})]]);});}]);});}]);});}]);});}]);});}]);});};};},_Wp=new T(function(){return B(_VN(_yz,_yA,_VC,_Vw));}),_Wq=new T(function(){return B(unCStr("Travel "));}),_Wr=new T(function(){return B(_EE(_0,_Wq));}),_Ws=function(_Wt){return function(_Wu,_){var _Wv=B(A(new T(function(){return B(_8m(new T(function(){return B(A(_Wp,[_fD,_Wt]));}),_5o));}),[_Wu,_])),_Ww=_Wv,_Wx=E(_Ww),_Wy=E(_Wx[1]);return [0,[0,function(_Wz,_){var _WA=B(A(_Wr,[_Wz,_])),_WB=_WA,_WC=B(A(_8,[_e,_WB,_fE,_fF,_])),_WD=_WC,_WE=B(A(_Wy[1],[_Wz,_])),_WF=_WE,_WG=B(_Dx(_Wz,_)),_WH=_WG;return _Wz;},_Wy[2]],_Wx[2]];};},_WI=new T(function(){return B(unCStr("Food "));}),_WJ=new T(function(){return B(_EE(_0,_WI));}),_WK=function(_WL){return function(_WM,_){var _WN=B(A(new T(function(){return B(_8m(new T(function(){return B(A(_Wp,[_fA,_WL]));}),_5o));}),[_WM,_])),_WO=_WN,_WP=E(_WO),_WQ=E(_WP[1]);return [0,[0,function(_WR,_){var _WS=B(A(_WJ,[_WR,_])),_WT=_WS,_WU=B(A(_8,[_e,_WT,_fE,_fF,_])),_WV=_WU,_WW=B(A(_WQ[1],[_WR,_])),_WX=_WW,_WY=B(_Dx(_WR,_)),_WZ=_WY;return _WR;},_WQ[2]],_WP[2]];};},_X0=new T(function(){return B(_EE(_0,_Gb));}),_X1=function(_X2){return function(_X3,_){var _X4=B(A(new T(function(){return B(_8m(new T(function(){return B(A(_Wp,[_fz,_X2]));}),_5o));}),[_X3,_])),_X5=_X4,_X6=E(_X5),_X7=E(_X6[1]);return [0,[0,function(_X8,_){var _X9=B(A(_X0,[_X8,_])),_Xa=_X9,_Xb=B(A(_8,[_e,_Xa,_fE,_fF,_])),_Xc=_Xb,_Xd=B(A(_X7[1],[_X8,_])),_Xe=_Xd,_Xf=B(_Dx(_X8,_)),_Xg=_Xf;return _X8;},_X7[2]],_X6[2]];};},_Xh=new T(function(){return B(unCStr("Other "));}),_Xi=new T(function(){return B(_EE(_0,_Xh));}),_Xj=function(_Xk){return function(_Xl,_){var _Xm=B(A(new T(function(){return B(_8m(new T(function(){return B(A(_Wp,[_fC,_Xk]));}),_5o));}),[_Xl,_])),_Xn=_Xm,_Xo=E(_Xn),_Xp=E(_Xo[1]);return [0,[0,function(_Xq,_){var _Xr=B(A(_Xi,[_Xq,_])),_Xs=_Xr,_Xt=B(A(_8,[_e,_Xs,_fE,_fF,_])),_Xu=_Xt,_Xv=B(A(_Xp[1],[_Xq,_])),_Xw=_Xv,_Xx=B(_Dx(_Xq,_)),_Xy=_Xx;return _Xq;},_Xp[2]],_Xo[2]];};},_Xz=new T(function(){return B(unCStr("Income "));}),_XA=new T(function(){return B(_EE(_0,_Xz));}),_XB=function(_XC){return function(_XD,_){var _XE=B(A(new T(function(){return B(_8m(new T(function(){return B(A(_Wp,[_fB,_XC]));}),_5o));}),[_XD,_])),_XF=_XE,_XG=E(_XF),_XH=E(_XG[1]);return [0,[0,function(_XI,_){var _XJ=B(_9X(_XI,_)),_XK=_XJ,_XL=B(A(_XA,[_XI,_])),_XM=_XL,_XN=B(A(_8,[_e,_XM,_fE,_fF,_])),_XO=_XN,_XP=B(A(_XH[1],[_XI,_])),_XQ=_XP,_XR=B(_Dx(_XI,_)),_XS=_XR;return _XI;},_XH[2]],_XG[2]];};},_XT=[1,_XB,_X],_XU=[1,_Xj,_XT],_XV=[1,_X1,_XU],_XW=[1,_WK,_XV],_XX=[1,_Ws,_XW],_XY=new T(function(){return B(A(_Vg,[_XX]));}),_XZ=new T(function(){return B(unCStr("regnumber"));}),_Y0=new T(function(){return [0,toJSStr(_X)];}),_Y1=[0,93],_Y2=[1,_Y1,_X],_Y3=new T(function(){return [0,toJSStr(_Y2)];}),_Y4=[0,125],_Y5=[1,_Y4,_X],_Y6=new T(function(){return [0,toJSStr(_Y5)];}),_Y7=[0,58],_Y8=[1,_Y7,_X],_Y9=new T(function(){return [0,toJSStr(_Y8)];}),_Ya=[0,44],_Yb=[1,_Ya,_X],_Yc=new T(function(){return [0,toJSStr(_Yb)];}),_Yd=new T(function(){return [0,"false"];}),_Ye=function(_Yf){var _Yg=jsShow(E(_Yf)[1]),_Yh=_Yg;return [0,_Yh];},_Yi=function(_Yj){var _Yk=jsStringify(E(_Yj)[1]),_Yl=_Yk;return [0,_Yl];},_Ym=new T(function(){return [0,"null"];}),_Yn=[0,91],_Yo=[1,_Yn,_X],_Yp=new T(function(){return [0,toJSStr(_Yo)];}),_Yq=[0,123],_Yr=[1,_Yq,_X],_Ys=new T(function(){return [0,toJSStr(_Yr)];}),_Yt=[0,34],_Yu=[1,_Yt,_X],_Yv=new T(function(){return [0,toJSStr(_Yu)];}),_Yw=new T(function(){return [0,"true"];}),_Yx=function(_Yy,_Yz){var _YA=E(_Yz);switch(_YA[0]){case 0:return [1,new T(function(){return B(_Ye(_YA[1]));}),_Yy];case 1:return [1,new T(function(){return B(_Yi(_YA[1]));}),_Yy];case 2:return !E(_YA[1])?[1,_Yd,_Yy]:[1,_Yw,_Yy];case 3:var _YB=E(_YA[1]);return _YB[0]==0?[1,_Yp,[1,_Y3,_Yy]]:[1,_Yp,new T(function(){return B(_Yx(new T(function(){var _YC=function(_YD){var _YE=E(_YD);return _YE[0]==0?E([1,_Y3,_Yy]):[1,_Yc,new T(function(){return B(_Yx(new T(function(){return B(_YC(_YE[2]));}),_YE[1]));})];};return B(_YC(_YB[2]));}),_YB[1]));})];case 4:var _YF=E(_YA[1]);if(!_YF[0]){return [1,_Ys,[1,_Y6,_Yy]];}else{var _YG=E(_YF[1]);return [1,_Ys,[1,new T(function(){return B(_Yi(_YG[1]));}),[1,_Y9,new T(function(){return B(_Yx(new T(function(){var _YH=function(_YI){var _YJ=E(_YI);if(!_YJ[0]){return E([1,_Y6,_Yy]);}else{var _YK=E(_YJ[1]);return [1,_Yc,[1,_Yv,[1,_YK[1],[1,_Yv,[1,_Y9,new T(function(){return B(_Yx(new T(function(){return B(_YH(_YJ[2]));}),_YK[2]));})]]]]];}};return B(_YH(_YF[2]));}),_YG[2]));})]]];}break;default:return [1,_Ym,_Yy];}},_YL=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_YM=function(_YN){return E(E(_YN)[1]);},_YO=function(_YP,_YQ){return function(_YR,_){var _YS=jsCat(new T(function(){return B(_Yx(_X,B(A(new T(function(){return B(_YM(_YP));}),[_YR]))));}),E(_Y0)[1]),_YT=_YS,_YU=B(A(new T(function(){return B(A(_50,[_YL,E(toJSStr(E(_YQ)))]));}),[E(_YT),_])),_YV=_YU;return _7;};},_YW=new T(function(){return B(_YO(_Ou,_OT));}),_YX=new T(function(){return B(unCStr("Enter Date:"));}),_YY=new T(function(){return B(_EE(_0,_YX));}),_YZ=new T(function(){return B(unCStr(" registers"));}),_Z0=new T(function(){return B(_EE(_0,_YZ));}),_Z1=function(_Z2,_Z3,_){return new F(function(){return _3Q(_Uj,function(_Z4){return function(_8i,_8j){return new F(function(){return _3Q(_TS,function(_Z5){return function(_8i,_8j){return new F(function(){return _3Q(_TN,function(_Z6){return function(_8i,_8j){return new F(function(){return _3Q(_TX,function(_Z7){return function(_8i,_8j){return new F(function(){return _3Q(function(_Z8,_){var _Z9=B(A(new T(function(){return B(_SG(_Z5,_Z6,_Z7));}),[_Z8,_])),_Za=_Z9,_Zb=E(_Za),_Zc=E(_Zb[1]);return [0,[0,function(_Zd,_){var _Ze=B(A(_YY,[_Zd,_])),_Zf=_Ze,_Zg=B(A(_8,[_e,_Zf,_fE,_fF,_])),_Zh=_Zg,_Zi=B(A(_Zc[1],[_Zd,_])),_Zj=_Zi;return _Zd;},_Zc[2]],_Zb[2]];},function(_Zk,_Zl,_){var _Zm=E(_Zk);return new F(function(){return (function(_Zn,_Zo,_Zp,_Zq,_){return new F(function(){return _3Q(_XY,function(_Zr){return function(_8i,_8j){return new F(function(){return _3Q(_UA,function(_Zs,_Dv,_){return new F(function(){return (function(_Dv,_){return new F(function(){return _3Q(_OW,function(_Zt){return function(_8i,_8j){return new F(function(){return _3Q(new T(function(){var _Zu=B(A(_YW,[[1,[0,_Zn,_Zo,_Zp,_Z2,_Z4,_Zr],_Zt]]));return function(_Zv,_){var _Zw=B(A(_Zu,[_])),_Zx=_Zw;return [0,[0,_2Q,[1,_Zx]],_Zv];};}),function(_Zy,_Dv,_){return new F(function(){return (function(_Dv,_){return new F(function(){return _3Q(_UK,function(_Zz){return function(_Dv,_){return new F(function(){return _7G(_XZ,_7C,function(_ZA,_){return [0,[0,function(_ZB,_){var _ZC=B(A(new T(function(){return B(_FW(_P3,new T(function(){return [0,B(_sd(_Zt,0))+1|0];})));}),[_ZB,_])),_ZD=_ZC,_ZE=B(A(_Z0,[_ZB,_])),_ZF=_ZE,_ZG=B(A(_8,[_e,_ZF,_fE,_fF,_])),_ZH=_ZG;return _ZB;},_4E],_ZA];},_Dv,_);});};},_Dv,_);});})(_Dv,_);});},_8i,_8j);});};},_Dv,_);});})(_Dv,_);});},_8i,_8j);});};},_Zq,_);});})(_Zm[1],_Zm[2],_Zm[3],_Zl,_);});},_8i,_8j);});};},_8i,_8j);});};},_8i,_8j);});};},_8i,_8j);});};},_Z3,_);});},_ZI=function(_Dv,_){return new F(function(){return _3Q(_Qr,_Z1,_Dv,_);});},_ZJ=new T(function(){return B(unCStr("new Entry"));}),_ZK=new T(function(){return B(_9D(_6H,_ZJ));}),_ZL=new T(function(){return B(unCStr("Remove Last entry"));}),_ZM=new T(function(){return B(_9D(_54,_ZL));}),_ZN=function(_ZO,_){var _ZP=B(A(_ZK,[_ZO,_])),_ZQ=_ZP,_ZR=E(_ZQ),_ZS=E(_ZR[1]),_ZT=B(A(_ZM,[_ZR[2],_])),_ZU=_ZT,_ZV=E(_ZU),_ZW=E(_ZV[1]);return [0,[0,function(_ZX,_){var _ZY=B(A(_ZS[1],[_ZX,_])),_ZZ=_ZY,_100=B(A(_ZW[1],[_ZX,_])),_101=_100;return _ZX;},new T(function(){var _102=E(_ZS[2]);return _102[0]==0?E(_ZW[2]):E(_102);})],_ZV[2]];},_103=[0,_2Q,_Gu],_104=function(_105,_){return [0,_103,_105];},_106=new T(function(){return B(unCStr(" Registers created:"));}),_107=new T(function(){return B(_EE(_0,_106));}),_108=new T(function(){return B(_EE(_0,_YZ));}),_109=function(_10a){var _10b=new T(function(){return [0,B(_sd(_10a,0))];});return function(_8i,_8j){return new F(function(){return _3Q(function(_10c,_){return [0,[0,function(_10d,_){var _10e=B(A(new T(function(){return B(_Fk(_Fu,function(_10f,_){var _10g=B(A(new T(function(){return B(_FW(_P3,_10b));}),[_10f,_])),_10h=_10g,_10i=B(A(_107,[_10f,_])),_10j=_10i,_10k=B(A(_8,[_e,_10j,_fE,_fF,_])),_10l=_10k;return _10f;}));}),[_10d,_])),_10m=_10e,_10n=B(A(_8,[_e,_10m,_Ey,_XZ,_])),_10o=_10n;return _10m;},_4E],_10c];},function(_10p,_Dv,_){return new F(function(){return (function(_Dv,_){return new F(function(){return _3Q(_ZN,function(_10q){return !E(_10q)?function(_Dv,_){return new F(function(){return _3Q(_OW,function(_10r){var _10s=E(_10r);return _10s[0]==0?E(_104):function(_8i,_8j){return new F(function(){return _3Q(new T(function(){var _10t=B(A(_YW,[_10s[2]]));return function(_10u,_){var _10v=B(A(_10t,[_])),_10w=_10v;return [0,[0,_2Q,[1,_10w]],_10u];};}),function(_10x){return function(_Dv,_){return new F(function(){return _7G(_XZ,_7C,function(_10y,_){return [0,[0,function(_10z,_){var _10A=B(A(new T(function(){return B(_FW(_P3,new T(function(){return [0,E(_10b)[1]-1|0];})));}),[_10z,_])),_10B=_10A,_10C=B(A(_108,[_10z,_])),_10D=_10C,_10E=B(A(_8,[_e,_10D,_fE,_fF,_])),_10F=_10E;return _10z;},_4E],_10y];},_Dv,_);});};},_8i,_8j);});};},_Dv,_);});}:E(_ZI);},_Dv,_);});})(_Dv,_);});},_8i,_8j);});};},_10G=function(_Dv,_){return new F(function(){return _3Q(_OW,_109,_Dv,_);});},_10H=new T(function(){return B(unCStr("from:"));}),_10I=new T(function(){return B(_EE(_0,_10H));}),_10J=function(_10K,_){var _10L=B(_Dx(_10K,_)),_10M=_10L,_10N=B(A(_10I,[_10K,_])),_10O=_10N,_10P=B(A(_8,[_e,_10O,_fE,_fF,_])),_10Q=_10P;return _10K;},_10R=[0,_10J,_4E],_10S=function(_10T,_){return [0,_10R,_10T];},_10U=function(_10V,_){var _10W=B(A(_50,[_TR,_])),_10X=_10W,_10Y=B(A(_50,[_TM,_])),_10Z=_10Y,_110=B(A(_50,[_TW,_])),_111=_110;return [0,[0,function(_112,_){return _112;},[1,[0,new T(function(){return B(_TI(_10X));}),new T(function(){return B(_TI(_10Z));}),new T(function(){return B(_TI(_111));})]]],_10V];},_113=function(_114){var _115=E(_114),_116=_115[1];return _116<0?[0, -_116]:E(_115);},_117=function(_118){var _119=E(_118);return _119[0]==0?_119[1]:I_toNumber(_119[1]);},_11a=function(_11b){return [0,B(_117(_11b))];},_11c=[0,0],_11d=[0,1],_11e=[0,-1],_11f=function(_11g){var _11h=E(_11g)[1];return _11h!=0?_11h<=0?E(_11e):E(_11d):E(_11c);},_11i=function(_11j,_11k){return [0,E(_11j)[1]-E(_11k)[1]];},_11l=function(_11m){return [0, -E(_11m)[1]];},_11n=function(_11o,_11p){return [0,E(_11o)[1]+E(_11p)[1]];},_11q=function(_11r,_11s){return [0,E(_11r)[1]*E(_11s)[1]];},_11t=[0,_11n,_11q,_11i,_11l,_113,_11f,_11a],_11u=function(_11v){return function(_8i,_8j){return new F(function(){return _0(new T(function(){return B(_w7(_11v));}),_8i,_8j);});};},_11w=new T(function(){return B(unCStr("to:"));}),_11x=new T(function(){return B(_EE(_0,_11w));}),_11y=function(_11z,_){var _11A=B(_Dx(_11z,_)),_11B=_11A,_11C=B(A(_11x,[_11z,_])),_11D=_11C,_11E=B(A(_8,[_e,_11D,_fE,_fF,_])),_11F=_11E;return _11z;},_11G=[0,_11y,_4E],_11H=function(_11I,_){return [0,_11G,_11I];},_11J=new T(function(){return B(unCStr(" Income: "));}),_11K=function(_Dv,_){return new F(function(){return _0(_11J,_Dv,_);});},_11L=new T(function(){return B(A(_zb,[_b6]));}),_11M=function(_11N,_11O,_11P,_11Q){return function(_8i,_8j){return new F(function(){return _6I(function(_w,_){return new F(function(){return _3Q(new T(function(){return B(_9r(function(_11R,_){var _11S=B(A(new T(function(){return B(_ER(_Fu,_11Q));}),[_11R,_])),_11T=_11S,_11U=B(A(_8,[_e,_11T,_EB,new T(function(){return B(unAppCStr("#/",new T(function(){var _11V=B(A(_11O,[_11P])),_11W=E(_11L),_11X=hs_eqWord64(_11V[1],_11W[1]),_11Y=_11X;if(!E(_11Y)){var _11Z=B(A(_Bt,[_11N,_11P]));}else{var _120=hs_eqWord64(_11V[2],_11W[2]),_121=_120,_11Z=E(_121)==0?B(A(_Bt,[_11N,_11P])):E(_11P);}var _122=_11Z,_123=_122;return _123;})));}),_])),_124=_11U;return _11T;},_5o));}),function(_125,_w,_){return new F(function(){return (function(_126,_){return [0,[0,_2Q,[1,_11P]],_126];})(_w,_);});},_w,_);});},_8i,_8j);});};},_127=new T(function(){return B(_11M(_Vw,_VC,_fB,_11K));}),_128=new T(function(){return B(unCStr(" Travel: "));}),_129=function(_Dv,_){return new F(function(){return _0(_128,_Dv,_);});},_12a=new T(function(){return B(_11M(_Vw,_VC,_fD,_129));}),_12b=new T(function(){return B(unCStr(" Food: "));}),_12c=function(_Dv,_){return new F(function(){return _0(_12b,_Dv,_);});},_12d=new T(function(){return B(_11M(_Vw,_VC,_fA,_12c));}),_12e=new T(function(){return B(unCStr(" Entertain"));}),_12f=function(_Dv,_){return new F(function(){return _0(_12e,_Dv,_);});},_12g=new T(function(){return B(_11M(_Vw,_VC,_fz,_12f));}),_12h=new T(function(){return B(unCStr("Other: "));}),_12i=function(_Dv,_){return new F(function(){return _0(_12h,_Dv,_);});},_12j=new T(function(){return B(_11M(_Vw,_VC,_fC,_12i));}),_12k=function(_12l){return function(_8i,_8j){return new F(function(){return _0(new T(function(){return B(_Vh(_12l));}),_8i,_8j);});};},_12m=[0,45],_12n=function(_12o,_12p){return [1,new T(function(){var _12q=E(_12o);return B(_Fk(_Fu,function(_12r,_){var _12s=B(A(new T(function(){return B(_EE(_0,new T(function(){return B(_1q(B(_3x(0,E(_12q[1])[1],_X)),[1,_12m,new T(function(){return B(_1q(B(_3x(0,E(_12q[2])[1],_X)),[1,_12m,new T(function(){return B(_3x(0,E(_12q[3])[1],_X));})]));})]));})));}),[_12r,_])),_12t=_12s,_12u=B(A(_8,[_e,_12t,_fE,_fF,_])),_12v=_12u,_12w=B(A(new T(function(){return B(_EE(_12k,_12q[6]));}),[_12r,_])),_12x=_12w,_12y=B(A(_8,[_e,_12x,_fE,_fF,_])),_12z=_12y,_12A=B(A(new T(function(){return B(_EE(_0,_12q[4]));}),[_12r,_])),_12B=_12A,_12C=B(A(_8,[_e,_12B,_fE,_fF,_])),_12D=_12C,_12E=B(A(new T(function(){return B(_EE(_11u,_12q[5]));}),[_12r,_])),_12F=_12E,_12G=B(A(_8,[_e,_12F,_fE,_fF,_])),_12H=_12G,_12I=B(_Dx(_12r,_)),_12J=_12I;return _12r;}));}),_12p];},_12K=function(_12L,_12M){while(1){var _12N=(function(_12O,_12P){var _12Q=E(_12P);if(!_12Q[0]){return [0];}else{var _12R=_12Q[1],_12S=_12Q[2];if(!B(A(_12O,[_12R]))){var _12T=_12O;_12M=_12S;_12L=_12T;return null;}else{return [1,_12R,new T(function(){return B(_12K(_12O,_12S));})];}}})(_12L,_12M);if(_12N!=null){return _12N;}}},_12U=function(_12V){while(1){var _12W=(function(_12X){var _12Y=E(_12X);if(!_12Y[0]){return [0];}else{var _12Z=_12Y[2],_130=E(_12Y[1]);if(E(_130[6])==4){return [1,_130[5],new T(function(){return B(_12U(_12Z));})];}else{_12V=_12Z;return null;}}})(_12V);if(_12W!=null){return _12W;}}},_131=function(_132){while(1){var _133=(function(_134){var _135=E(_134);if(!_135[0]){return [0];}else{var _136=_135[2],_137=E(_135[1]);if(E(_137[6])==3){return [1,_137[5],new T(function(){return B(_131(_136));})];}else{_132=_136;return null;}}})(_132);if(_133!=null){return _133;}}},_138=function(_139){while(1){var _13a=(function(_13b){var _13c=E(_13b);if(!_13c[0]){return [0];}else{var _13d=_13c[2],_13e=E(_13c[1]);if(E(_13e[6])==2){return [1,_13e[5],new T(function(){return B(_138(_13d));})];}else{_139=_13d;return null;}}})(_139);if(_13a!=null){return _13a;}}},_13f=function(_13g){while(1){var _13h=(function(_13i){var _13j=E(_13i);if(!_13j[0]){return [0];}else{var _13k=_13j[2],_13l=E(_13j[1]);if(E(_13l[6])==1){return [1,_13l[5],new T(function(){return B(_13f(_13k));})];}else{_13g=_13k;return null;}}})(_13g);if(_13h!=null){return _13h;}}},_13m=function(_13n){while(1){var _13o=(function(_13p){var _13q=E(_13p);if(!_13q[0]){return [0];}else{var _13r=_13q[2],_13s=E(_13q[1]);switch(E(_13s[6])){case 0:return [1,_13s[5],new T(function(){return B(_13m(_13r));})];case 1:_13n=_13r;return null;case 2:_13n=_13r;return null;case 3:_13n=_13r;return null;case 4:_13n=_13r;return null;default:_13n=_13r;return null;}}})(_13n);if(_13o!=null){return _13o;}}},_13t=function(_13u){var _13v=E(_13u);if(!_13v[0]){return [0];}else{return new F(function(){return _12n(_13v[1],new T(function(){return B(_13t(_13v[2]));}));});}},_13w=[0,_Gc,_Gd],_13x=new T(function(){return B(unCStr("Al registers selected:"));}),_13y=new T(function(){return B(_DL(_0,_13x));}),_13z=new T(function(){return B(unCStr("Date"));}),_13A=new T(function(){return B(_FW(_0,_13z));}),_13B=new T(function(){return B(_EE(_Fu,_13A));}),_13C=new T(function(){return B(unCStr("Description"));}),_13D=new T(function(){return B(_FW(_0,_13C));}),_13E=new T(function(){return B(_EE(_Fu,_13D));}),_13F=new T(function(){return B(_FW(_0,_Gc));}),_13G=new T(function(){return B(_EE(_Fu,_13F));}),_13H=new T(function(){return B(unCStr("Amount"));}),_13I=new T(function(){return B(_FW(_0,_13H));}),_13J=new T(function(){return B(_EE(_Fu,_13I));}),_13K=function(_13L,_){var _13M=B(A(_13B,[_13L,_])),_13N=_13M,_13O=B(A(_8,[_e,_13N,_fE,_fF,_])),_13P=_13O,_13Q=B(A(_13E,[_13L,_])),_13R=_13Q,_13S=B(A(_8,[_e,_13R,_fE,_fF,_])),_13T=_13S,_13U=B(A(_13G,[_13L,_])),_13V=_13U,_13W=B(A(_8,[_e,_13V,_fE,_fF,_])),_13X=_13W,_13Y=B(A(_13J,[_13L,_])),_13Z=_13Y,_140=B(A(_8,[_e,_13Z,_fE,_fF,_])),_141=_140,_142=B(_Dx(_13L,_)),_143=_142;return _13L;},_144=new T(function(){return B(_Fk(_Fu,_13K));}),_145=function(_146){return E(E(_146)[1]);},_147=function(_148){return E(E(_148)[7]);},_149=[0,0],_14a=function(_14b,_14c){return new F(function(){return (function(_14d,_14e){while(1){var _14f=(function(_14g,_14h){var _14i=E(_14g);if(!_14i[0]){return E(_14h);}else{_14d=_14i[2];_14e=new T(function(){return B(A(new T(function(){return B(_145(_14b));}),[_14h,_14i[1]]));});return null;}})(_14d,_14e);if(_14f!=null){return _14f;}}})(_14c,new T(function(){return B(A(_147,[_14b,_149]));}));});},_14j=new T(function(){return B(unCStr("Balance: "));}),_14k=new T(function(){return B(_EE(_0,_14j));}),_14l=function(_14m,_14n,_14o){return function(_8i,_8j){return new F(function(){return _3Q(new T(function(){return B(_SG(_14m,new T(function(){var _14p=E(_14n),_14q=_14p[1];if(_14q<=1){var _14r=E(_14p);}else{var _14r=[0,_14q-1|0];}var _14s=_14r;return _14s;}),_14o));}),function(_14t){var _14u=E(_14t),_14v=_14u[2];return function(_8i,_8j){return new F(function(){return _3Q(_11H,function(_14w,_Dv,_){return new F(function(){return (function(_Dv,_){return new F(function(){return _3Q(new T(function(){return B(_SG(_14m,_14n,_14o));}),function(_14x,_14y,_){var _14z=E(_14x);return new F(function(){return (function(_14A,_14B,_14C,_){return new F(function(){return _3Q(_OW,function(_14D){var _14E=new T(function(){return B(_12K(function(_14F){var _14G=E(_14F);return new F(function(){return (function(_14H,_14I,_14J){var _14K=E(_14u[3])[1],_14L=new T(function(){var _14M=new T(function(){if(E(_14I)[1]!=E(_14v)[1]){var _14N=E(_14H),_14O=true;}else{var _14P=E(_14H),_14Q=E(_14u[1]),_14O=true;}var _14R=_14O,_14S=_14R;return _14S;});if(_14J!=E(_14B)[1]){var _14T=E(_14M);}else{var _14T=E(_14I)[1]>=E(_14A)[1]?E(_14M):true;}var _14U=_14T;return _14U;}),_14V=new T(function(){if(_14J!=_14K){var _14W=E(_14L);}else{var _14W=E(_14I)[1]<=E(_14v)[1]?E(_14L):true;}return _14W;});return _14J<=_14K?E(_14V):_14J>=E(_14B)[1]?E(_14V):true;})(_14G[1],_14G[2],E(_14G[3])[1]);});},_14D));}),_14X=new T(function(){return B(_14a(_11t,B(_131(_14E))));}),_14Y=new T(function(){return B(_14a(_11t,B(_138(_14E))));}),_14Z=new T(function(){return B(_14a(_11t,B(_13f(_14E))));}),_150=new T(function(){return B(_14a(_11t,B(_13m(_14E))));}),_151=new T(function(){return B(_14a(_11t,B(_12U(_14E))));});return function(_8i,_8j){return new F(function(){return _3Q(function(_152,_){var _153=B(A(_127,[_152,_])),_154=_153,_155=E(_154),_156=E(_155[1]),_157=B(A(_12a,[_155[2],_])),_158=_157,_159=E(_158),_15a=E(_159[1]),_15b=B(A(_12d,[_159[2],_])),_15c=_15b,_15d=E(_15c),_15e=E(_15d[1]),_15f=B(A(_12g,[_15d[2],_])),_15g=_15f,_15h=E(_15g),_15i=E(_15h[1]),_15j=B(A(_12j,[_15h[2],_])),_15k=_15j,_15l=E(_15k),_15m=E(_15l[1]),_15n=B(_FK([0,_13w,[0,_vM,_150],[0,_D2,_14Z],[0,_Gb,_14Y],[0,_Dc,_14X],[0,_Dh,_151]],_15l[2],_)),_15o=_15n,_15p=E(_15o);return [0,[0,function(_15q,_){var _15r=B(A(new T(function(){return B(_EE(_Fu,function(_15s,_){var _15t=B(A(_156[1],[_15s,_])),_15u=_15t,_15v=B(A(new T(function(){return B(_FW(_11u,_151));}),[_15s,_])),_15w=_15v;return _15s;}));}),[_15q,_])),_15x=_15r,_15y=B(A(_8,[_e,_15x,_fE,_fF,_])),_15z=_15y,_15A=B(A(new T(function(){return B(_EE(_Fu,function(_15B,_){var _15C=B(A(_15a[1],[_15B,_])),_15D=_15C,_15E=B(A(new T(function(){return B(_FW(_11u,_150));}),[_15B,_])),_15F=_15E;return _15B;}));}),[_15q,_])),_15G=_15A,_15H=B(A(_8,[_e,_15G,_fE,_fF,_])),_15I=_15H,_15J=B(A(new T(function(){return B(_EE(_Fu,function(_15K,_){var _15L=B(A(_15e[1],[_15K,_])),_15M=_15L,_15N=B(A(new T(function(){return B(_FW(_11u,_14Z));}),[_15K,_])),_15O=_15N;return _15K;}));}),[_15q,_])),_15P=_15J,_15Q=B(A(_8,[_e,_15P,_fE,_fF,_])),_15R=_15Q,_15S=B(A(new T(function(){return B(_EE(_Fu,function(_15T,_){var _15U=B(A(_15i[1],[_15T,_])),_15V=_15U,_15W=B(A(new T(function(){return B(_FW(_11u,_14Y));}),[_15T,_])),_15X=_15W;return _15T;}));}),[_15q,_])),_15Y=_15S,_15Z=B(A(_8,[_e,_15Y,_fE,_fF,_])),_160=_15Z,_161=B(A(new T(function(){return B(_EE(_Fu,function(_162,_){var _163=B(A(_15m[1],[_162,_])),_164=_163,_165=B(A(new T(function(){return B(_FW(_11u,_14X));}),[_162,_])),_166=_165;return _162;}));}),[_15q,_])),_167=_161,_168=B(A(_8,[_e,_167,_fE,_fF,_])),_169=_168,_16a=B(_Dx(_15q,_)),_16b=_16a,_16c=B(_Dx(_15q,_)),_16d=_16c,_16e=B(A(_14k,[_15q,_])),_16f=_16e,_16g=B(A(_8,[_e,_16f,_fE,_fF,_])),_16h=_16g,_16i=B(A(new T(function(){return B(_FW(_11u,new T(function(){return [0,E(_151)[1]-E(_150)[1]-E(_14Z)[1]-E(_14Y)[1]-E(_14X)[1]];})));}),[_15q,_])),_16j=_16i,_16k=B(A(E(_15p[1])[1],[_15q,_])),_16l=_16k;return _15q;},new T(function(){var _16m=E(_156[2]);if(!_16m[0]){var _16n=E(_15a[2]);if(!_16n[0]){var _16o=E(_15e[2]);if(!_16o[0]){var _16p=E(_15i[2]);if(!_16p[0]){var _16q=E(_15m[2]),_16r=_16q[0]==0?E(_Gt):E(_16q);}else{var _16r=E(_16p);}var _16s=_16r;}else{var _16s=E(_16o);}var _16t=_16s;}else{var _16t=E(_16n);}var _16u=_16t;}else{var _16u=E(_16m);}return _16u;})],_15p[2]];},function(_16v){return function(_16w,_){return [0,[0,function(_16x,_){var _16y=B(A(_13y,[_16x,_])),_16z=_16y,_16A=B(A(_144,[_16x,_])),_16B=_16A,_16C=B(_yi(new T(function(){var _16D=E(_16v);if(_16D==5){var _16E=E(new T(function(){return B(_13t(_14E));}));}else{var _16E=B((function(_16F){while(1){var _16G=(function(_16H){var _16I=E(_16H);if(!_16I[0]){return [0];}else{var _16J=_16I[2],_16K=E(_16I[1]);switch(E(_16K[6])){case 0:switch(E(_16D)){case 0:return new F(function(){return _12n(_16K,new T(function(){var _16L=function(_16M){while(1){var _16N=(function(_16O){var _16P=E(_16O);if(!_16P[0]){return [0];}else{var _16Q=_16P[2],_16R=E(_16P[1]);switch(E(_16R[6])){case 0:return new F(function(){return _12n(_16R,new T(function(){return B(_16L(_16Q));}));});break;case 1:_16M=_16Q;return null;case 2:_16M=_16Q;return null;case 3:_16M=_16Q;return null;case 4:_16M=_16Q;return null;default:_16M=_16Q;return null;}}})(_16M);if(_16N!=null){return _16N;}}};return B(_16L(_16J));}));});break;case 1:var _16S=function(_16T){while(1){var _16U=(function(_16V){var _16W=E(_16V);if(!_16W[0]){return [0];}else{var _16X=_16W[2],_16Y=E(_16W[1]);if(E(_16Y[6])==1){return new F(function(){return _12n(_16Y,new T(function(){return B(_16S(_16X));}));});}else{_16T=_16X;return null;}}})(_16T);if(_16U!=null){return _16U;}}};return new F(function(){return _16S(_16J);});break;case 2:var _16Z=function(_170){while(1){var _171=(function(_172){var _173=E(_172);if(!_173[0]){return [0];}else{var _174=_173[2],_175=E(_173[1]);if(E(_175[6])==2){return new F(function(){return _12n(_175,new T(function(){return B(_16Z(_174));}));});}else{_170=_174;return null;}}})(_170);if(_171!=null){return _171;}}};return new F(function(){return _16Z(_16J);});break;case 3:var _176=function(_177){while(1){var _178=(function(_179){var _17a=E(_179);if(!_17a[0]){return [0];}else{var _17b=_17a[2],_17c=E(_17a[1]);if(E(_17c[6])==3){return new F(function(){return _12n(_17c,new T(function(){return B(_176(_17b));}));});}else{_177=_17b;return null;}}})(_177);if(_178!=null){return _178;}}};return new F(function(){return _176(_16J);});break;default:var _17d=function(_17e){while(1){var _17f=(function(_17g){var _17h=E(_17g);if(!_17h[0]){return [0];}else{var _17i=_17h[2],_17j=E(_17h[1]);if(E(_17j[6])==4){return new F(function(){return _12n(_17j,new T(function(){return B(_17d(_17i));}));});}else{_17e=_17i;return null;}}})(_17e);if(_17f!=null){return _17f;}}};return new F(function(){return _17d(_16J);});}break;case 1:var _17k=E(_16D);if(_17k==1){return new F(function(){return _12n(_16K,new T(function(){var _17l=function(_17m){while(1){var _17n=(function(_17o){var _17p=E(_17o);if(!_17p[0]){return [0];}else{var _17q=_17p[2],_17r=E(_17p[1]);if(E(_17r[6])==1){return new F(function(){return _12n(_17r,new T(function(){return B(_17l(_17q));}));});}else{_17m=_17q;return null;}}})(_17m);if(_17n!=null){return _17n;}}};return B(_17l(_16J));}));});}else{var _17s=function(_17t){while(1){var _17u=(function(_17v){var _17w=E(_17v);if(!_17w[0]){return [0];}else{var _17x=_17w[2],_17y=E(_17w[1]);switch(E(_17y[6])){case 0:switch(E(_17k)){case 0:return new F(function(){return _12n(_17y,new T(function(){return B(_17s(_17x));}));});break;case 2:_17t=_17x;return null;case 3:_17t=_17x;return null;default:_17t=_17x;return null;}break;case 1:_17t=_17x;return null;case 2:if(E(_17k)==2){return new F(function(){return _12n(_17y,new T(function(){return B(_17s(_17x));}));});}else{_17t=_17x;return null;}break;case 3:if(E(_17k)==3){return new F(function(){return _12n(_17y,new T(function(){return B(_17s(_17x));}));});}else{_17t=_17x;return null;}break;case 4:if(E(_17k)==4){return new F(function(){return _12n(_17y,new T(function(){return B(_17s(_17x));}));});}else{_17t=_17x;return null;}break;default:_17t=_17x;return null;}}})(_17t);if(_17u!=null){return _17u;}}};return new F(function(){return _17s(_16J);});}break;case 2:var _17z=E(_16D);if(_17z==2){return new F(function(){return _12n(_16K,new T(function(){var _17A=function(_17B){while(1){var _17C=(function(_17D){var _17E=E(_17D);if(!_17E[0]){return [0];}else{var _17F=_17E[2],_17G=E(_17E[1]);if(E(_17G[6])==2){return new F(function(){return _12n(_17G,new T(function(){return B(_17A(_17F));}));});}else{_17B=_17F;return null;}}})(_17B);if(_17C!=null){return _17C;}}};return B(_17A(_16J));}));});}else{var _17H=function(_17I){while(1){var _17J=(function(_17K){var _17L=E(_17K);if(!_17L[0]){return [0];}else{var _17M=_17L[2],_17N=E(_17L[1]);switch(E(_17N[6])){case 0:switch(E(_17z)){case 0:return new F(function(){return _12n(_17N,new T(function(){return B(_17H(_17M));}));});break;case 1:_17I=_17M;return null;case 3:_17I=_17M;return null;default:_17I=_17M;return null;}break;case 1:if(E(_17z)==1){return new F(function(){return _12n(_17N,new T(function(){return B(_17H(_17M));}));});}else{_17I=_17M;return null;}break;case 2:_17I=_17M;return null;case 3:if(E(_17z)==3){return new F(function(){return _12n(_17N,new T(function(){return B(_17H(_17M));}));});}else{_17I=_17M;return null;}break;case 4:if(E(_17z)==4){return new F(function(){return _12n(_17N,new T(function(){return B(_17H(_17M));}));});}else{_17I=_17M;return null;}break;default:_17I=_17M;return null;}}})(_17I);if(_17J!=null){return _17J;}}};return new F(function(){return _17H(_16J);});}break;case 3:var _17O=E(_16D);if(_17O==3){return new F(function(){return _12n(_16K,new T(function(){var _17P=function(_17Q){while(1){var _17R=(function(_17S){var _17T=E(_17S);if(!_17T[0]){return [0];}else{var _17U=_17T[2],_17V=E(_17T[1]);if(E(_17V[6])==3){return new F(function(){return _12n(_17V,new T(function(){return B(_17P(_17U));}));});}else{_17Q=_17U;return null;}}})(_17Q);if(_17R!=null){return _17R;}}};return B(_17P(_16J));}));});}else{var _17W=function(_17X){while(1){var _17Y=(function(_17Z){var _180=E(_17Z);if(!_180[0]){return [0];}else{var _181=_180[2],_182=E(_180[1]);switch(E(_182[6])){case 0:switch(E(_17O)){case 0:return new F(function(){return _12n(_182,new T(function(){return B(_17W(_181));}));});break;case 1:_17X=_181;return null;case 2:_17X=_181;return null;default:_17X=_181;return null;}break;case 1:if(E(_17O)==1){return new F(function(){return _12n(_182,new T(function(){return B(_17W(_181));}));});}else{_17X=_181;return null;}break;case 2:if(E(_17O)==2){return new F(function(){return _12n(_182,new T(function(){return B(_17W(_181));}));});}else{_17X=_181;return null;}break;case 3:_17X=_181;return null;case 4:if(E(_17O)==4){return new F(function(){return _12n(_182,new T(function(){return B(_17W(_181));}));});}else{_17X=_181;return null;}break;default:_17X=_181;return null;}}})(_17X);if(_17Y!=null){return _17Y;}}};return new F(function(){return _17W(_16J);});}break;case 4:var _183=E(_16D);if(_183==4){return new F(function(){return _12n(_16K,new T(function(){var _184=function(_185){while(1){var _186=(function(_187){var _188=E(_187);if(!_188[0]){return [0];}else{var _189=_188[2],_18a=E(_188[1]);if(E(_18a[6])==4){return new F(function(){return _12n(_18a,new T(function(){return B(_184(_189));}));});}else{_185=_189;return null;}}})(_185);if(_186!=null){return _186;}}};return B(_184(_16J));}));});}else{var _18b=function(_18c){while(1){var _18d=(function(_18e){var _18f=E(_18e);if(!_18f[0]){return [0];}else{var _18g=_18f[2],_18h=E(_18f[1]);switch(E(_18h[6])){case 0:switch(E(_183)){case 0:return new F(function(){return _12n(_18h,new T(function(){return B(_18b(_18g));}));});break;case 1:_18c=_18g;return null;case 2:_18c=_18g;return null;default:_18c=_18g;return null;}break;case 1:if(E(_183)==1){return new F(function(){return _12n(_18h,new T(function(){return B(_18b(_18g));}));});}else{_18c=_18g;return null;}break;case 2:if(E(_183)==2){return new F(function(){return _12n(_18h,new T(function(){return B(_18b(_18g));}));});}else{_18c=_18g;return null;}break;case 3:if(E(_183)==3){return new F(function(){return _12n(_18h,new T(function(){return B(_18b(_18g));}));});}else{_18c=_18g;return null;}break;case 4:_18c=_18g;return null;default:_18c=_18g;return null;}}})(_18c);if(_18d!=null){return _18d;}}};return new F(function(){return _18b(_16J);});}break;default:_16F=_16J;return null;}}})(_16F);if(_16G!=null){return _16G;}}})(_14E));}return _16E;}),_16x,_)),_18i=_16C;return _16x;},_4E],_16w];};},_8i,_8j);});};},_14C,_);});})(_14z[2],_14z[3],_14y,_);});},_Dv,_);});})(_Dv,_);});},_8i,_8j);});};},_8i,_8j);});};},_18j=function(_18k){var _18l=E(_18k);return new F(function(){return _14l(_18l[1],_18l[2],_18l[3]);});},_18m=function(_Dv,_){return new F(function(){return _3Q(_10U,_18j,_Dv,_);});},_18n=function(_18o){return E(_18m);},_18p=function(_Dv,_){return new F(function(){return _3Q(_10S,_18n,_Dv,_);});},_18q=new T(function(){return B(_gv("mybudget.hs:(50,5)-(53,23)|case"));}),_18r=[0,_2Q,_Gu],_18s=function(_18t,_18u,_){return [0,_18r,_18u];},_18v=function(_18w,_18x,_){return new F(function(){return _3Q(new T(function(){switch(E(_18w)){case 0:var _18y=E(_10G);break;case 1:var _18y=E(_18p);break;case 2:var _18y=E(_Il);break;default:var _18y=E(_18q);}return _18y;}),_18s,_18x,_);});},_18z=function(_Dv,_){return new F(function(){return _3Q(_a2,_18v,_Dv,_);});},_18A=function(_18B){return E(_18z);},_18C=new T(function(){return B(unCStr("text/javascript"));}),_18D=new T(function(){return B(unCStr("type"));}),_18E=new T(function(){return B(unCStr("var options;function init(){google.load(\'visualization\', \'1\', {packages:[\'corechart\'],\'callback\' : drawChart});function drawChart() {options = {title: \'Preview expenses\'};}}function waitGoogle(){if (typeof google !== \'undefined\') {init();}else{window.setTimeout(function(){waitGoogle();}, 10);}}waitGoogle();"));}),_18F=new T(function(){return B(unCStr("https://www.google.com/jsapi"));}),_18G=new T(function(){return B(unCStr(".label1 {float: left;width: 20%;}"));}),_18H=new T(function(){return B(unCStr("style"));}),_18I=new T(function(){return B(unCStr("script"));}),_18J=function(_18K,_18L){return function(_18M,_){var _18N=jsCreateElem(toJSStr(E(_18I))),_18O=_18N,_18P=jsAppendChild(_18O,E(_18M)[1]),_18Q=[0,_18O],_18R=B(A(new T(function(){return B(A(_18K,[_18L]));}),[_18Q,_])),_18S=_18R;return _18Q;};},_18T=new T(function(){return B(unCStr("src"));}),_18U=function(_){var _18V=B(A(_52,[_])),_18W=_18V,_18X=B(_y(_18H,[0,_18W],_)),_18Y=_18X,_18Z=B(_0(_18G,_18Y,_)),_190=_18Z,_191=B(A(_52,[_])),_192=_191,_193=[0,_192],_194=B(A(_18J,[_Fu,_2Q,_193,_])),_195=_194,_196=E(_195),_197=E(_18D),_198=E(_18C),_199=jsSetAttr(_196[1],toJSStr(_197),toJSStr(_198)),_19a=B(A(_8,[_e,_196,_18T,_18F,_])),_19b=_19a,_19c=B(A(_18J,[_0,_18E,_193,_])),_19d=_19c,_19e=jsSetAttr(E(_19d)[1],toJSStr(_197),toJSStr(_198)),_19f=B(A(_8l,[_])),_19g=_19f,_19h=E(_5l)[1],_19i=takeMVar(_19h),_19j=_19i,_19k=B(_3Q(_4T,_18A,_19j,_)),_19l=_19k,_19m=E(_19l),_19n=E(_19m[1]),_=putMVar(_19h,_19m[2]),_19o=B(A(_19n[1],[[0,_19g],_])),_19p=_19o;return _19n[2];},_19q=function(_){return new F(function(){return _18U(_);});};
var hasteMain = function() {B(A(_19q, [0]));};window.onload = hasteMain;