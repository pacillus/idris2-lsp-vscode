#!/usr/bin/env node
class IdrisError extends Error { }

function __prim_js2idris_array(x){
  let acc = { h:0 };

  for (let i = x.length-1; i>=0; i--) {
      acc = { a1:x[i], a2:acc };
  }
  return acc;
}

function __prim_idris2js_array(x){
  const result = Array();
  while (x.h === undefined) {
    result.push(x.a1); x = x.a2;
  }
  return result;
}

function __lazy(thunk) {
  let res;
  return function () {
    if (thunk === undefined) return res;
    res = thunk();
    thunk = undefined;
    return res;
  };
};

function __prim_stringIteratorNew(_str) {
  return 0
}

function __prim_stringIteratorToString(_, str, it, f) {
  return f(str.slice(it))
}

function __prim_stringIteratorNext(str, it) {
  if (it >= str.length)
    return {h: 0};
  else
    return {a1: str.charAt(it), a2: it + 1};
}

function __tailRec(f,ini) {
  let obj = ini;
  while(true){
    switch(obj.h){
      case 0: return obj.a1;
      default: obj = f(obj);
    }
  }
}

const _idrisworld = Symbol('idrisworld')

const _crashExp = x=>{throw new IdrisError(x)}

const _bigIntOfString = s=> {
  try {
    const idx = s.indexOf('.')
    return idx === -1 ? BigInt(s) : BigInt(s.slice(0, idx))
  } catch (e) { return 0n }
}

const _numberOfString = s=> {
  try {
    const res = Number(s);
    return isNaN(res) ? 0 : res;
  } catch (e) { return 0 }
}

const _intOfString = s=> Math.trunc(_numberOfString(s))

const _truncToChar = x=> String.fromCodePoint(
  (x >= 0 && x <= 55295) || (x >= 57344 && x <= 1114111) ? x : 0
)

// Int8
const _truncInt8 = x => {
  const res = x & 0xff;
  return res >= 0x80 ? res - 0x100 : res;
}

const _truncBigInt8 = x => Number(BigInt.asIntN(8, x))

// Euclidian Division
const _div = (a,b) => {
  const q = Math.trunc(a / b)
  const r = a % b
  return r < 0 ? (b > 0 ? q - 1 : q + 1) : q
}

const _divBigInt = (a,b) => {
  const q = a / b
  const r = a % b
  return r < 0n ? (b > 0n ? q - 1n : q + 1n) : q
}

// Euclidian Modulo
const _mod = (a,b) => {
  const r = a % b
  return r < 0 ? (b > 0 ? r + b : r - b) : r
}

const _modBigInt = (a,b) => {
  const r = a % b
  return r < 0n ? (b > 0n ? r + b : r - b) : r
}

const _add8s = (a,b) => _truncInt8(a + b)
const _sub8s = (a,b) => _truncInt8(a - b)
const _mul8s = (a,b) => _truncInt8(a * b)
const _div8s = (a,b) => _truncInt8(_div(a,b))
const _shl8s = (a,b) => _truncInt8(a << b)
const _shr8s = (a,b) => _truncInt8(a >> b)

// Int16
const _truncInt16 = x => {
  const res = x & 0xffff;
  return res >= 0x8000 ? res - 0x10000 : res;
}

const _truncBigInt16 = x => Number(BigInt.asIntN(16, x))

const _add16s = (a,b) => _truncInt16(a + b)
const _sub16s = (a,b) => _truncInt16(a - b)
const _mul16s = (a,b) => _truncInt16(a * b)
const _div16s = (a,b) => _truncInt16(_div(a,b))
const _shl16s = (a,b) => _truncInt16(a << b)
const _shr16s = (a,b) => _truncInt16(a >> b)

//Int32
const _truncInt32 = x => x & 0xffffffff

const _truncBigInt32 = x => Number(BigInt.asIntN(32, x))

const _add32s = (a,b) => _truncInt32(a + b)
const _sub32s = (a,b) => _truncInt32(a - b)
const _div32s = (a,b) => _truncInt32(_div(a,b))

const _mul32s = (a,b) => {
  const res = a * b;
  if (res <= Number.MIN_SAFE_INTEGER || res >= Number.MAX_SAFE_INTEGER) {
    return _truncInt32((a & 0xffff) * b + (b & 0xffff) * (a & 0xffff0000))
  } else {
    return _truncInt32(res)
  }
}

//Int64
const _truncBigInt64 = x => BigInt.asIntN(64, x)

const _add64s = (a,b) => _truncBigInt64(a + b)
const _sub64s = (a,b) => _truncBigInt64(a - b)
const _mul64s = (a,b) => _truncBigInt64(a * b)
const _shl64s = (a,b) => _truncBigInt64(a << b)
const _div64s = (a,b) => _truncBigInt64(_divBigInt(a,b))
const _shr64s = (a,b) => _truncBigInt64(a >> b)

//Bits8
const _truncUInt8 = x => x & 0xff

const _truncUBigInt8 = x => Number(BigInt.asUintN(8, x))

const _add8u = (a,b) => (a + b) & 0xff
const _sub8u = (a,b) => (a - b) & 0xff
const _mul8u = (a,b) => (a * b) & 0xff
const _div8u = (a,b) => Math.trunc(a / b)
const _shl8u = (a,b) => (a << b) & 0xff
const _shr8u = (a,b) => (a >> b) & 0xff

//Bits16
const _truncUInt16 = x => x & 0xffff

const _truncUBigInt16 = x => Number(BigInt.asUintN(16, x))

const _add16u = (a,b) => (a + b) & 0xffff
const _sub16u = (a,b) => (a - b) & 0xffff
const _mul16u = (a,b) => (a * b) & 0xffff
const _div16u = (a,b) => Math.trunc(a / b)
const _shl16u = (a,b) => (a << b) & 0xffff
const _shr16u = (a,b) => (a >> b) & 0xffff

//Bits32
const _truncUBigInt32 = x => Number(BigInt.asUintN(32, x))

const _truncUInt32 = x => {
  const res = x & -1;
  return res < 0 ? res + 0x100000000 : res;
}

const _add32u = (a,b) => _truncUInt32(a + b)
const _sub32u = (a,b) => _truncUInt32(a - b)
const _mul32u = (a,b) => _truncUInt32(_mul32s(a,b))
const _div32u = (a,b) => Math.trunc(a / b)

const _shl32u = (a,b) => _truncUInt32(a << b)
const _shr32u = (a,b) => _truncUInt32(a <= 0x7fffffff ? a >> b : (b == 0 ? a : (a >> b) ^ ((-0x80000000) >> (b-1))))
const _and32u = (a,b) => _truncUInt32(a & b)
const _or32u = (a,b)  => _truncUInt32(a | b)
const _xor32u = (a,b) => _truncUInt32(a ^ b)

//Bits64
const _truncUBigInt64 = x => BigInt.asUintN(64, x)

const _add64u = (a,b) => BigInt.asUintN(64, a + b)
const _mul64u = (a,b) => BigInt.asUintN(64, a * b)
const _div64u = (a,b) => a / b
const _shl64u = (a,b) => BigInt.asUintN(64, a << b)
const _shr64u = (a,b) => BigInt.asUintN(64, a >> b)
const _sub64u = (a,b) => BigInt.asUintN(64, a - b)

//String
const _strReverse = x => x.split('').reverse().join('')

const _substr = (o,l,x) => x.slice(o, o + l)

const Prelude_Types_fastUnpack = ((str)=>__prim_js2idris_array(Array.from(str)));
const Prelude_Types_fastPack = ((xs)=>__prim_idris2js_array(xs).join(''));
const Prelude_IO_prim__putStr = (x=>process.stdout.write(x));
const System_prim__getArgCount = (() => process.argv.length - 1);
const System_prim__getArg = (n => process.argv[n + 1]);
/* {$tcOpt:1} */
function x24tcOpt_1($0) {
 switch($0.h) {
  case 1: /* {TcContinue1:1} */ {
   switch($0.a7.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:1} */, a1: {h: 0}};
    case undefined: /* cons */ return {h: 2 /* {TcContinue1:2} */, a1: $0.a1, a2: $0.a2, a3: $0.a3, a4: $0.a4, a5: $0.a5, a6: $0.a6, a7: $0.a7.a1.a1, a8: $0.a7.a1.a2, a9: $0.a7.a2, a10: $0.a8, a11: Text_Lexer_Core_scan($0.a7.a1.a1, {h: 0}, $0.a8)};
   }
  }
  case 2: /* {TcContinue1:2} */ {
   switch($0.a11.h) {
    case undefined: /* just */ {
     const $16 = _add32s($0.a5, Number(_truncBigInt32(Text_Lexer_Core_n__3934_2499_countNLs($0.a1, $0.a2, $0.a3, $0.a4, $0.a5, $0.a6, $0.a11.a1.a1))));
     const $22 = Text_Lexer_Core_n__3934_2500_getCols($0.a1, $0.a2, $0.a3, $0.a4, $0.a5, $0.a6, $0.a11.a1.a1, $0.a4);
     return {h: 0 /* {TcDone:1} */, a1: {a1: {a1: {a1: $0.a8(Prelude_Types_fastPack(Prelude_Types_List_reverse($0.a11.a1.a1))), a2: 0, a3: {a1: $0.a5, a2: $0.a4, a3: $16, a4: $22}}, a2: {a1: $16, a2: {a1: $22, a2: $0.a11.a1.a2}}}}};
    }
    case 0: /* nothing */ return {h: 1 /* {TcContinue1:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3, a4: $0.a4, a5: $0.a5, a6: $0.a6, a7: $0.a9, a8: $0.a10};
   }
  }
 }
}

/* Text.Lexer.Core.3934:2501:getFirstToken */
function Text_Lexer_Core_n__3934_2501_getFirstToken($0, $1, $2, $3, $4, $5, $6, $7) {
 return __tailRec(x24tcOpt_1, {h: 1 /* {TcContinue1:1} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5, a7: $6, a8: $7});
}

/* Text.Lexer.Core.case block in tokenise,getFirstToken */
function Text_Lexer_Core_case__tokenisex2cgetFirstToken_2634($0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a) {
 return __tailRec(x24tcOpt_1, {h: 2 /* {TcContinue1:2} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5, a7: $6, a8: $7, a9: $8, a10: $9, a11: $a});
}

/* {$tcOpt:2} */
function x24tcOpt_2($0) {
 switch($0.h) {
  case 1: /* {TcContinue2:1} */ return {h: 2 /* {TcContinue2:2} */, a1: $0.a6, a2: $0.a5, a3: $0.a4, a4: $0.a3, a5: $0.a2, a6: $0.a1, a7: Text_Lexer_Core_n__3934_2501_getFirstToken($0.a6, $0.a5, $0.a4, $0.a3, $0.a2, $0.a1, $0.a5, $0.a6)};
  case 2: /* {TcContinue2:2} */ {
   switch($0.a7.h) {
    case undefined: /* just */ {
     switch($0.a6($0.a7.a1.a1.a1)) {
      case 1: return {h: 0 /* {TcDone:2} */, a1: {a1: Prelude_Types_List_reverse($0.a3), a2: {a1: $0.a5, a2: {a1: $0.a4, a2: {h: 0}}}}};
      case 0: return {h: 1 /* {TcContinue2:1} */, a1: $0.a6, a2: $0.a7.a1.a2.a1, a3: $0.a7.a1.a2.a2.a1, a4: {a1: $0.a7.a1.a1, a2: $0.a3}, a5: $0.a2, a6: $0.a7.a1.a2.a2.a2};
     }
    }
    case 0: /* nothing */ return {h: 0 /* {TcDone:2} */, a1: {a1: Prelude_Types_List_reverse($0.a3), a2: {a1: $0.a5, a2: {a1: $0.a4, a2: $0.a1}}}};
   }
  }
 }
}

/* Text.Lexer.Core.tokenise : (a -> Bool) -> Int -> Int -> List (WithBounds a) -> TokenMap a -> List Char -> (List (WithBounds a),
(Int, (Int, List Char))) */
function Text_Lexer_Core_tokenise($0, $1, $2, $3, $4, $5) {
 return __tailRec(x24tcOpt_2, {h: 1 /* {TcContinue2:1} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5});
}

/* Text.Lexer.Core.case block in tokenise */
function Text_Lexer_Core_case__tokenise_2724($0, $1, $2, $3, $4, $5, $6) {
 return __tailRec(x24tcOpt_2, {h: 2 /* {TcContinue2:2} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5, a7: $6});
}

/* {$tcOpt:3} */
function x24tcOpt_3($0) {
 switch($0.a2.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:3} */, a1: $0.a1};
  case undefined: /* cons */ return {h: 1 /* {TcContinue3:1} */, a1: {a1: $0.a2.a1, a2: $0.a1}, a2: $0.a2.a2};
 }
}

/* Prelude.Types.List.reverseOnto : List a -> List a -> List a */
function Prelude_Types_List_reverseOnto($0, $1) {
 return __tailRec(x24tcOpt_3, {h: 1 /* {TcContinue3:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:4} */
function x24tcOpt_4($0) {
 switch($0.a2) {
  case 0n: return {h: 0 /* {TcDone:4} */, a1: $0.a1};
  default: {
   const $4 = ($0.a2-1n);
   return {h: 1 /* {TcContinue4:1} */, a1: {a1: $0.a3, a2: $0.a1}, a2: $4, a3: $0.a3};
  }
 }
}

/* Data.List.replicateTR : List a -> Nat -> a -> List a */
function Data_List_replicateTR($0, $1, $2) {
 return __tailRec(x24tcOpt_4, {h: 1 /* {TcContinue4:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:5} */
function x24tcOpt_5($0) {
 switch($0.a3.h) {
  case undefined: /* cons */ return {h: 1 /* {TcContinue5:1} */, a1: {a1: $0.a1, a2: $0.a2($0.a3.a1)}, a2: $0.a2, a3: $0.a3.a2};
  case 0: /* nil */ return {h: 0 /* {TcDone:5} */, a1: Prelude_Types_SnocList_x3cx3ex3e($0.a1, {h: 0})};
 }
}

/* Prelude.Types.List.mapAppend : SnocList b -> (a -> b) -> List a -> List b */
function Prelude_Types_List_mapAppend($0, $1, $2) {
 return __tailRec(x24tcOpt_5, {h: 1 /* {TcContinue5:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:6} */
function x24tcOpt_6($0) {
 switch($0.a3.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:6} */, a1: {h: 0}};
  case undefined: /* cons */ {
   switch($0.a1($0.a2)($0.a3.a1.a1)) {
    case 1: return {h: 0 /* {TcDone:6} */, a1: {a1: $0.a3.a1.a2}};
    case 0: return {h: 1 /* {TcContinue6:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3.a2};
   }
  }
 }
}

/* Data.List.lookupBy : (a -> b -> Bool) -> a -> List (b, v) -> Maybe v */
function Data_List_lookupBy($0, $1, $2) {
 return __tailRec(x24tcOpt_6, {h: 1 /* {TcContinue6:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:7} */
function x24tcOpt_7($0) {
 switch($0.a2.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:7} */, a1: $0.a1};
  case undefined: /* cons */ return {h: 1 /* {TcContinue7:1} */, a1: ($0.a1+1n), a2: $0.a2.a2};
 }
}

/* Prelude.Types.List.lengthPlus : Nat -> List a -> Nat */
function Prelude_Types_List_lengthPlus($0, $1) {
 return __tailRec(x24tcOpt_7, {h: 1 /* {TcContinue7:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:8} */
function x24tcOpt_8($0) {
 switch($0.a3.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:8} */, a1: $0.a2};
  case undefined: /* cons */ return {h: 1 /* {TcContinue8:1} */, a1: $0.a1, a2: $0.a1($0.a2)($0.a3.a1), a3: $0.a3.a2};
 }
}

/* Prelude.Types.foldl */
function Prelude_Types_foldl_Foldable_List($0, $1, $2) {
 return __tailRec(x24tcOpt_8, {h: 1 /* {TcContinue8:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:9} */
function x24tcOpt_9($0) {
 switch($0.a1.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:9} */, a1: {a1: {h: 0}, a2: {h: 0}}};
  case undefined: /* cons */ {
   switch($0.a1.a1.a1.h) {
    case undefined: /* cons */ {
     switch($0.a1.a1.a1.a1) {
      case 1: {
       switch($0.a1.a1.a3.h) {
        case undefined: /* record */ return {h: 1 /* {TcContinue9:1} */, a1: $0.a1.a2};
        default: {
         const $c = Pacillus_Idris2LSP_Lex_getTokPosx27($0.a1.a2);
         return {h: 0 /* {TcDone:9} */, a1: {a1: {a1: $0.a1.a1.a3.a2, a2: Builtin_fst($c)}, a2: Builtin_snd($c)}};
        }
       }
      }
      case 0: {
       switch($0.a1.a1.a3.h) {
        case undefined: /* record */ {
         const $19 = Pacillus_Idris2LSP_Lex_getTokPosx27($0.a1.a2);
         return {h: 0 /* {TcDone:9} */, a1: {a1: {a1: $0.a1.a1.a3.a2, a2: Builtin_fst($19)}, a2: {a1: $0.a1.a1.a1.a2, a2: Builtin_snd($19)}}};
        }
        default: {
         const $28 = Pacillus_Idris2LSP_Lex_getTokPosx27($0.a1.a2);
         return {h: 0 /* {TcDone:9} */, a1: {a1: {a1: $0.a1.a1.a3.a2, a2: Builtin_fst($28)}, a2: Builtin_snd($28)}};
        }
       }
      }
      default: {
       const $35 = Pacillus_Idris2LSP_Lex_getTokPosx27($0.a1.a2);
       return {h: 0 /* {TcDone:9} */, a1: {a1: {a1: $0.a1.a1.a3.a2, a2: Builtin_fst($35)}, a2: Builtin_snd($35)}};
      }
     }
    }
    default: {
     const $42 = Pacillus_Idris2LSP_Lex_getTokPosx27($0.a1.a2);
     return {h: 0 /* {TcDone:9} */, a1: {a1: {a1: $0.a1.a1.a3.a2, a2: Builtin_fst($42)}, a2: Builtin_snd($42)}};
    }
   }
  }
 }
}

/* Pacillus.Idris2LSP.Lex.getTokPos' : List (WithBounds SimpleExprToken) -> (List Int, List String) */
function Pacillus_Idris2LSP_Lex_getTokPosx27($0) {
 return __tailRec(x24tcOpt_9, {h: 1 /* {TcContinue9:1} */, a1: $0});
}

/* {$tcOpt:10} */
function x24tcOpt_10($0) {
 switch($0.a3.h) {
  case undefined: /* cons */ {
   switch($0.a2($0.a3.a1)) {
    case 1: return {h: 1 /* {TcContinue10:1} */, a1: {a1: $0.a1, a2: $0.a3.a1}, a2: $0.a2, a3: $0.a3.a2};
    case 0: return {h: 1 /* {TcContinue10:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3.a2};
   }
  }
  case 0: /* nil */ return {h: 0 /* {TcDone:10} */, a1: Prelude_Types_SnocList_x3cx3ex3e($0.a1, {h: 0})};
 }
}

/* Prelude.Types.List.filterAppend : SnocList a -> (a -> Bool) -> List a -> List a */
function Prelude_Types_List_filterAppend($0, $1, $2) {
 return __tailRec(x24tcOpt_10, {h: 1 /* {TcContinue10:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:11} */
function x24tcOpt_11($0) {
 switch($0.a1.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:11} */, a1: $0.a2};
  case undefined: /* cons */ return {h: 1 /* {TcContinue11:1} */, a1: $0.a1.a1, a2: {a1: $0.a1.a2, a2: $0.a2}};
 }
}

/* Prelude.Types.SnocList.(<>>) : SnocList a -> List a -> List a */
function Prelude_Types_SnocList_x3cx3ex3e($0, $1) {
 return __tailRec(x24tcOpt_11, {h: 1 /* {TcContinue11:1} */, a1: $0, a2: $1});
}

/* {__mainExpression:0} */
const __mainExpression_0 = __lazy(function () {
 return PrimIO_unsafePerformIO($2 => Pacillus_Idris2LSP_Lex_main($2));
});

/* {csegen:80} */
const csegen_80 = __lazy(function () {
 return {a1: $1 => $2 => ($1+$2), a2: ''};
});

/* {csegen:97} */
const csegen_97 = __lazy(function () {
 return {a1: $1 => $2 => _add32s($1, $2), a2: $6 => $7 => _mul32s($6, $7), a3: $b => Number(_truncBigInt32($b))};
});

/* prim__sub_Integer : Integer -> Integer -> Integer */
function prim__sub_Integer($0, $1) {
 return ($0-$1);
}

/* Pacillus.Idris2LSP.Lex.output : Maybe (List Int, List String) -> JSON */
function Pacillus_Idris2LSP_Lex_output($0) {
 switch($0.h) {
  case 0: /* nothing */ return {h: 3 /* JString */, a1: '*Error : lex failed'};
  case undefined: /* just */ return {h: 5 /* JObject */, a1: {a1: {a1: 'pos', a2: {h: 4 /* JArray */, a1: Prelude_Types_List_mapAppend({h: 0}, $c => ({h: 3 /* JString */, a1: Prelude_Show_show_Show_Int($c)}), $0.a1.a1)}}, a2: {a1: {a1: 'syms', a2: {h: 4 /* JArray */, a1: Prelude_Types_List_mapAppend({h: 0}, $19 => ({h: 3 /* JString */, a1: $19}), $0.a1.a2)}}, a2: {h: 0}}}};
 }
}

/* Pacillus.Idris2LSP.Lex.main : IO () */
function Pacillus_Idris2LSP_Lex_main($0) {
 const $11 = b => a => $12 => $13 => $14 => {
  const $15 = $12($14);
  const $18 = $13($14);
  return $15($18);
 };
 const $6 = {a1: b => a => func => $8 => $9 => Prelude_IO_map_Functor_IO(func, $8, $9), a2: a => $f => $10 => $f, a3: $11};
 const $1d = b => a => $1e => $1f => $20 => {
  const $21 = $1e($20);
  return $1f($21)($20);
 };
 const $28 = a => $29 => $2a => {
  const $2b = $29($2a);
  return $2b($2a);
 };
 const $5 = {a1: $6, a2: $1d, a3: $28};
 const $4 = {a1: $5, a2: a => $31 => $31};
 const $2 = System_getArgs($4);
 const $1 = $2($0);
 switch($1.h) {
  case 0: /* nil */ return Prelude_IO_prim__putStr('*Unknown Error : Something went wrong with arguments\n', $0);
  case undefined: /* cons */ return Prelude_IO_prim__putStr((Pacillus_Idris2LSP_Lex_lexAndOutput(Data_String_unwords($1.a2))+'\n'), $0);
 }
}

/* Pacillus.Idris2LSP.Lex.lexAndOutput : String -> String */
function Pacillus_Idris2LSP_Lex_lexAndOutput($0) {
 return Language_JSON_Data_show_Show_JSON(Pacillus_Idris2LSP_Lex_output(Prelude_Types_map_Functor_Maybe($7 => Pacillus_Idris2LSP_Lex_getTokPosx27($7), Pacillus_Idris2LSP_Syntax_Lexer_lexSimpleExpr($0))));
}
exports.Pacillus_Idris2LSP_Lex_lexAndOutput = Pacillus_Idris2LSP_Lex_lexAndOutput;

/* Pacillus.Idris2LSP.Syntax.Lexer.symbolLexer : Lexer */
const Pacillus_Idris2LSP_Syntax_Lexer_symbolLexer = __lazy(function () {
 return Text_Lexer_some(Text_Lexer_Core_pred($4 => Pacillus_Idris2LSP_Syntax_Lexer_isOpChar($4)));
});

/* Pacillus.Idris2LSP.Syntax.Lexer.simpleExprTokenMap : TokenMap SimpleExprToken */
const Pacillus_Idris2LSP_Syntax_Lexer_simpleExprTokenMap = __lazy(function () {
 const $19 = s => {
  const $1a = Data_List_lookup({a1: $1e => $1f => Prelude_EqOrd_x3dx3d_Eq_String($1e, $1f), a2: $24 => $25 => Prelude_EqOrd_x2fx3d_Eq_String($24, $25)}, s, Pacillus_Idris2LSP_Syntax_Lexer_reservedSyms());
  switch($1a.h) {
   case undefined: /* just */ return {a1: $1a.a1, a2: s};
   case 0: /* nothing */ return {a1: 0, a2: s};
  }
 };
 const $16 = {a1: Pacillus_Idris2LSP_Syntax_Lexer_symbolLexer(), a2: $19};
 const $15 = {a1: $16, a2: {h: 0}};
 const $13 = Prelude_Types_List_tailRecAppend($15, Text_Lexer_toTokenMap({a1: {a1: Text_Lexer_exact('('), a2: 2}, a2: {a1: {a1: Text_Lexer_exact(')'), a2: 3}, a2: {a1: {a1: Text_Lexer_exact('`'), a2: 5}, a2: {a1: {a1: Text_Lexer_exact(','), a2: 9}, a2: {a1: {a1: Text_Lexer_digits(), a2: 10}, a2: {a1: {a1: Pacillus_Idris2LSP_Syntax_Lexer_doubleLit(), a2: 11}, a2: {a1: {a1: Text_Lexer_stringLit(), a2: 12}, a2: {h: 0}}}}}}}}));
 const $9 = Prelude_Types_List_tailRecAppend(Text_Lexer_toTokenMap({a1: {a1: Pacillus_Idris2LSP_Syntax_Lexer_idLexer(), a2: 4}, a2: {h: 0}}), $13);
 return Prelude_Types_List_tailRecAppend(Text_Lexer_toTokenMap({a1: {a1: Text_Lexer_spaces(), a2: 1}, a2: {h: 0}}), $9);
});

/* Pacillus.Idris2LSP.Syntax.Lexer.reservedSyms : List (String, SimpleExprTokenKind) */
const Pacillus_Idris2LSP_Syntax_Lexer_reservedSyms = __lazy(function () {
 return {a1: {a1: '->', a2: 6}, a2: {a1: {a1: '=', a2: 7}, a2: {a1: {a1: ':', a2: 8}, a2: {h: 0}}}};
});

/* Pacillus.Idris2LSP.Syntax.Lexer.nameLexer : Lexer */
const Pacillus_Idris2LSP_Syntax_Lexer_nameLexer = __lazy(function () {
 return Text_Lexer_Core_x3cx7cx3e({h: 4 /* SeqEat */, a1: Text_Lexer_alpha(), a2: () => Text_Lexer_many(Text_Lexer_Core_x3cx7cx3e(Text_Lexer_alphaNum(), Text_Lexer_Core_x3cx7cx3e(Text_Lexer_is('_'), Text_Lexer_is('\''))))}, {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: Text_Lexer_is('('), a2: () => Text_Lexer_many(Text_Lexer_spaces())}, a2: () => Pacillus_Idris2LSP_Syntax_Lexer_symbolLexer()}, a2: () => Text_Lexer_many(Text_Lexer_spaces())}, a2: () => Text_Lexer_is(')')});
});

/* Pacillus.Idris2LSP.Syntax.Lexer.lexSimpleExpr : String -> Maybe (List (WithBounds SimpleExprToken)) */
function Pacillus_Idris2LSP_Syntax_Lexer_lexSimpleExpr($0) {
 const $1 = Text_Lexer_Core_lex(Pacillus_Idris2LSP_Syntax_Lexer_simpleExprTokenMap(), $0);
 switch($1.h) {
  case undefined: /* cons */ {
   switch($1.a2.h) {
    case undefined: /* cons */ {
     switch($1.a2.a2.h) {
      case undefined: /* cons */ {
       switch($1.a2.a2.a2) {
        case '': return {a1: $1.a1};
        default: return {h: 0};
       }
      }
      default: return {h: 0};
     }
    }
    default: return {h: 0};
   }
  }
  default: return {h: 0};
 }
}

/* Pacillus.Idris2LSP.Syntax.Lexer.isOpChar : Char -> Bool */
function Pacillus_Idris2LSP_Syntax_Lexer_isOpChar($0) {
 return Prelude_Types_elem({a1: acc => elem => func => init => input => Prelude_Types_foldr_Foldable_List(func, init, input), a2: elem => acc => func => init => input => Prelude_Types_foldl_Foldable_List(func, init, input), a3: elem => $e => Prelude_Types_null_Foldable_List($e), a4: elem => acc => m => $12 => funcM => init => input => Prelude_Types_foldlM_Foldable_List($12, funcM, init, input), a5: elem => $19 => $19, a6: a => m => $1b => f => $1c => Prelude_Types_foldMap_Foldable_List($1b, f, $1c)}, {a1: $23 => $24 => Prelude_EqOrd_x3dx3d_Eq_Char($23, $24), a2: $29 => $2a => Prelude_EqOrd_x2fx3d_Eq_Char($29, $2a)}, $0, Prelude_Types_fastUnpack(':!#$%&*+./<=>?@\u{5c}^|-~'));
}

/* Pacillus.Idris2LSP.Syntax.Lexer.idLexer : Lexer */
const Pacillus_Idris2LSP_Syntax_Lexer_idLexer = __lazy(function () {
 return {h: 5 /* SeqEmpty */, a1: Text_Lexer_many({h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: Text_Lexer_alpha(), a2: () => Text_Lexer_many(Text_Lexer_alphaNum())}, a2: () => Text_Lexer_is('.')}), a2: Pacillus_Idris2LSP_Syntax_Lexer_nameLexer()};
});

/* Pacillus.Idris2LSP.Syntax.Lexer.doubleLit : Lexer */
const Pacillus_Idris2LSP_Syntax_Lexer_doubleLit = __lazy(function () {
 return {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: Text_Lexer_digits(), a2: () => Text_Lexer_is('.')}, a2: () => Text_Lexer_digits()}, a2: () => Text_Lexer_opt({h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: Text_Lexer_is('e'), a2: () => Text_Lexer_opt(Text_Lexer_Core_x3cx7cx3e(Text_Lexer_is('-'), Text_Lexer_is('+')))}, a2: () => Text_Lexer_digits()})};
});

/* Data.String.unwords : List String -> String */
function Data_String_unwords($0) {
 return Data_String_joinBy(' ', $0);
}

/* Data.String.singleton : Char -> String */
function Data_String_singleton($0) {
 return ($0+'');
}

/* Data.String.replicate : Nat -> Char -> String */
function Data_String_replicate($0, $1) {
 return Prelude_Types_fastPack(Data_List_replicateTR({h: 0}, $0, $1));
}

/* Data.String.joinBy : String -> List String -> String */
function Data_String_joinBy($0, $1) {
 return Prelude_Types_foldMap_Foldable_List(csegen_80(), $6 => $6, Data_List_intersperse($0, $1));
}

/* Prelude.Basics.flip : (a -> b -> c) -> b -> a -> c */
function Prelude_Basics_flip($0, $1, $2) {
 return $0($2)($1);
}

/* Builtin.snd : (a, b) -> b */
function Builtin_snd($0) {
 return $0.a2;
}

/* Builtin.fst : (a, b) -> a */
function Builtin_fst($0) {
 return $0.a1;
}

/* Prelude.Types.traverse */
function Prelude_Types_traverse_Traversable_List($0, $1, $2) {
 switch($2.h) {
  case 0: /* nil */ return $0.a2(undefined)({h: 0});
  case undefined: /* cons */ return $0.a3(undefined)(undefined)($0.a3(undefined)(undefined)($0.a2(undefined)($1e => $1f => ({a1: $1e, a2: $1f})))($1($2.a1)))(Prelude_Types_traverse_Traversable_List($0, $1, $2.a2));
 }
}

/* Prelude.Types.rangeFromTo */
function Prelude_Types_rangeFromTo_Range_x24a($0, $1, $2) {
 const $4 = Builtin_fst(Builtin_snd($0));
 const $3 = $4.a2($1)($2);
 switch($3) {
  case 0: {
   const $e = $f => {
    const $10 = Builtin_fst(Builtin_snd($0));
    return $10.a6($f)($2);
   };
   const $1c = $1d => {
    const $1e = Builtin_snd(Builtin_snd($0));
    const $28 = Builtin_snd(Builtin_snd($0));
    const $27 = $28.a1.a3(1n);
    return $1e.a1.a1($1d)($27);
   };
   const $19 = Prelude_Types_countFrom($1, $1c);
   return Prelude_Types_takeUntil($e, $19);
  }
  case 1: return Prelude_Types_pure_Applicative_List($1);
  case 2: {
   const $33 = $34 => {
    const $35 = Builtin_fst(Builtin_snd($0));
    return $35.a5($34)($2);
   };
   const $41 = x => {
    const $42 = Builtin_snd(Builtin_snd($0));
    const $4b = Builtin_snd(Builtin_snd($0));
    const $4a = $4b.a1.a3(1n);
    return $42.a3(x)($4a);
   };
   const $3e = Prelude_Types_countFrom($1, $41);
   return Prelude_Types_takeUntil($33, $3e);
  }
 }
}

/* Prelude.Types.pure */
function Prelude_Types_pure_Applicative_List($0) {
 return {a1: $0, a2: {h: 0}};
}

/* Prelude.Types.null */
function Prelude_Types_null_Foldable_List($0) {
 switch($0.h) {
  case 0: /* nil */ return 1;
  case undefined: /* cons */ return 0;
 }
}

/* Prelude.Types.map */
function Prelude_Types_map_Functor_Maybe($0, $1) {
 switch($1.h) {
  case undefined: /* just */ return {a1: $0($1.a1)};
  case 0: /* nothing */ return {h: 0};
 }
}

/* Prelude.Types.foldr */
function Prelude_Types_foldr_Foldable_List($0, $1, $2) {
 switch($2.h) {
  case 0: /* nil */ return $1;
  case undefined: /* cons */ return $0($2.a1)(Prelude_Types_foldr_Foldable_List($0, $1, $2.a2));
 }
}

/* Prelude.Types.foldlM */
function Prelude_Types_foldlM_Foldable_List($0, $1, $2, $3) {
 return Prelude_Types_foldl_Foldable_List(ma => b => $0.a2(undefined)(undefined)(ma)($f => Prelude_Basics_flip($1, b, $f)), $0.a1.a2(undefined)($2), $3);
}

/* Prelude.Types.foldMap */
function Prelude_Types_foldMap_Foldable_List($0, $1, $2) {
 return Prelude_Types_foldl_Foldable_List(acc => elem => $0.a1(acc)($1(elem)), $0.a2, $2);
}

/* Prelude.Types.>>= */
function Prelude_Types_x3ex3ex3d_Monad_Maybe($0, $1) {
 switch($0.h) {
  case 0: /* nothing */ return {h: 0};
  case undefined: /* just */ return $1($0.a1);
 }
}

/* Prelude.Types.takeUntil : (n -> Bool) -> Stream n -> List n */
function Prelude_Types_takeUntil($0, $1) {
 switch($0($1.a1)) {
  case 1: return {a1: $1.a1, a2: {h: 0}};
  case 0: return {a1: $1.a1, a2: Prelude_Types_takeUntil($0, $1.a2())};
 }
}

/* Prelude.Types.List.tailRecAppend : List a -> List a -> List a */
function Prelude_Types_List_tailRecAppend($0, $1) {
 return Prelude_Types_List_reverseOnto($1, Prelude_Types_List_reverse($0));
}

/* Prelude.Types.List.reverse : List a -> List a */
function Prelude_Types_List_reverse($0) {
 return Prelude_Types_List_reverseOnto({h: 0}, $0);
}

/* Prelude.Types.prim__integerToNat : Integer -> Nat */
function Prelude_Types_prim__integerToNat($0) {
 switch(((0n<=$0)?1:0)) {
  case 0: return 0n;
  default: return $0;
 }
}

/* Prelude.Types.maybe : Lazy b -> Lazy (a -> b) -> Maybe a -> b */
function Prelude_Types_maybe($0, $1, $2) {
 switch($2.h) {
  case 0: /* nothing */ return $0();
  case undefined: /* just */ return $1()($2.a1);
 }
}

/* Prelude.Types.List.lengthTR : List a -> Nat */
function Prelude_Types_List_lengthTR($0) {
 return Prelude_Types_List_lengthPlus(0n, $0);
}

/* Prelude.Types.String.length : String -> Nat */
function Prelude_Types_String_length($0) {
 return Prelude_Types_prim__integerToNat(BigInt($0.length));
}

/* Prelude.Types.isUpper : Char -> Bool */
function Prelude_Types_isUpper($0) {
 switch(Prelude_EqOrd_x3ex3d_Ord_Char($0, 'A')) {
  case 1: return Prelude_EqOrd_x3cx3d_Ord_Char($0, 'Z');
  case 0: return 0;
 }
}

/* Prelude.Types.isSpace : Char -> Bool */
function Prelude_Types_isSpace($0) {
 switch($0) {
  case ' ': return 1;
  case '\u{9}': return 1;
  case '\r': return 1;
  case '\n': return 1;
  case '\u{c}': return 1;
  case '\u{b}': return 1;
  case '\u{a0}': return 1;
  default: return 0;
 }
}

/* Prelude.Types.isLower : Char -> Bool */
function Prelude_Types_isLower($0) {
 switch(Prelude_EqOrd_x3ex3d_Ord_Char($0, 'a')) {
  case 1: return Prelude_EqOrd_x3cx3d_Ord_Char($0, 'z');
  case 0: return 0;
 }
}

/* Prelude.Types.isDigit : Char -> Bool */
function Prelude_Types_isDigit($0) {
 switch(Prelude_EqOrd_x3ex3d_Ord_Char($0, '0')) {
  case 1: return Prelude_EqOrd_x3cx3d_Ord_Char($0, '9');
  case 0: return 0;
 }
}

/* Prelude.Types.isControl : Char -> Bool */
function Prelude_Types_isControl($0) {
 let $1;
 switch(Prelude_EqOrd_x3ex3d_Ord_Char($0, '\0')) {
  case 1: {
   $1 = Prelude_EqOrd_x3cx3d_Ord_Char($0, '\u{1f}');
   break;
  }
  case 0: {
   $1 = 0;
   break;
  }
 }
 switch($1) {
  case 1: return 1;
  case 0: {
   switch(Prelude_EqOrd_x3ex3d_Ord_Char($0, '\u{7f}')) {
    case 1: return Prelude_EqOrd_x3cx3d_Ord_Char($0, '\u{9f}');
    case 0: return 0;
   }
  }
 }
}

/* Prelude.Types.isAlphaNum : Char -> Bool */
function Prelude_Types_isAlphaNum($0) {
 switch(Prelude_Types_isDigit($0)) {
  case 1: return 1;
  case 0: return Prelude_Types_isAlpha($0);
 }
}

/* Prelude.Types.isAlpha : Char -> Bool */
function Prelude_Types_isAlpha($0) {
 switch(Prelude_Types_isUpper($0)) {
  case 1: return 1;
  case 0: return Prelude_Types_isLower($0);
 }
}

/* Prelude.Types.elemBy : Foldable t => (a -> a -> Bool) -> a -> t a -> Bool */
function Prelude_Types_elemBy($0, $1, $2, $3) {
 return $0.a6(undefined)(undefined)({a1: $e => $f => Prelude_Interfaces_Bool_Semigroup_x3cx2bx3e_Semigroup_AnyBool($e, $f), a2: 0})($1($2))($3);
}

/* Prelude.Types.elem : Foldable t => Eq a => a -> t a -> Bool */
function Prelude_Types_elem($0, $1, $2, $3) {
 return Prelude_Types_elemBy($0, $7 => $8 => $1.a1($7)($8), $2, $3);
}

/* Prelude.Types.countFrom : n -> (n -> n) -> Stream n */
function Prelude_Types_countFrom($0, $1) {
 return {a1: $0, a2: () => Prelude_Types_countFrom($1($0), $1)};
}

/* Prelude.Num.mod */
function Prelude_Num_mod_Integral_Int($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Int($1, Number(_truncBigInt32(0n)))) {
  case 0: return _mod($0, $1);
  default: return _crashExp('Unhandled input for Prelude.Num.case block in mod at Prelude.Num:131:3--133:40');
 }
}

/* Prelude.Num.div */
function Prelude_Num_div_Integral_Int($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Int($1, Number(_truncBigInt32(0n)))) {
  case 0: return _div32s($0, $1);
  default: return _crashExp('Unhandled input for Prelude.Num.case block in div at Prelude.Num:128:3--130:40');
 }
}

/* Prelude.EqOrd.min */
function Prelude_EqOrd_min_Ord_Int($0, $1) {
 switch(Prelude_EqOrd_x3c_Ord_Int($0, $1)) {
  case 1: return $0;
  case 0: return $1;
 }
}

/* Prelude.EqOrd.max */
function Prelude_EqOrd_max_Ord_Int($0, $1) {
 switch(Prelude_EqOrd_x3e_Ord_Int($0, $1)) {
  case 1: return $0;
  case 0: return $1;
 }
}

/* Prelude.EqOrd.compare */
function Prelude_EqOrd_compare_Ord_Integer($0, $1) {
 switch(Prelude_EqOrd_x3c_Ord_Integer($0, $1)) {
  case 1: return 0;
  case 0: {
   switch(Prelude_EqOrd_x3dx3d_Eq_Integer($0, $1)) {
    case 1: return 1;
    case 0: return 2;
   }
  }
 }
}

/* Prelude.EqOrd.compare */
function Prelude_EqOrd_compare_Ord_Int($0, $1) {
 switch(Prelude_EqOrd_x3c_Ord_Int($0, $1)) {
  case 1: return 0;
  case 0: {
   switch(Prelude_EqOrd_x3dx3d_Eq_Int($0, $1)) {
    case 1: return 1;
    case 0: return 2;
   }
  }
 }
}

/* Prelude.EqOrd.> */
function Prelude_EqOrd_x3e_Ord_Int($0, $1) {
 switch((($0>$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.>= */
function Prelude_EqOrd_x3ex3d_Ord_Int($0, $1) {
 switch((($0>=$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.>= */
function Prelude_EqOrd_x3ex3d_Ord_Char($0, $1) {
 switch((($0>=$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_String($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Ordering($0, $1) {
 switch($0) {
  case 0: {
   switch($1) {
    case 0: return 1;
    default: return 0;
   }
  }
  case 1: {
   switch($1) {
    case 1: return 1;
    default: return 0;
   }
  }
  case 2: {
   switch($1) {
    case 2: return 1;
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Integer($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Int($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Char($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Bool($0, $1) {
 switch($0) {
  case 1: {
   switch($1) {
    case 1: return 1;
    default: return 0;
   }
  }
  case 0: {
   switch($1) {
    case 0: return 1;
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Prelude.EqOrd.< */
function Prelude_EqOrd_x3c_Ord_Integer($0, $1) {
 switch((($0<$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.< */
function Prelude_EqOrd_x3c_Ord_Int($0, $1) {
 switch((($0<$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.<= */
function Prelude_EqOrd_x3cx3d_Ord_Int($0, $1) {
 switch((($0<=$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.<= */
function Prelude_EqOrd_x3cx3d_Ord_Char($0, $1) {
 switch((($0<=$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd./= */
function Prelude_EqOrd_x2fx3d_Eq_String($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_String($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Prelude.EqOrd./= */
function Prelude_EqOrd_x2fx3d_Eq_Ordering($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Ordering($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Prelude.EqOrd./= */
function Prelude_EqOrd_x2fx3d_Eq_Int($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Int($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Prelude.EqOrd./= */
function Prelude_EqOrd_x2fx3d_Eq_Char($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Char($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Prelude.EqOrd.compareInteger : Integer -> Integer -> Ordering */
function Prelude_EqOrd_compareInteger($0, $1) {
 return Prelude_EqOrd_compare_Ord_Integer($0, $1);
}

/* Prelude.Interfaces.Bool.Semigroup.<+> */
function Prelude_Interfaces_Bool_Semigroup_x3cx2bx3e_Semigroup_AnyBool($0, $1) {
 switch($0) {
  case 1: return 1;
  case 0: return $1;
 }
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_Int($0) {
 return Prelude_Show_showPrec_Show_Int({h: 0 /* Open */}, $0);
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_Double($0) {
 return Prelude_Show_showPrec_Show_Double({h: 0 /* Open */}, $0);
}

/* Prelude.Show.showPrec */
function Prelude_Show_showPrec_Show_Int($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

/* Prelude.Show.showPrec */
function Prelude_Show_showPrec_Show_Double($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

/* Prelude.Show.compare */
function Prelude_Show_compare_Ord_Prec($0, $1) {
 switch($0.h) {
  case 4: /* User */ {
   switch($1.h) {
    case 4: /* User */ return Prelude_EqOrd_compare_Ord_Integer($0.a1, $1.a1);
    default: return Prelude_EqOrd_compare_Ord_Integer(Prelude_Show_precCon($0), Prelude_Show_precCon($1));
   }
  }
  default: return Prelude_EqOrd_compare_Ord_Integer(Prelude_Show_precCon($0), Prelude_Show_precCon($1));
 }
}

/* Prelude.Show.>= */
function Prelude_Show_x3ex3d_Ord_Prec($0, $1) {
 return Prelude_EqOrd_x2fx3d_Eq_Ordering(Prelude_Show_compare_Ord_Prec($0, $1), 0);
}

/* Prelude.Show.showParens : Bool -> String -> String */
function Prelude_Show_showParens($0, $1) {
 switch($0) {
  case 0: return $1;
  case 1: return ('('+($1+')'));
 }
}

/* Prelude.Show.primNumShow : (a -> String) -> Prec -> a -> String */
function Prelude_Show_primNumShow($0, $1, $2) {
 const $3 = $0($2);
 let $7;
 switch(Prelude_Show_x3ex3d_Ord_Prec($1, {h: 5 /* PrefixMinus */})) {
  case 1: {
   $7 = Prelude_Show_firstCharIs($e => Prelude_EqOrd_x3dx3d_Eq_Char($e, '-'), $3);
   break;
  }
  case 0: {
   $7 = 0;
   break;
  }
 }
 return Prelude_Show_showParens($7, $3);
}

/* Prelude.Show.precCon : Prec -> Integer */
function Prelude_Show_precCon($0) {
 switch($0.h) {
  case 0: /* Open */ return 0n;
  case 1: /* Equal */ return 1n;
  case 2: /* Dollar */ return 2n;
  case 3: /* Backtick */ return 3n;
  case 4: /* User */ return 4n;
  case 5: /* PrefixMinus */ return 5n;
  case 6: /* App */ return 6n;
 }
}

/* Prelude.Show.firstCharIs : (Char -> Bool) -> String -> Bool */
function Prelude_Show_firstCharIs($0, $1) {
 switch($1) {
  case '': return 0;
  default: return $0(($1.charAt(0)));
 }
}

/* Prelude.IO.map */
function Prelude_IO_map_Functor_IO($0, $1, $2) {
 const $3 = $1($2);
 return $0($3);
}

/* PrimIO.unsafePerformIO : IO a -> a */
function PrimIO_unsafePerformIO($0) {
 const $2 = w => {
  const $3 = $0(w);
  return $3;
 };
 return PrimIO_unsafeCreateWorld($2);
}

/* PrimIO.unsafeCreateWorld : (1 _ : ((1 _ : %World) -> a)) -> a */
function PrimIO_unsafeCreateWorld($0) {
 return $0(_idrisworld);
}

/* Data.Maybe.isJust : Maybe a -> Bool */
function Data_Maybe_isJust($0) {
 switch($0.h) {
  case 0: /* nothing */ return 0;
  case undefined: /* just */ return 1;
 }
}

/* Data.List.span : (a -> Bool) -> List a -> (List a, List a) */
function Data_List_span($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return {a1: {h: 0}, a2: {h: 0}};
  case undefined: /* cons */ {
   switch($0($1.a1)) {
    case 1: {
     const $8 = Data_List_span($0, $1.a2);
     return {a1: {a1: $1.a1, a2: $8.a1}, a2: $8.a2};
    }
    case 0: return {a1: {h: 0}, a2: {a1: $1.a1, a2: $1.a2}};
   }
  }
 }
}

/* Data.List.mergeReplicate : a -> List a -> List a */
function Data_List_mergeReplicate($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return {h: 0};
  case undefined: /* cons */ return {a1: $0, a2: {a1: $1.a1, a2: Data_List_mergeReplicate($0, $1.a2)}};
 }
}

/* Data.List.lookup : Eq a => a -> List (a, b) -> Maybe b */
function Data_List_lookup($0, $1, $2) {
 return Data_List_lookupBy($5 => $6 => $0.a1($5)($6), $1, $2);
}

/* Data.List.isNil : List a -> Bool */
function Data_List_isNil($0) {
 switch($0.h) {
  case 0: /* nil */ return 1;
  default: return 0;
 }
}

/* Data.List.intersperse : a -> List a -> List a */
function Data_List_intersperse($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return {h: 0};
  case undefined: /* cons */ return {a1: $1.a1, a2: Data_List_mergeReplicate($0, $1.a2)};
 }
}

/* Text.Lexer.toTokenMap : List (Lexer, k) -> TokenMap (Token k) */
function Text_Lexer_toTokenMap($0) {
 return Prelude_Types_List_mapAppend({h: 0}, $4 => ({a1: $4.a1, a2: $8 => ({a1: $4.a2, a2: $8})}), $0);
}

/* Text.Lexer.surround : Lexer -> Lexer -> Lexer -> Lexer */
function Text_Lexer_surround($0, $1, $2) {
 return {h: 4 /* SeqEat */, a1: $0, a2: () => Text_Lexer_manyThen($1, $2)};
}

/* Text.Lexer.stringLit : Lexer */
const Text_Lexer_stringLit = __lazy(function () {
 return Text_Lexer_quote(Text_Lexer_is('\"'), Text_Lexer_Core_x3cx7cx3e(Text_Lexer_escape(Text_Lexer_is('\u{5c}'), Text_Lexer_any()), Text_Lexer_any()));
});

/* Text.Lexer.spaces : Lexer */
const Text_Lexer_spaces = __lazy(function () {
 return Text_Lexer_some(Text_Lexer_space());
});

/* Text.Lexer.space : Lexer */
const Text_Lexer_space = __lazy(function () {
 return Text_Lexer_Core_pred($2 => Prelude_Types_isSpace($2));
});

/* Text.Lexer.some : Lexer -> Lexer */
function Text_Lexer_some($0) {
 return {h: 4 /* SeqEat */, a1: $0, a2: () => Text_Lexer_many($0)};
}

/* Text.Lexer.quote : Lexer -> Lexer -> Lexer */
function Text_Lexer_quote($0, $1) {
 return Text_Lexer_surround($0, $0, $1);
}

/* Text.Lexer.opt : Lexer -> Recognise False */
function Text_Lexer_opt($0) {
 return Text_Lexer_Core_x3cx7cx3e($0, Text_Lexer_Core_empty());
}

/* Text.Lexer.manyUntil : Recognise c -> Lexer -> Recognise False */
function Text_Lexer_manyUntil($0, $1) {
 return Text_Lexer_many({h: 5 /* SeqEmpty */, a1: Text_Lexer_Core_reject($0), a2: $1});
}

/* Text.Lexer.manyThen : Recognise c -> Lexer -> Recognise c */
function Text_Lexer_manyThen($0, $1) {
 return {h: 5 /* SeqEmpty */, a1: Text_Lexer_manyUntil($0, $1), a2: $0};
}

/* Text.Lexer.many : Lexer -> Recognise False */
function Text_Lexer_many($0) {
 return Text_Lexer_opt(Text_Lexer_some($0));
}

/* Text.Lexer.is : Char -> Lexer */
function Text_Lexer_is($0) {
 return Text_Lexer_Core_pred($3 => Prelude_EqOrd_x3dx3d_Eq_Char($3, $0));
}

/* Text.Lexer.exact : String -> Lexer */
function Text_Lexer_exact($0) {
 const $1 = Prelude_Types_fastUnpack($0);
 switch($1.h) {
  case 0: /* nil */ return Text_Lexer_Core_fail();
  case undefined: /* cons */ return Text_Lexer_Core_concatMap($7 => Text_Lexer_is($7), {a1: $1.a1, a2: $1.a2});
 }
}

/* Text.Lexer.escape : Lexer -> Lexer -> Lexer */
function Text_Lexer_escape($0, $1) {
 return {h: 4 /* SeqEat */, a1: $0, a2: () => $1};
}

/* Text.Lexer.digits : Lexer */
const Text_Lexer_digits = __lazy(function () {
 return Text_Lexer_some(Text_Lexer_digit());
});

/* Text.Lexer.digit : Lexer */
const Text_Lexer_digit = __lazy(function () {
 return Text_Lexer_Core_pred($2 => Prelude_Types_isDigit($2));
});

/* Text.Lexer.any : Lexer */
const Text_Lexer_any = __lazy(function () {
 return Text_Lexer_Core_pred($2 => 1);
});

/* Text.Lexer.alphaNum : Lexer */
const Text_Lexer_alphaNum = __lazy(function () {
 return Text_Lexer_Core_pred($2 => Prelude_Types_isAlphaNum($2));
});

/* Text.Lexer.alpha : Lexer */
const Text_Lexer_alpha = __lazy(function () {
 return Text_Lexer_Core_pred($2 => Prelude_Types_isAlpha($2));
});

/* Text.Lexer.Core.3934:2500:getCols */
function Text_Lexer_Core_n__3934_2500_getCols($0, $1, $2, $3, $4, $5, $6, $7) {
 const $8 = Data_List_span($b => Prelude_EqOrd_x2fx3d_Eq_Char($b, '\n'), $6);
 switch($8.a2.h) {
  case 0: /* nil */ return _add32s($7, Number(_truncBigInt32(Prelude_Types_List_lengthTR($8.a1))));
  default: return Number(_truncBigInt32(Prelude_Types_List_lengthTR($8.a1)));
 }
}

/* Text.Lexer.Core.3934:2499:countNLs */
function Text_Lexer_Core_n__3934_2499_countNLs($0, $1, $2, $3, $4, $5, $6) {
 return Prelude_Types_List_lengthTR(Prelude_Types_List_filterAppend({h: 0}, $c => Prelude_EqOrd_x3dx3d_Eq_Char($c, '\n'), $6));
}

/* Text.Lexer.Core.scan : Recognise c -> List Char -> List Char -> Maybe (List Char, List Char) */
function Text_Lexer_Core_scan($0, $1, $2) {
 switch($0.h) {
  case 0: /* Empty */ return {a1: {a1: $1, a2: $2}};
  case 1: /* Fail */ return {h: 0};
  case 2: /* Lookahead */ {
   switch(Prelude_EqOrd_x3dx3d_Eq_Bool(Data_Maybe_isJust(Text_Lexer_Core_scan($0.a2, $1, $2)), $0.a1)) {
    case 1: return {a1: {a1: $1, a2: $2}};
    case 0: return {h: 0};
   }
  }
  case 3: /* Pred */ {
   switch($2.h) {
    case 0: /* nil */ return {h: 0};
    case undefined: /* cons */ {
     switch($0.a1($2.a1)) {
      case 1: return {a1: {a1: {a1: $2.a1, a2: $1}, a2: $2.a2}};
      case 0: return {h: 0};
     }
    }
   }
  }
  case 4: /* SeqEat */ return Prelude_Types_x3ex3ex3d_Monad_Maybe(Text_Lexer_Core_scan($0.a1, $1, $2), $24 => Text_Lexer_Core_scan($0.a2(), $24.a1, $24.a2));
  case 5: /* SeqEmpty */ return Prelude_Types_x3ex3ex3d_Monad_Maybe(Text_Lexer_Core_scan($0.a1, $1, $2), $32 => Text_Lexer_Core_scan($0.a2, $32.a1, $32.a2));
  case 6: /* SeqSame */ return Prelude_Types_x3ex3ex3d_Monad_Maybe(Text_Lexer_Core_scan($0.a1, $1, $2), $3f => Text_Lexer_Core_scan($0.a2, $3f.a1, $3f.a2));
  case 7: /* Alt */ return Prelude_Types_maybe(() => Text_Lexer_Core_scan($0.a2, $1, $2), () => $4c => ({a1: $4c}), Text_Lexer_Core_scan($0.a1, $1, $2));
 }
}

/* Text.Lexer.Core.reject : Recognise c -> Recognise False */
function Text_Lexer_Core_reject($0) {
 return {h: 2 /* Lookahead */, a1: 0, a2: $0};
}

/* Text.Lexer.Core.pred : (Char -> Bool) -> Lexer */
function Text_Lexer_Core_pred($0) {
 return {h: 3 /* Pred */, a1: $0};
}

/* Text.Lexer.Core.lex : TokenMap a -> String -> (List (WithBounds a), (Int, (Int, String))) */
function Text_Lexer_Core_lex($0, $1) {
 const $2 = Text_Lexer_Core_tokenise($5 => 0, 0, 0, {h: 0}, $0, Prelude_Types_fastUnpack($1));
 return {a1: $2.a1, a2: {a1: $2.a2.a1, a2: {a1: $2.a2.a2.a1, a2: Prelude_Types_fastPack($2.a2.a2.a2)}}};
}

/* Text.Lexer.Core.fail : Recognise c */
const Text_Lexer_Core_fail = __lazy(function () {
 return {h: 1 /* Fail */};
});

/* Text.Lexer.Core.empty : Recognise False */
const Text_Lexer_Core_empty = __lazy(function () {
 return {h: 0 /* Empty */};
});

/* Text.Lexer.Core.concatMap : (a -> Recognise c) -> (xs : List a) -> Recognise (isCons xs && Delay c) */
function Text_Lexer_Core_concatMap($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return {h: 0 /* Empty */};
  case undefined: /* cons */ {
   switch($1.a2.h) {
    case 0: /* nil */ return $0($1.a1);
    case undefined: /* cons */ return {h: 6 /* SeqSame */, a1: $0($1.a1), a2: Text_Lexer_Core_concatMap($0, $1.a2)};
   }
  }
 }
}

/* Text.Lexer.Core.(<|>) : Recognise c1 -> Recognise c2 -> Recognise (c1 && Delay c2) */
function Text_Lexer_Core_x3cx7cx3e($0, $1) {
 return {h: 7 /* Alt */, a1: $0, a2: $1};
}

/* System.getArgs : HasIO io => io (List String) */
function System_getArgs($0) {
 const $12 = n => {
  switch(Prelude_EqOrd_x3e_Ord_Int(n, Number(_truncBigInt32(0n)))) {
   case 1: return Prelude_Basics_flip($1a => $1b => Prelude_Types_traverse_Traversable_List($0.a1.a1, $1a, $1b), Prelude_Types_rangeFromTo_Range_x24a({a1: {a1: csegen_97(), a2: $29 => $2a => Prelude_Num_div_Integral_Int($29, $2a), a3: $2f => $30 => Prelude_Num_mod_Integral_Int($2f, $30)}, a2: {a1: {a1: {a1: $38 => $39 => Prelude_EqOrd_x3dx3d_Eq_Int($38, $39), a2: $3e => $3f => Prelude_EqOrd_x2fx3d_Eq_Int($3e, $3f)}, a2: $44 => $45 => Prelude_EqOrd_compare_Ord_Int($44, $45), a3: $4a => $4b => Prelude_EqOrd_x3c_Ord_Int($4a, $4b), a4: $50 => $51 => Prelude_EqOrd_x3e_Ord_Int($50, $51), a5: $56 => $57 => Prelude_EqOrd_x3cx3d_Ord_Int($56, $57), a6: $5c => $5d => Prelude_EqOrd_x3ex3d_Ord_Int($5c, $5d), a7: $62 => $63 => Prelude_EqOrd_max_Ord_Int($62, $63), a8: $68 => $69 => Prelude_EqOrd_min_Ord_Int($68, $69)}, a2: {a1: csegen_97(), a2: $71 => _sub32s(0, $71), a3: $75 => $76 => _sub32s($75, $76)}}}, 0, _sub32s(n, 1)), $7e => $0.a2(undefined)($84 => System_prim__getArg($7e, $84)));
   case 0: return $0.a1.a1.a2(undefined)({h: 0});
  }
 };
 return $0.a1.a2(undefined)(undefined)($0.a2(undefined)($f => System_prim__getArgCount($f)))($12);
}

/* Language.JSON.Data.5181:9463:stringifyValues */
function Language_JSON_Data_n__5181_9463_stringifyValues($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return '';
  case undefined: /* cons */ {
   let $6;
   switch(Data_List_isNil($1.a2)) {
    case 1: {
     $6 = '';
     break;
    }
    case 0: {
     $6 = (','+Language_JSON_Data_n__5181_9463_stringifyValues($0, $1.a2));
     break;
    }
   }
   return (Language_JSON_Data_stringify($1.a1)+$6);
  }
 }
}

/* Language.JSON.Data.5181:9508:stringifyProps */
function Language_JSON_Data_n__5181_9508_stringifyProps($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return '';
  case undefined: /* cons */ {
   let $7;
   switch(Data_List_isNil($1.a2)) {
    case 1: {
     $7 = '';
     break;
    }
    case 0: {
     $7 = (','+Language_JSON_Data_n__5181_9508_stringifyProps($0, $1.a2));
     break;
    }
   }
   return (Language_JSON_Data_n__5181_9507_stringifyProp($0, $1.a1)+$7);
  }
 }
}

/* Language.JSON.Data.5181:9507:stringifyProp */
function Language_JSON_Data_n__5181_9507_stringifyProp($0, $1) {
 return (Language_JSON_Data_showString($1.a1)+(':'+Language_JSON_Data_stringify($1.a2)));
}

/* Language.JSON.Data.show */
function Language_JSON_Data_show_Show_JSON($0) {
 return Language_JSON_Data_stringify($0);
}

/* Language.JSON.Data.stringify : JSON -> String */
function Language_JSON_Data_stringify($0) {
 switch($0.h) {
  case 0: /* JNull */ return 'null';
  case 1: /* JBoolean */ {
   switch($0.a1) {
    case 1: return 'true';
    case 0: return 'false';
   }
  }
  case 2: /* JNumber */ return Prelude_Show_show_Show_Double($0.a1);
  case 3: /* JString */ return Language_JSON_Data_showString($0.a1);
  case 4: /* JArray */ return ('['+(Language_JSON_Data_n__5181_9463_stringifyValues($0.a1, $0.a1)+']'));
  case 5: /* JObject */ return ('{'+(Language_JSON_Data_n__5181_9508_stringifyProps($0.a1, $0.a1)+'}'));
 }
}

/* Language.JSON.Data.showString : String -> String */
function Language_JSON_Data_showString($0) {
 return ('\"'+(Prelude_Types_foldMap_Foldable_List(csegen_80(), $8 => Language_JSON_Data_showChar($8), Prelude_Types_fastUnpack($0))+'\"'));
}

/* Language.JSON.Data.showChar : Char -> String */
function Language_JSON_Data_showChar($0) {
 switch($0) {
  case '\u{8}': return '\u{5c}b';
  case '\u{c}': return '\u{5c}f';
  case '\n': return '\u{5c}n';
  case '\r': return '\u{5c}r';
  case '\u{9}': return '\u{5c}t';
  case '\u{5c}': return '\u{5c}\u{5c}';
  case '\"': return '\u{5c}\"';
  default: {
   let $2;
   switch(Prelude_Types_isControl($0)) {
    case 1: {
     $2 = 1;
     break;
    }
    case 0: {
     $2 = Prelude_EqOrd_x3ex3d_Ord_Char($0, '\u{7f}');
     break;
    }
   }
   switch($2) {
    case 1: {
     const $9 = Language_JSON_Data_b16ToHexString(_truncUInt16(_truncInt32($0.codePointAt(0))));
     return ('\u{5c}u'+Data_String_Extra_justifyRight(4n, '0', $9));
    }
    case 0: return Data_String_singleton($0);
   }
  }
 }
}

/* Language.JSON.Data.b16ToHexString : Bits16 -> String */
function Language_JSON_Data_b16ToHexString($0) {
 switch($0) {
  case 0: return '0';
  case 1: return '1';
  case 2: return '2';
  case 3: return '3';
  case 4: return '4';
  case 5: return '5';
  case 6: return '6';
  case 7: return '7';
  case 8: return '8';
  case 9: return '9';
  case 10: return 'A';
  case 11: return 'B';
  case 12: return 'C';
  case 13: return 'D';
  case 14: return 'E';
  case 15: return 'F';
  default: return (Language_JSON_Data_b16ToHexString(_shr16u($0, 4))+Language_JSON_Data_b16ToHexString(($0&15)));
 }
}

/* Data.String.Extra.justifyRight : Nat -> Char -> String -> String */
function Data_String_Extra_justifyRight($0, $1, $2) {
 return (Data_String_replicate(Prelude_Types_prim__integerToNat(($0-Prelude_Types_String_length($2))), $1)+$2);
}


try{__mainExpression_0()}catch(e){if(e instanceof IdrisError){console.log('ERROR: ' + e.message)}else{throw e} }
