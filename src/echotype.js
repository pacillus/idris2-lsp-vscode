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
const Prelude_Types_fastConcat = ((xs)=>__prim_idris2js_array(xs).join(''));
const Prelude_IO_prim__putStr = (x=>process.stdout.write(x));
const System_prim__getArgCount = (() => process.argv.length - 1);
const System_prim__getArg = (n => process.argv[n + 1]);
/* {$tcOpt:1} */
function x24tcOpt_1($0) {
 switch($0.a3.h) {
  case undefined: /* cons */ {
   switch($0.a3.a2.h) {
    case undefined: /* cons */ {
     switch($0.a4.h) {
      case undefined: /* cons */ return {h: 1 /* {TcContinue1:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3.a2.a2, a4: $0.a4.a2, a5: $a => $0.a5({a1: $0.a4.a1, a2: $a})};
      default: return {h: 0 /* {TcDone:1} */, a1: {a1: $0.a4, a2: $0.a5({h: 0})}};
     }
    }
    default: return {h: 0 /* {TcDone:1} */, a1: {a1: $0.a4, a2: $0.a5({h: 0})}};
   }
  }
  default: return {h: 0 /* {TcDone:1} */, a1: {a1: $0.a4, a2: $0.a5({h: 0})}};
 }
}

/* Data.List.7824:8306:splitRec */
function Data_List_n__7824_8306_splitRec($0, $1, $2, $3, $4) {
 return __tailRec(x24tcOpt_1, {h: 1 /* {TcContinue1:1} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4});
}

/* {$tcOpt:2} */
function x24tcOpt_2($0) {
 switch($0.a4.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:2} */, a1: $0.a3};
  case undefined: /* cons */ {
   switch($0.a4.a2.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:2} */, a1: ($0.a3+$0.a1.a1($0.a4.a1))};
    default: return {h: 1 /* {TcContinue2:1} */, a1: $0.a1, a2: $0.a2, a3: ($0.a3+($0.a1.a1($0.a4.a1)+', ')), a4: $0.a4.a2};
   }
  }
 }
}

/* Prelude.Show.3219:12514:show' */
function Prelude_Show_n__3219_12514_showx27($0, $1, $2, $3) {
 return __tailRec(x24tcOpt_2, {h: 1 /* {TcContinue2:1} */, a1: $0, a2: $1, a3: $2, a4: $3});
}

/* {$tcOpt:3} */
function x24tcOpt_3($0) {
 switch($0.h) {
  case 1: /* {TcContinue3:1} */ {
   switch($0.a7.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:3} */, a1: {h: 0}};
    case undefined: /* cons */ return {h: 2 /* {TcContinue3:2} */, a1: $0.a1, a2: $0.a2, a3: $0.a3, a4: $0.a4, a5: $0.a5, a6: $0.a6, a7: $0.a7.a1.a1, a8: $0.a7.a1.a2, a9: $0.a7.a2, a10: $0.a8, a11: Text_Lexer_Core_scan($0.a7.a1.a1, {h: 0}, $0.a8)};
   }
  }
  case 2: /* {TcContinue3:2} */ {
   switch($0.a11.h) {
    case undefined: /* just */ {
     const $16 = _add32s($0.a5, Number(_truncBigInt32(Text_Lexer_Core_n__3934_2499_countNLs($0.a1, $0.a2, $0.a3, $0.a4, $0.a5, $0.a6, $0.a11.a1.a1))));
     const $22 = Text_Lexer_Core_n__3934_2500_getCols($0.a1, $0.a2, $0.a3, $0.a4, $0.a5, $0.a6, $0.a11.a1.a1, $0.a4);
     return {h: 0 /* {TcDone:3} */, a1: {a1: {a1: {a1: $0.a8(Prelude_Types_fastPack(Prelude_Types_List_reverse($0.a11.a1.a1))), a2: 0, a3: {a1: $0.a5, a2: $0.a4, a3: $16, a4: $22}}, a2: {a1: $16, a2: {a1: $22, a2: $0.a11.a1.a2}}}}};
    }
    case 0: /* nothing */ return {h: 1 /* {TcContinue3:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3, a4: $0.a4, a5: $0.a5, a6: $0.a6, a7: $0.a9, a8: $0.a10};
   }
  }
 }
}

/* Text.Lexer.Core.3934:2501:getFirstToken */
function Text_Lexer_Core_n__3934_2501_getFirstToken($0, $1, $2, $3, $4, $5, $6, $7) {
 return __tailRec(x24tcOpt_3, {h: 1 /* {TcContinue3:1} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5, a7: $6, a8: $7});
}

/* Text.Lexer.Core.case block in tokenise,getFirstToken */
function Text_Lexer_Core_case__tokenisex2cgetFirstToken_2634($0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a) {
 return __tailRec(x24tcOpt_3, {h: 2 /* {TcContinue3:2} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5, a7: $6, a8: $7, a9: $8, a10: $9, a11: $a});
}

/* {$tcOpt:4} */
function x24tcOpt_4($0) {
 switch($0.h) {
  case 1: /* {TcContinue4:1} */ {
   switch($0.a1.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:4} */, a1: {h: 1 /* Right */, a1: {h: 0}}};
    case undefined: /* cons */ {
     switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_exprEquality($0.a1.a1.a1, $0.a1.a1.a2)) {
      case 1: return {h: 1 /* {TcContinue4:1} */, a1: $0.a1.a2};
      case 0: return {h: 2 /* {TcContinue4:2} */, a1: $0.a1.a2, a2: $0.a1.a1.a1, a3: $0.a1.a1.a2};
     }
    }
   }
  }
  case 2: /* {TcContinue4:2} */ {
   switch($0.a2.h) {
    case 0: /* IdTerm */ {
     switch($0.a3.h) {
      case 0: /* IdTerm */ {
       switch(Pacillus_Idris2LSP_TypeRecons_TypeRecons_isImplicitVar($0.a2.a1)) {
        case 1: return {h: 0 /* {TcDone:4} */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_uniVarExprL($0.a1, {h: 0 /* IdTerm */, a1: $0.a2.a1}, {h: 0 /* IdTerm */, a1: $0.a3.a1})};
        case 0: {
         switch(Pacillus_Idris2LSP_TypeRecons_TypeRecons_isImplicitVar($0.a3.a1)) {
          case 0: {
           switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_x3dx3d_Eq_Identifier($0.a2.a1, $0.a3.a1)) {
            case 1: return {h: 1 /* {TcContinue4:1} */, a1: $0.a1};
            case 0: return {h: 0 /* {TcDone:4} */, a1: {h: 0 /* Left */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_showUniError({h: 0 /* IdTerm */, a1: $0.a2.a1}, {h: 0 /* IdTerm */, a1: $0.a3.a1})}};
           }
          }
          case 1: return {h: 0 /* {TcDone:4} */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_uniVarExprR($0.a1, {h: 0 /* IdTerm */, a1: $0.a2.a1}, {h: 0 /* IdTerm */, a1: $0.a3.a1})};
         }
        }
       }
      }
      default: {
       switch(Pacillus_Idris2LSP_TypeRecons_TypeRecons_isImplicitVar($0.a2.a1)) {
        case 1: return {h: 0 /* {TcDone:4} */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_uniVarExprL($0.a1, {h: 0 /* IdTerm */, a1: $0.a2.a1}, $0.a3)};
        case 0: return {h: 0 /* {TcDone:4} */, a1: {h: 0 /* Left */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_showUniError({h: 0 /* IdTerm */, a1: $0.a2.a1}, $0.a3)}};
       }
      }
     }
    }
    default: {
     switch($0.a3.h) {
      case 0: /* IdTerm */ {
       switch(Pacillus_Idris2LSP_TypeRecons_TypeRecons_isImplicitVar($0.a3.a1)) {
        case 1: return {h: 0 /* {TcDone:4} */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_uniVarExprR($0.a1, $0.a2, {h: 0 /* IdTerm */, a1: $0.a3.a1})};
        case 0: return {h: 0 /* {TcDone:4} */, a1: {h: 0 /* Left */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_showUniError($0.a2, {h: 0 /* IdTerm */, a1: $0.a3.a1})}};
       }
      }
      default: {
       switch($0.a2.h) {
        case 1: /* AppTerm */ {
         switch($0.a3.h) {
          case 1: /* AppTerm */ return {h: 3 /* {TcContinue4:3} */, a1: $0.a1, a2: $0.a2.a1, a3: $0.a3.a1};
          default: return {h: 0 /* {TcDone:4} */, a1: {h: 0 /* Left */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_showUniError($0.a2, $0.a3)}};
         }
        }
        case 2: /* ArwTerm */ {
         switch($0.a3.h) {
          case 2: /* ArwTerm */ return {h: 4 /* {TcContinue4:4} */, a1: $0.a1, a2: $0.a2.a1, a3: $0.a3.a1};
          default: return {h: 0 /* {TcDone:4} */, a1: {h: 0 /* Left */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_showUniError($0.a2, $0.a3)}};
         }
        }
        case 3: /* EqTerm */ {
         switch($0.a3.h) {
          case 3: /* EqTerm */ return {h: 5 /* {TcContinue4:5} */, a1: $0.a1, a2: $0.a2.a1, a3: $0.a3.a1};
          default: return {h: 0 /* {TcDone:4} */, a1: {h: 0 /* Left */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_showUniError($0.a2, $0.a3)}};
         }
        }
        case 8: /* PrTerm */ {
         switch($0.a3.h) {
          case 8: /* PrTerm */ return {h: 6 /* {TcContinue4:6} */, a1: $0.a1, a2: $0.a2.a1, a3: $0.a3.a1};
          default: return {h: 0 /* {TcDone:4} */, a1: {h: 0 /* Left */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_showUniError($0.a2, $0.a3)}};
         }
        }
        default: return {h: 0 /* {TcDone:4} */, a1: {h: 0 /* Left */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_showUniError($0.a2, $0.a3)}};
       }
      }
     }
    }
   }
  }
  case 3: /* {TcContinue4:3} */ return {h: 1 /* {TcContinue4:1} */, a1: {a1: {a1: $0.a2.a1, a2: $0.a3.a1}, a2: {a1: {a1: $0.a2.a2, a2: $0.a3.a2}, a2: $0.a1}}};
  case 4: /* {TcContinue4:4} */ {
   switch($0.a2.h) {
    case 0: /* ExExArr */ {
     switch($0.a3.h) {
      case 0: /* ExExArr */ return {h: 1 /* {TcContinue4:1} */, a1: {a1: {a1: $0.a2.a2, a2: $0.a3.a2}, a2: {a1: {a1: $0.a2.a3, a2: $0.a3.a3}, a2: $0.a1}}};
      case 1: /* SiExArr */ return {h: 1 /* {TcContinue4:1} */, a1: {a1: {a1: $0.a2.a2, a2: $0.a3.a1.a2}, a2: {a1: {a1: $0.a2.a3, a2: $0.a3.a2}, a2: $0.a1}}};
     }
    }
    case 1: /* SiExArr */ {
     switch($0.a3.h) {
      case 0: /* ExExArr */ return {h: 1 /* {TcContinue4:1} */, a1: {a1: {a1: $0.a2.a1.a2, a2: $0.a3.a2}, a2: {a1: {a1: $0.a2.a2, a2: $0.a3.a3}, a2: $0.a1}}};
      case 1: /* SiExArr */ return {h: 1 /* {TcContinue4:1} */, a1: {a1: {a1: $0.a2.a1.a2, a2: $0.a3.a1.a2}, a2: {a1: {a1: $0.a2.a2, a2: $0.a3.a2}, a2: $0.a1}}};
     }
    }
   }
  }
  case 5: /* {TcContinue4:5} */ return {h: 1 /* {TcContinue4:1} */, a1: {a1: {a1: $0.a2.a1, a2: $0.a3.a1}, a2: {a1: {a1: $0.a2.a2, a2: $0.a3.a2}, a2: $0.a1}}};
  case 6: /* {TcContinue4:6} */ return {h: 1 /* {TcContinue4:1} */, a1: {a1: {a1: $0.a2.a1, a2: $0.a3.a1}, a2: {a1: {a1: $0.a2.a2, a2: $0.a3.a2}, a2: $0.a1}}};
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.unify : Constraints -> Either String Constraints */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_unify($0) {
 return __tailRec(x24tcOpt_4, {h: 1 /* {TcContinue4:1} */, a1: $0});
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.unifyExpr : Constraints -> SimpleExpr -> SimpleExpr -> Either String Constraints */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_unifyExpr($0, $1, $2) {
 return __tailRec(x24tcOpt_4, {h: 2 /* {TcContinue4:2} */, a1: $0, a2: $1, a3: $2});
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.unifyApp : Constraints -> Application -> Application -> Either String Constraints */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_unifyApp($0, $1, $2) {
 return __tailRec(x24tcOpt_4, {h: 3 /* {TcContinue4:3} */, a1: $0, a2: $1, a3: $2});
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.unifyArw : Constraints -> Arrow False -> Arrow False -> Either String Constraints */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_unifyArw($0, $1, $2) {
 return __tailRec(x24tcOpt_4, {h: 4 /* {TcContinue4:4} */, a1: $0, a2: $1, a3: $2});
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.unifyEq : Constraints -> Equality -> Equality -> Either String Constraints */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_unifyEq($0, $1, $2) {
 return __tailRec(x24tcOpt_4, {h: 5 /* {TcContinue4:5} */, a1: $0, a2: $1, a3: $2});
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.unifyPr : Constraints -> Pair -> Pair -> Either String Constraints */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_unifyPr($0, $1, $2) {
 return __tailRec(x24tcOpt_4, {h: 6 /* {TcContinue4:6} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:5} */
function x24tcOpt_5($0) {
 switch($0.a2.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:5} */, a1: _truncToChar($0.a3)};
  case undefined: /* cons */ return {h: 1 /* {TcContinue5:1} */, a1: $0.a1, a2: $0.a2.a2, a3: _add32s(Language_JSON_String_Tokens_n__3455_1167_hexVal($0.a1, $0.a2.a1), _mul32s(Number(_truncBigInt32(16n)), $0.a3))};
 }
}

/* Language.JSON.String.Tokens.3455:1166:fromHex */
function Language_JSON_String_Tokens_n__3455_1166_fromHex($0, $1, $2) {
 return __tailRec(x24tcOpt_5, {h: 1 /* {TcContinue5:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:6} */
function x24tcOpt_6($0) {
 switch($0.h) {
  case 1: /* {TcContinue6:1} */ return {h: 2 /* {TcContinue6:2} */, a1: $0.a6, a2: $0.a5, a3: $0.a4, a4: $0.a3, a5: $0.a2, a6: $0.a1, a7: Text_Lexer_Core_n__3934_2501_getFirstToken($0.a6, $0.a5, $0.a4, $0.a3, $0.a2, $0.a1, $0.a5, $0.a6)};
  case 2: /* {TcContinue6:2} */ {
   switch($0.a7.h) {
    case undefined: /* just */ {
     switch($0.a6($0.a7.a1.a1.a1)) {
      case 1: return {h: 0 /* {TcDone:6} */, a1: {a1: Prelude_Types_List_reverse($0.a3), a2: {a1: $0.a5, a2: {a1: $0.a4, a2: {h: 0}}}}};
      case 0: return {h: 1 /* {TcContinue6:1} */, a1: $0.a6, a2: $0.a7.a1.a2.a1, a3: $0.a7.a1.a2.a2.a1, a4: {a1: $0.a7.a1.a1, a2: $0.a3}, a5: $0.a2, a6: $0.a7.a1.a2.a2.a2};
     }
    }
    case 0: /* nothing */ return {h: 0 /* {TcDone:6} */, a1: {a1: Prelude_Types_List_reverse($0.a3), a2: {a1: $0.a5, a2: {a1: $0.a4, a2: $0.a1}}}};
   }
  }
 }
}

/* Text.Lexer.Core.tokenise : (a -> Bool) -> Int -> Int -> List (WithBounds a) -> TokenMap a -> List Char -> (List (WithBounds a),
(Int, (Int, List Char))) */
function Text_Lexer_Core_tokenise($0, $1, $2, $3, $4, $5) {
 return __tailRec(x24tcOpt_6, {h: 1 /* {TcContinue6:1} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5});
}

/* Text.Lexer.Core.case block in tokenise */
function Text_Lexer_Core_case__tokenise_2724($0, $1, $2, $3, $4, $5, $6) {
 return __tailRec(x24tcOpt_6, {h: 2 /* {TcContinue6:2} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5, a7: $6});
}

/* {$tcOpt:7} */
function x24tcOpt_7($0) {
 switch($0.a2.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:7} */, a1: $0.a1};
  case undefined: /* cons */ return {h: 1 /* {TcContinue7:1} */, a1: {a1: $0.a2.a1, a2: $0.a1}, a2: $0.a2.a2};
 }
}

/* Prelude.Types.List.reverseOnto : List a -> List a -> List a */
function Prelude_Types_List_reverseOnto($0, $1) {
 return __tailRec(x24tcOpt_7, {h: 1 /* {TcContinue7:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:8} */
function x24tcOpt_8($0) {
 switch($0.a1.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:8} */, a1: {a1: $0.a2}};
  case undefined: /* cons */ {
   let $5;
   switch(Prelude_EqOrd_x3ex3d_Ord_Char($0.a1.a1, '0')) {
    case 1: {
     $5 = Prelude_EqOrd_x3cx3d_Ord_Char($0.a1.a1, '9');
     break;
    }
    case 0: {
     $5 = 0;
     break;
    }
   }
   switch($5) {
    case 1: return {h: 1 /* {TcContinue8:1} */, a1: $0.a1.a2, a2: (($0.a2*10n)+BigInt(_sub32s(_truncInt32($0.a1.a1.codePointAt(0)), _truncInt32('0'.codePointAt(0)))))};
    case 0: return {h: 0 /* {TcDone:8} */, a1: {h: 0}};
   }
  }
 }
}

/* Data.String.parseNumWithoutSign : List Char -> Integer -> Maybe Integer */
function Data_String_parseNumWithoutSign($0, $1) {
 return __tailRec(x24tcOpt_8, {h: 1 /* {TcContinue8:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:9} */
function x24tcOpt_9($0) {
 switch($0.a1) {
  case '': {
   switch($0.a2.h) {
    case 0: /* Nil */ return {h: 0 /* {TcDone:9} */, a1: ''};
    default: {
     const $6 = ($0.a2.a1+$0.a2.a2);
     switch(Prelude_Types_isSpace($0.a2.a1)) {
      case 1: return {h: 1 /* {TcContinue9:1} */, a1: $0.a2.a2, a2: $0.a2.a3()};
      case 0: return {h: 0 /* {TcDone:9} */, a1: $6};
     }
    }
   }
  }
  default: {
   const $11 = ($0.a2.a1+$0.a2.a2);
   switch(Prelude_Types_isSpace($0.a2.a1)) {
    case 1: return {h: 1 /* {TcContinue9:1} */, a1: $0.a2.a2, a2: $0.a2.a3()};
    case 0: return {h: 0 /* {TcDone:9} */, a1: $11};
   }
  }
 }
}

/* Data.String.with block in ltrim */
function Data_String_with__ltrim_9542($0, $1) {
 return __tailRec(x24tcOpt_9, {h: 1 /* {TcContinue9:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:10} */
function x24tcOpt_10($0) {
 switch($0.a3.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:10} */, a1: {h: 0}};
  case undefined: /* cons */ {
   switch($0.a1($0.a2)($0.a3.a1.a1)) {
    case 1: return {h: 0 /* {TcDone:10} */, a1: {a1: $0.a3.a1.a2}};
    case 0: return {h: 1 /* {TcContinue10:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3.a2};
   }
  }
 }
}

/* Data.List.lookupBy : (a -> b -> Bool) -> a -> List (b, v) -> Maybe v */
function Data_List_lookupBy($0, $1, $2) {
 return __tailRec(x24tcOpt_10, {h: 1 /* {TcContinue10:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:11} */
function x24tcOpt_11($0) {
 switch($0.a2.h) {
  case 0: /* nil */ {
   switch($0.a3.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:11} */, a1: {h: 0}};
    default: {
     switch($0.a3.h) {
      case 0: /* nil */ return {h: 0 /* {TcDone:11} */, a1: {a1: Prelude_Types_List_reverse($0.a2), a2: {h: 0}}};
      case undefined: /* cons */ {
       switch($0.a3.a1) {
        case '\n': return {h: 0 /* {TcDone:11} */, a1: {a1: Prelude_Types_List_reverse($0.a2), a2: Data_String_n__3977_9380_linesHelp($0.a1, {h: 0}, $0.a3.a2)}};
        case '\r': {
         switch($0.a3.a2.h) {
          case undefined: /* cons */ {
           switch($0.a3.a2.a1) {
            case '\n': return {h: 0 /* {TcDone:11} */, a1: {a1: Prelude_Types_List_reverse($0.a2), a2: Data_String_n__3977_9380_linesHelp($0.a1, {h: 0}, $0.a3.a2.a2)}};
            default: return {h: 0 /* {TcDone:11} */, a1: {a1: Prelude_Types_List_reverse($0.a2), a2: Data_String_n__3977_9380_linesHelp($0.a1, {h: 0}, $0.a3.a2)}};
           }
          }
          default: return {h: 0 /* {TcDone:11} */, a1: {a1: Prelude_Types_List_reverse($0.a2), a2: Data_String_n__3977_9380_linesHelp($0.a1, {h: 0}, $0.a3.a2)}};
         }
        }
        default: return {h: 1 /* {TcContinue11:1} */, a1: $0.a1, a2: {a1: $0.a3.a1, a2: $0.a2}, a3: $0.a3.a2};
       }
      }
     }
    }
   }
  }
  default: {
   switch($0.a3.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:11} */, a1: {a1: Prelude_Types_List_reverse($0.a2), a2: {h: 0}}};
    case undefined: /* cons */ {
     switch($0.a3.a1) {
      case '\n': return {h: 0 /* {TcDone:11} */, a1: {a1: Prelude_Types_List_reverse($0.a2), a2: Data_String_n__3977_9380_linesHelp($0.a1, {h: 0}, $0.a3.a2)}};
      case '\r': {
       switch($0.a3.a2.h) {
        case undefined: /* cons */ {
         switch($0.a3.a2.a1) {
          case '\n': return {h: 0 /* {TcDone:11} */, a1: {a1: Prelude_Types_List_reverse($0.a2), a2: Data_String_n__3977_9380_linesHelp($0.a1, {h: 0}, $0.a3.a2.a2)}};
          default: return {h: 0 /* {TcDone:11} */, a1: {a1: Prelude_Types_List_reverse($0.a2), a2: Data_String_n__3977_9380_linesHelp($0.a1, {h: 0}, $0.a3.a2)}};
         }
        }
        default: return {h: 0 /* {TcDone:11} */, a1: {a1: Prelude_Types_List_reverse($0.a2), a2: Data_String_n__3977_9380_linesHelp($0.a1, {h: 0}, $0.a3.a2)}};
       }
      }
      default: return {h: 1 /* {TcContinue11:1} */, a1: $0.a1, a2: {a1: $0.a3.a1, a2: $0.a2}, a3: $0.a3.a2};
     }
    }
   }
  }
 }
}

/* Data.String.3977:9380:linesHelp */
function Data_String_n__3977_9380_linesHelp($0, $1, $2) {
 return __tailRec(x24tcOpt_11, {h: 1 /* {TcContinue11:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:12} */
function x24tcOpt_12($0) {
 switch($0.a2.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:12} */, a1: $0.a1};
  case undefined: /* cons */ return {h: 1 /* {TcContinue12:1} */, a1: ($0.a1+1n), a2: $0.a2.a2};
 }
}

/* Prelude.Types.List.lengthPlus : Nat -> List a -> Nat */
function Prelude_Types_List_lengthPlus($0, $1) {
 return __tailRec(x24tcOpt_12, {h: 1 /* {TcContinue12:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:13} */
function x24tcOpt_13($0) {
 switch($0.a4.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:13} */, a1: $0.a3};
  case undefined: /* cons */ return {h: 1 /* {TcContinue13:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a4.a1, a4: $0.a4.a2};
 }
}

/* Data.List1.3038:2661:loop */
function Data_List1_n__3038_2661_loop($0, $1, $2, $3) {
 return __tailRec(x24tcOpt_13, {h: 1 /* {TcContinue13:1} */, a1: $0, a2: $1, a3: $2, a4: $3});
}

/* {$tcOpt:14} */
function x24tcOpt_14($0) {
 switch($0.a2.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:14} */, a1: {h: 0}};
  case undefined: /* cons */ {
   switch($0.a3) {
    case 0n: return {h: 0 /* {TcDone:14} */, a1: {a1: $0.a2.a1}};
    default: {
     const $7 = ($0.a3-1n);
     return {h: 1 /* {TcContinue14:1} */, a1: $0.a1, a2: $0.a2.a2, a3: $7};
    }
   }
  }
 }
}

/* Data.String.Extra.with block in index */
function Data_String_Extra_with__index_3642($0, $1, $2) {
 return __tailRec(x24tcOpt_14, {h: 1 /* {TcContinue14:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:15} */
function x24tcOpt_15($0) {
 switch($0.a1.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:15} */, a1: {h: 0 /* Left */, a1: ('could not find the type of identifier '+Pacillus_Idris2LSP_Syntax_SimpleExpr_show_Show_Identifier($0.a2))}};
  case undefined: /* cons */ {
   const $a = {a1: $0.a1.a1.a1, a2: $0.a1.a1.a2};
   switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_x3dx3d_Eq_Identifier($0.a1.a1.a1, $0.a2)) {
    case 1: return {h: 0 /* {TcDone:15} */, a1: {h: 1 /* Right */, a1: {h: 0 /* Start */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_toSpSig($a)}}};
    case 0: return {h: 1 /* {TcContinue15:1} */, a1: $0.a1.a2, a2: $0.a2};
   }
  }
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.getIdType : List Signature -> Identifier -> Either String ReconsTree */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_getIdType($0, $1) {
 return __tailRec(x24tcOpt_15, {h: 1 /* {TcContinue15:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:16} */
function x24tcOpt_16($0) {
 switch($0.a1) {
  case 0n: {
   switch($0.a2.h) {
    case undefined: /* cons */ return {h: 0 /* {TcDone:16} */, a1: {a1: $0.a2.a1}};
    default: return {h: 0 /* {TcDone:16} */, a1: {h: 0}};
   }
  }
  default: {
   const $8 = ($0.a1-1n);
   switch($0.a2.h) {
    case undefined: /* cons */ return {h: 1 /* {TcContinue16:1} */, a1: $8, a2: $0.a2.a2};
    default: return {h: 0 /* {TcDone:16} */, a1: {h: 0}};
   }
  }
 }
}

/* Prelude.Types.getAt : Nat -> List a -> Maybe a */
function Prelude_Types_getAt($0, $1) {
 return __tailRec(x24tcOpt_16, {h: 1 /* {TcContinue16:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:17} */
function x24tcOpt_17($0) {
 switch($0.a3.h) {
  case undefined: /* cons */ {
   switch($0.a2($0.a3.a1)) {
    case 1: return {h: 1 /* {TcContinue17:1} */, a1: {a1: $0.a1, a2: $0.a3.a1}, a2: $0.a2, a3: $0.a3.a2};
    case 0: return {h: 1 /* {TcContinue17:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3.a2};
   }
  }
  case 0: /* nil */ return {h: 0 /* {TcDone:17} */, a1: Prelude_Types_SnocList_x3cx3ex3e($0.a1, {h: 0})};
 }
}

/* Prelude.Types.List.filterAppend : SnocList a -> (a -> Bool) -> List a -> List a */
function Prelude_Types_List_filterAppend($0, $1, $2) {
 return __tailRec(x24tcOpt_17, {h: 1 /* {TcContinue17:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:18} */
function x24tcOpt_18($0) {
 switch($0.a3.h) {
  case undefined: /* cons */ return {h: 1 /* {TcContinue18:1} */, a1: {a1: $0.a1, a2: $0.a2($0.a3.a1)}, a2: $0.a2, a3: $0.a3.a2};
  case 0: /* nil */ return {h: 0 /* {TcDone:18} */, a1: Prelude_Types_SnocList_x3cx3ex3e($0.a1, {h: 0})};
 }
}

/* Prelude.Types.List.mapAppend : SnocList b -> (a -> b) -> List a -> List b */
function Prelude_Types_List_mapAppend($0, $1, $2) {
 return __tailRec(x24tcOpt_18, {h: 1 /* {TcContinue18:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:19} */
function x24tcOpt_19($0) {
 switch($0.a1) {
  case 0n: return {h: 0 /* {TcDone:19} */, a1: $0.a2};
  default: {
   const $4 = ($0.a1-1n);
   switch($0.a2.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:19} */, a1: {h: 0}};
    case undefined: /* cons */ return {h: 1 /* {TcContinue19:1} */, a1: $4, a2: $0.a2.a2};
   }
  }
 }
}

/* Data.List.drop : Nat -> List a -> List a */
function Data_List_drop($0, $1) {
 return __tailRec(x24tcOpt_19, {h: 1 /* {TcContinue19:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:20} */
function x24tcOpt_20($0) {
 switch($0.a3.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:20} */, a1: $0.a2};
  case undefined: /* cons */ return {h: 1 /* {TcContinue20:1} */, a1: $0.a1, a2: $0.a1($0.a2)($0.a3.a1), a3: $0.a3.a2};
 }
}

/* Prelude.Types.foldl */
function Prelude_Types_foldl_Foldable_List($0, $1, $2) {
 return __tailRec(x24tcOpt_20, {h: 1 /* {TcContinue20:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:21} */
function x24tcOpt_21($0) {
 switch($0.h) {
  case 1: /* {TcContinue21:1} */ {
   switch($0.a1.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:21} */, a1: {h: 1 /* Right */, a1: $0.a2}};
    case undefined: /* cons */ return {h: 2 /* {TcContinue21:2} */, a1: $0.a1.a1.a1, a2: $0.a1.a1.a2, a3: $0.a1.a2, a4: $0.a2, a5: {a1: $0.a1.a1.a1, a2: $0.a1.a1.a2}};
   }
  }
  case 2: /* {TcContinue21:2} */ {
   switch($0.a5.a1.h) {
    case 0: /* IdTerm */ {
     switch($0.a5.a2.h) {
      case 0: /* IdTerm */ {
       switch(Pacillus_Idris2LSP_TypeRecons_TypeRecons_isImplicitVar($0.a5.a1.a1)) {
        case 0: {
         switch(Pacillus_Idris2LSP_TypeRecons_TypeRecons_isImplicitVar($0.a5.a2.a1)) {
          case 0: return {h: 0 /* {TcDone:21} */, a1: {h: 0 /* Left */, a1: 'unexpected error: illegal constraints'}};
          case 1: return {h: 1 /* {TcContinue21:1} */, a1: $0.a3, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a4, {h: 0 /* IdTerm */, a1: $0.a5.a2.a1}, {h: 0 /* IdTerm */, a1: $0.a5.a1.a1})};
         }
        }
        case 1: {
         switch(Pacillus_Idris2LSP_TypeRecons_TypeRecons_isImplicitVar($0.a5.a2.a1)) {
          case 0: return {h: 1 /* {TcContinue21:1} */, a1: $0.a3, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a4, {h: 0 /* IdTerm */, a1: $0.a5.a1.a1}, {h: 0 /* IdTerm */, a1: $0.a5.a2.a1})};
          case 1: return {h: 1 /* {TcContinue21:1} */, a1: $0.a3, a2: $0.a4};
         }
        }
       }
      }
      default: {
       switch(Pacillus_Idris2LSP_TypeRecons_TypeRecons_isImplicitVar($0.a5.a1.a1)) {
        case 1: return {h: 1 /* {TcContinue21:1} */, a1: $0.a3, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a4, {h: 0 /* IdTerm */, a1: $0.a5.a1.a1}, $0.a5.a2)};
        case 0: return {h: 0 /* {TcDone:21} */, a1: {h: 0 /* Left */, a1: 'unexpected error: illegal constraints'}};
       }
      }
     }
    }
    default: {
     switch($0.a5.a2.h) {
      case 0: /* IdTerm */ {
       switch(Pacillus_Idris2LSP_TypeRecons_TypeRecons_isImplicitVar($0.a5.a2.a1)) {
        case 1: return {h: 1 /* {TcContinue21:1} */, a1: $0.a3, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a4, {h: 0 /* IdTerm */, a1: $0.a5.a2.a1}, $0.a5.a1)};
        case 0: return {h: 0 /* {TcDone:21} */, a1: {h: 0 /* Left */, a1: 'unexpected error: illegal constraints'}};
       }
      }
      default: return {h: 0 /* {TcDone:21} */, a1: {h: 0 /* Left */, a1: 'unexpected error: illegal constraints'}};
     }
    }
   }
  }
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.applyConstraints : Constraints -> SimpleExpr -> Either String SimpleExpr */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_applyConstraints($0, $1) {
 return __tailRec(x24tcOpt_21, {h: 1 /* {TcContinue21:1} */, a1: $0, a2: $1});
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.case block in applyConstraints */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_case__applyConstraints_4235($0, $1, $2, $3, $4) {
 return __tailRec(x24tcOpt_21, {h: 2 /* {TcContinue21:2} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4});
}

/* {$tcOpt:22} */
function x24tcOpt_22($0) {
 switch($0.h) {
  case 1: /* {TcContinue22:1} */ {
   switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_exprEquality($0.a1.a1, $0.a2.a1)) {
    case 1: return {h: 2 /* {TcContinue22:2} */, a1: $0.a1.a2, a2: $0.a2.a2};
    case 0: return {h: 0 /* {TcDone:22} */, a1: 0};
   }
  }
  case 2: /* {TcContinue22:2} */ {
   switch($0.a1.h) {
    case 0: /* IdTerm */ {
     switch($0.a2.h) {
      case 0: /* IdTerm */ return {h: 0 /* {TcDone:22} */, a1: Pacillus_Idris2LSP_Syntax_SimpleExpr_x3dx3d_Eq_Identifier($0.a1.a1, $0.a2.a1)};
      default: return {h: 0 /* {TcDone:22} */, a1: 0};
     }
    }
    case 1: /* AppTerm */ {
     switch($0.a2.h) {
      case 1: /* AppTerm */ return {h: 1 /* {TcContinue22:1} */, a1: $0.a1.a1, a2: $0.a2.a1};
      default: return {h: 0 /* {TcDone:22} */, a1: 0};
     }
    }
    case 2: /* ArwTerm */ {
     switch($0.a2.h) {
      case 2: /* ArwTerm */ return {h: 5 /* {TcContinue22:5} */, a1: $0.a1.a1, a2: $0.a2.a1};
      default: return {h: 0 /* {TcDone:22} */, a1: 0};
     }
    }
    case 3: /* EqTerm */ {
     switch($0.a2.h) {
      case 3: /* EqTerm */ return {h: 3 /* {TcContinue22:3} */, a1: $0.a1.a1, a2: $0.a2.a1};
      default: return {h: 0 /* {TcDone:22} */, a1: 0};
     }
    }
    case 8: /* PrTerm */ {
     switch($0.a2.h) {
      case 8: /* PrTerm */ return {h: 4 /* {TcContinue22:4} */, a1: $0.a1.a1, a2: $0.a2.a1};
      default: return {h: 0 /* {TcDone:22} */, a1: 0};
     }
    }
    case 4: /* IntegerLiteral */ {
     switch($0.a2.h) {
      case 4: /* IntegerLiteral */ return {h: 0 /* {TcDone:22} */, a1: Prelude_EqOrd_x3dx3d_Eq_Integer($0.a1.a1, $0.a2.a1)};
      default: return {h: 0 /* {TcDone:22} */, a1: 0};
     }
    }
    case 5: /* DoubleLiteral */ {
     switch($0.a2.h) {
      case 5: /* DoubleLiteral */ return {h: 0 /* {TcDone:22} */, a1: Prelude_EqOrd_x3dx3d_Eq_Double($0.a1.a1, $0.a2.a1)};
      default: return {h: 0 /* {TcDone:22} */, a1: 0};
     }
    }
    case 7: /* StringLiteral */ {
     switch($0.a2.h) {
      case 7: /* StringLiteral */ return {h: 0 /* {TcDone:22} */, a1: Prelude_EqOrd_x3dx3d_Eq_String($0.a1.a1, $0.a2.a1)};
      default: return {h: 0 /* {TcDone:22} */, a1: 0};
     }
    }
    case 9: /* UnitTerm */ {
     switch($0.a2.h) {
      case 9: /* UnitTerm */ return {h: 0 /* {TcDone:22} */, a1: 1};
      default: return {h: 0 /* {TcDone:22} */, a1: 0};
     }
    }
    default: return {h: 0 /* {TcDone:22} */, a1: 0};
   }
  }
  case 3: /* {TcContinue22:3} */ {
   switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_exprEquality($0.a1.a1, $0.a2.a1)) {
    case 1: return {h: 2 /* {TcContinue22:2} */, a1: $0.a1.a2, a2: $0.a2.a2};
    case 0: return {h: 0 /* {TcDone:22} */, a1: 0};
   }
  }
  case 4: /* {TcContinue22:4} */ {
   switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_exprEquality($0.a1.a1, $0.a2.a1)) {
    case 1: return {h: 2 /* {TcContinue22:2} */, a1: $0.a1.a2, a2: $0.a2.a2};
    case 0: return {h: 0 /* {TcDone:22} */, a1: 0};
   }
  }
  case 5: /* {TcContinue22:5} */ {
   switch($0.a1.h) {
    case 0: /* ExExArr */ {
     switch($0.a2.h) {
      case 0: /* ExExArr */ {
       switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_exprEquality($0.a1.a2, $0.a1.a3)) {
        case 1: return {h: 2 /* {TcContinue22:2} */, a1: $0.a2.a2, a2: $0.a2.a3};
        case 0: return {h: 0 /* {TcDone:22} */, a1: 0};
       }
      }
      case 1: /* SiExArr */ {
       switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_exprEquality($0.a1.a2, $0.a1.a3)) {
        case 1: return {h: 2 /* {TcContinue22:2} */, a1: $0.a2.a1.a2, a2: $0.a2.a2};
        case 0: return {h: 0 /* {TcDone:22} */, a1: 0};
       }
      }
     }
    }
    case 1: /* SiExArr */ {
     switch($0.a2.h) {
      case 0: /* ExExArr */ {
       switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_exprEquality($0.a1.a1.a2, $0.a1.a2)) {
        case 1: return {h: 2 /* {TcContinue22:2} */, a1: $0.a2.a2, a2: $0.a2.a3};
        case 0: return {h: 0 /* {TcDone:22} */, a1: 0};
       }
      }
      case 1: /* SiExArr */ {
       switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_exprEquality($0.a1.a1.a2, $0.a1.a2)) {
        case 1: return {h: 2 /* {TcContinue22:2} */, a1: $0.a2.a1.a2, a2: $0.a2.a2};
        case 0: return {h: 0 /* {TcDone:22} */, a1: 0};
       }
      }
     }
    }
   }
  }
 }
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.appEquality : Application -> Application -> Bool */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_appEquality($0, $1) {
 return __tailRec(x24tcOpt_22, {h: 1 /* {TcContinue22:1} */, a1: $0, a2: $1});
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.exprEquality : SimpleExpr -> SimpleExpr -> Bool */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_exprEquality($0, $1) {
 return __tailRec(x24tcOpt_22, {h: 2 /* {TcContinue22:2} */, a1: $0, a2: $1});
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.eqEquality : Equality -> Equality -> Bool */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_eqEquality($0, $1) {
 return __tailRec(x24tcOpt_22, {h: 3 /* {TcContinue22:3} */, a1: $0, a2: $1});
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.prEquality : Pair -> Pair -> Bool */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_prEquality($0, $1) {
 return __tailRec(x24tcOpt_22, {h: 4 /* {TcContinue22:4} */, a1: $0, a2: $1});
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.sameTypeArw : Arrow b1 -> Arrow b2 -> Bool */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_sameTypeArw($0, $1) {
 return __tailRec(x24tcOpt_22, {h: 5 /* {TcContinue22:5} */, a1: $0, a2: $1});
}

/* {$tcOpt:23} */
function x24tcOpt_23($0) {
 switch($0.a2.h) {
  case 0: /* nil */ {
   switch($0.a3.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:23} */, a1: 1};
    default: return {h: 0 /* {TcDone:23} */, a1: 0};
   }
  }
  case undefined: /* cons */ {
   switch($0.a3.h) {
    case undefined: /* cons */ {
     switch($0.a1.a1($0.a2.a1)($0.a3.a1)) {
      case 1: return {h: 1 /* {TcContinue23:1} */, a1: $0.a1, a2: $0.a2.a2, a3: $0.a3.a2};
      case 0: return {h: 0 /* {TcDone:23} */, a1: 0};
     }
    }
    default: return {h: 0 /* {TcDone:23} */, a1: 0};
   }
  }
  default: return {h: 0 /* {TcDone:23} */, a1: 0};
 }
}

/* Prelude.Types.== */
function Prelude_Types_x3dx3d_Eq_x28Listx20x24ax29($0, $1, $2) {
 return __tailRec(x24tcOpt_23, {h: 1 /* {TcContinue23:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:24} */
function x24tcOpt_24($0) {
 switch($0.a1.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:24} */, a1: $0.a2};
  case undefined: /* cons */ return {h: 1 /* {TcContinue24:1} */, a1: $0.a1.a1, a2: {a1: $0.a1.a2, a2: $0.a2}};
 }
}

/* Prelude.Types.SnocList.(<>>) : SnocList a -> List a -> List a */
function Prelude_Types_SnocList_x3cx3ex3e($0, $1) {
 return __tailRec(x24tcOpt_24, {h: 1 /* {TcContinue24:1} */, a1: $0, a2: $1});
}

/* {__mainExpression:0} */
const __mainExpression_0 = __lazy(function () {
 return PrimIO_unsafePerformIO($2 => Pacillus_Idris2LSP_GetType_main($2));
});

/* {csegen:17} */
const csegen_17 = __lazy(function () {
 return {a1: $1 => $2 => ($1+$2), a2: $6 => $7 => ($6*$7), a3: $b => $b};
});

/* {csegen:19} */
const csegen_19 = __lazy(function () {
 return {a1: csegen_17(), a2: $3 => (0n-$3), a3: $7 => $8 => ($7-$8)};
});

/* {csegen:22} */
const csegen_22 = __lazy(function () {
 return b => a => $0 => $1 => Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29($0, $1);
});

/* {csegen:23} */
const csegen_23 = __lazy(function () {
 return a => $0 => Prelude_Types_join_Monad_x28Eitherx20x24ex29($0);
});

/* {csegen:25} */
const csegen_25 = __lazy(function () {
 return {a1: x => Pacillus_Idris2LSP_Syntax_Lexer_show_Show_SimpleExprToken(x), a2: d => x => Pacillus_Idris2LSP_Syntax_Lexer_showPrec_Show_SimpleExprToken(d, x)};
});

/* {csegen:28} */
const csegen_28 = __lazy(function () {
 return {a1: x => Text_Bounded_show_Show_x28WithBoundsx20x24tyx29(csegen_25(), x), a2: d => x => Text_Bounded_showPrec_Show_x28WithBoundsx20x24tyx29(csegen_25(), d, x)};
});

/* {csegen:31} */
const csegen_31 = __lazy(function () {
 return {a1: x => Text_Parser_Core_show_Show_x28ParsingErrorx20x24tokx29(csegen_25(), x), a2: d => x => Text_Parser_Core_showPrec_Show_x28ParsingErrorx20x24tokx29(csegen_25(), d, x)};
});

/* {csegen:34} */
const csegen_34 = __lazy(function () {
 return $0 => {
  switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_ignored($0)) {
   case 1: return 0;
   case 0: return 1;
  }
 };
});

/* {csegen:49} */
const csegen_49 = __lazy(function () {
 return {a1: acc => elem => func => init => input => Prelude_Types_foldr_Foldable_List(func, init, input), a2: elem => acc => func => init => input => Prelude_Types_foldl_Foldable_List(func, init, input), a3: elem => $b => Prelude_Types_null_Foldable_List($b), a4: elem => acc => m => $f => funcM => init => input => Prelude_Types_foldlM_Foldable_List($f, funcM, init, input), a5: elem => $16 => $16, a6: a => m => $18 => f => $19 => Prelude_Types_foldMap_Foldable_List($18, f, $19)};
});

/* {csegen:62} */
const csegen_62 = __lazy(function () {
 return {a1: $1 => $2 => Prelude_EqOrd_x3dx3d_Eq_String($1, $2), a2: $7 => $8 => Prelude_EqOrd_x2fx3d_Eq_String($7, $8)};
});

/* {csegen:91} */
const csegen_91 = __lazy(function () {
 return {a1: $1 => $2 => Prelude_EqOrd_x3dx3d_Eq_Char($1, $2), a2: $7 => $8 => Prelude_EqOrd_x2fx3d_Eq_Char($7, $8)};
});

/* {csegen:104} */
const csegen_104 = __lazy(function () {
 return c => {
  switch(Prelude_EqOrd_x3dx3d_Eq_Char(c, 'e')) {
   case 1: return 1;
   case 0: return Prelude_EqOrd_x3dx3d_Eq_Char(c, 'E');
  }
 };
});

/* {csegen:202} */
const csegen_202 = __lazy(function () {
 return {a1: $1 => $2 => ($1+$2), a2: ''};
});

/* {csegen:204} */
const csegen_204 = __lazy(function () {
 return {a1: x => Prelude_Show_show_Show_Int(x), a2: d => x => Prelude_Show_showPrec_Show_Int(d, x)};
});

/* {csegen:219} */
const csegen_219 = __lazy(function () {
 return {a1: {a1: $2 => $3 => Prelude_EqOrd_x3dx3d_Eq_Int($2, $3), a2: $8 => $9 => Prelude_EqOrd_x2fx3d_Eq_Int($8, $9)}, a2: $e => $f => Prelude_EqOrd_compare_Ord_Int($e, $f), a3: $14 => $15 => Prelude_EqOrd_x3c_Ord_Int($14, $15), a4: $1a => $1b => Prelude_EqOrd_x3e_Ord_Int($1a, $1b), a5: $20 => $21 => Prelude_EqOrd_x3cx3d_Ord_Int($20, $21), a6: $26 => $27 => Prelude_EqOrd_x3ex3d_Ord_Int($26, $27), a7: $2c => $2d => Prelude_EqOrd_max_Ord_Int($2c, $2d), a8: $32 => $33 => Prelude_EqOrd_min_Ord_Int($32, $33)};
});

/* {csegen:222} */
const csegen_222 = __lazy(function () {
 return x => Prelude_EqOrd_x3dx3d_Eq_Char(x, '.');
});

/* {csegen:224} */
const csegen_224 = __lazy(function () {
 return {a1: $1 => Pacillus_Idris2LSP_Syntax_Lexer_TokType_TokenKind_SimpleExprTokenKind($1), a2: kind => $5 => Pacillus_Idris2LSP_Syntax_Lexer_tokValue_TokenKind_SimpleExprTokenKind(kind, $5)};
});

/* {csegen:227} */
const csegen_227 = __lazy(function () {
 return {a1: $1 => $2 => Pacillus_Idris2LSP_Syntax_Lexer_x3dx3d_Eq_SimpleExprTokenKind($1, $2), a2: $7 => $8 => Pacillus_Idris2LSP_Syntax_Lexer_x2fx3d_Eq_SimpleExprTokenKind($7, $8)};
});

/* {csegen:228} */
const csegen_228 = __lazy(function () {
 return Text_Parser_match(csegen_224(), csegen_227(), 2);
});

/* {csegen:229} */
const csegen_229 = __lazy(function () {
 return Text_Parser_match(csegen_224(), csegen_227(), 3);
});

/* {csegen:233} */
const csegen_233 = __lazy(function () {
 return Text_Parser_match(csegen_224(), csegen_227(), 4);
});

/* {csegen:236} */
const csegen_236 = __lazy(function () {
 return Text_Parser_match(csegen_224(), csegen_227(), 9);
});

/* {csegen:253} */
const csegen_253 = __lazy(function () {
 return Text_Parser_match(csegen_224(), csegen_227(), 5);
});

/* {csegen:266} */
const csegen_266 = __lazy(function () {
 return Text_Parser_match(csegen_224(), csegen_227(), 6);
});

/* {csegen:277} */
const csegen_277 = __lazy(function () {
 return {a1: {a1: 'End of input', a2: {h: 0}}, a2: {h: 0}};
});

/* {csegen:280} */
const csegen_280 = __lazy(function () {
 return {a1: $1 => $2 => _add32s($1, $2), a2: $6 => $7 => _mul32s($6, $7), a3: $b => Number(_truncBigInt32($b))};
});

/* {csegen:294} */
const csegen_294 = __lazy(function () {
 return {a1: $1 => Language_JSON_Tokens_TokType_TokenKind_JSONTokenKind($1), a2: kind => $5 => Language_JSON_Tokens_tokValue_TokenKind_JSONTokenKind(kind, $5)};
});

/* {csegen:297} */
const csegen_297 = __lazy(function () {
 return {a1: $1 => $2 => Language_JSON_Tokens_x3dx3d_Eq_JSONTokenKind($1, $2), a2: $7 => $8 => Language_JSON_Tokens_x2fx3d_Eq_JSONTokenKind($7, $8)};
});

/* {csegen:326} */
const csegen_326 = __lazy(function () {
 return {a1: $1 => Language_JSON_String_Tokens_TokType_TokenKind_JSONStringTokenKind($1), a2: kind => $5 => Language_JSON_String_Tokens_tokValue_TokenKind_JSONStringTokenKind(kind, $5)};
});

/* {csegen:329} */
const csegen_329 = __lazy(function () {
 return {a1: $1 => $2 => Language_JSON_String_Tokens_x3dx3d_Eq_JSONStringTokenKind($1, $2), a2: $7 => $8 => Language_JSON_String_Tokens_x2fx3d_Eq_JSONStringTokenKind($7, $8)};
});

/* prim__sub_Integer : Integer -> Integer -> Integer */
function prim__sub_Integer($0, $1) {
 return ($0-$1);
}

/* Pacillus.Idris2LSP.GetType.4530:5272:convstrarr */
function Pacillus_Idris2LSP_GetType_n__4530_5272_convstrarr($0, $1, $2) {
 switch($2.h) {
  case 3: /* JString */ return {h: 1 /* Right */, a1: $2.a1};
  default: return {h: 0 /* Left */, a1: 'Error : Non string at \"sigs\" in input JSON'};
 }
}

/* Pacillus.Idris2LSP.GetType.4530:5271:conv3 */
function Pacillus_Idris2LSP_GetType_n__4530_5271_conv3($0, $1, $2) {
 switch($2.h) {
  case undefined: /* cons */ {
   switch($2.a1) {
    case 'sigs': {
     switch($2.a2.h) {
      case 4: /* JArray */ return {h: 1 /* Right */, a1: $2.a2.a1};
      default: return {h: 0 /* Left */, a1: ''};
     }
    }
    default: return {h: 0 /* Left */, a1: ''};
   }
  }
  default: return {h: 0 /* Left */, a1: ''};
 }
}

/* Pacillus.Idris2LSP.GetType.4344:5079:conv3 */
function Pacillus_Idris2LSP_GetType_n__4344_5079_conv3($0, $1) {
 switch($1.h) {
  case undefined: /* cons */ {
   switch($1.a1) {
    case 'assoc': {
     switch($1.a2.h) {
      case 3: /* JString */ return {h: 1 /* Right */, a1: $1.a2.a1};
      default: return {h: 0 /* Left */, a1: ''};
     }
    }
    default: return {h: 0 /* Left */, a1: ''};
   }
  }
  default: return {h: 0 /* Left */, a1: ''};
 }
}

/* Pacillus.Idris2LSP.GetType.4530:5270:conv2 */
function Pacillus_Idris2LSP_GetType_n__4530_5270_conv2($0, $1, $2) {
 switch($2.h) {
  case undefined: /* cons */ {
   switch($2.a1) {
    case 'ops': {
     switch($2.a2.h) {
      case 4: /* JArray */ return {h: 1 /* Right */, a1: $2.a2.a1};
      default: return {h: 0 /* Left */, a1: ''};
     }
    }
    default: return {h: 0 /* Left */, a1: ''};
   }
  }
  default: return {h: 0 /* Left */, a1: ''};
 }
}

/* Pacillus.Idris2LSP.GetType.4344:5078:conv2 */
function Pacillus_Idris2LSP_GetType_n__4344_5078_conv2($0, $1) {
 switch($1.h) {
  case undefined: /* cons */ {
   switch($1.a1) {
    case 'prec': {
     switch($1.a2.h) {
      case 3: /* JString */ return {h: 1 /* Right */, a1: $1.a2.a1};
      default: return {h: 0 /* Left */, a1: ''};
     }
    }
    default: return {h: 0 /* Left */, a1: ''};
   }
  }
  default: return {h: 0 /* Left */, a1: ''};
 }
}

/* Pacillus.Idris2LSP.GetType.4530:5269:conv1 */
function Pacillus_Idris2LSP_GetType_n__4530_5269_conv1($0, $1, $2) {
 switch($2.h) {
  case undefined: /* cons */ {
   switch($2.a1) {
    case 'expr': {
     switch($2.a2.h) {
      case 3: /* JString */ return {h: 1 /* Right */, a1: $2.a2.a1};
      default: return {h: 0 /* Left */, a1: ''};
     }
    }
    default: return {h: 0 /* Left */, a1: ''};
   }
  }
  default: return {h: 0 /* Left */, a1: ''};
 }
}

/* Pacillus.Idris2LSP.GetType.4344:5077:conv1 */
function Pacillus_Idris2LSP_GetType_n__4344_5077_conv1($0, $1) {
 switch($1.h) {
  case undefined: /* cons */ {
   switch($1.a1) {
    case 'symbol': {
     switch($1.a2.h) {
      case 3: /* JString */ return {h: 1 /* Right */, a1: $1.a2.a1};
      default: return {h: 0 /* Left */, a1: ''};
     }
    }
    default: return {h: 0 /* Left */, a1: ''};
   }
  }
  default: return {h: 0 /* Left */, a1: ''};
 }
}

/* Pacillus.Idris2LSP.GetType.4344:5076:assocconv */
function Pacillus_Idris2LSP_GetType_n__4344_5076_assocconv($0, $1) {
 switch($1) {
  case 'infixr': return {h: 1 /* Right */, a1: 2};
  case 'infixl': return {h: 1 /* Right */, a1: 1};
  case 'infix': return {h: 1 /* Right */, a1: 0};
  default: return {h: 0 /* Left */, a1: 'Error : Invalid value at \"ops.assoc\" in input JSON'};
 }
}

/* Pacillus.Idris2LSP.GetType.process : String -> String */
function Pacillus_Idris2LSP_GetType_process($0) {
 const $1 = Pacillus_Idris2LSP_GetType_parseInput($0);
 switch($1.h) {
  case 0: /* Left */ return $1.a1;
  case 1: /* Right */ return Pacillus_Idris2LSP_GetType_inferType($1.a1.a1, $1.a1.a2.a1, $1.a1.a2.a2);
 }
}

/* Pacillus.Idris2LSP.GetType.parseInput : String -> Either String (String, (InOperatorMap, List String)) */
function Pacillus_Idris2LSP_GetType_parseInput($0) {
 const $1 = Language_JSON_parse($0);
 switch($1.h) {
  case 0: /* nothing */ return {h: 0 /* Left */, a1: 'Error : Input JSON parse failed'};
  case undefined: /* just */ return Pacillus_Idris2LSP_GetType_json2Info($1.a1);
 }
}

/* Pacillus.Idris2LSP.GetType.main : IO () */
function Pacillus_Idris2LSP_GetType_main($0) {
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
  case undefined: /* cons */ {
   switch($1.a2.h) {
    case 0: /* nil */ return Prelude_IO_prim__putStr('*Error : No input arguments\n', $0);
    case undefined: /* cons */ return Prelude_IO_prim__putStr((Pacillus_Idris2LSP_GetType_process($1.a2.a1)+'\n'), $0);
   }
  }
 }
}

/* Pacillus.Idris2LSP.GetType.json2OpMap : JSON -> Either String OpRecord */
function Pacillus_Idris2LSP_GetType_json2OpMap($0) {
 switch($0.h) {
  case 5: /* JObject */ return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_GetType_findFirst('Error : Failed to find member \"ops.symbol\" in input JSON', $7 => Pacillus_Idris2LSP_GetType_n__4344_5077_conv1($0.a1, $7), $0.a1), symbol => Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_GetType_findFirst('Error : Failed to find member \"ops.prec\" in input JSON', $12 => Pacillus_Idris2LSP_GetType_n__4344_5078_conv2($0.a1, $12), $0.a1), precraw => Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_GetType_findFirst('Error : Failed to find member \"ops.assoc\" in input JSON', $1d => Pacillus_Idris2LSP_GetType_n__4344_5079_conv3($0.a1, $1d), $0.a1), assocraw => Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Prelude_Types_maybe(() => ({h: 0 /* Left */, a1: 'Error : Found none number at \"ops.prec\" in input JSON'}), () => $29 => ({h: 1 /* Right */, a1: $29}), Prelude_Types_map_Functor_Maybe($2e => Prelude_Types_prim__integerToNat($2e), Data_String_parseInteger(csegen_17(), csegen_19(), precraw))), prec => Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_GetType_n__4344_5076_assocconv($0.a1, assocraw), assoc => ({h: 1 /* Right */, a1: {a1: symbol, a2: prec, a3: assoc}}))))));
  default: return {h: 0 /* Left */, a1: 'Invalid JSON format at \"ops\" in input JSON '};
 }
}

/* Pacillus.Idris2LSP.GetType.json2Info : JSON -> Either String (String, (InOperatorMap, List String)) */
function Pacillus_Idris2LSP_GetType_json2Info($0) {
 switch($0.h) {
  case 5: /* JObject */ {
   const $d = x => {
    const $19 = yraw => {
     const $25 = zraw => {
      const $2b = b => a => func => $2c => {
       switch($2c.h) {
        case 0: /* Left */ return {h: 0 /* Left */, a1: $2c.a1};
        case 1: /* Right */ return {h: 1 /* Right */, a1: func($2c.a1)};
       }
      };
      const $35 = b => a => $36 => $37 => {
       switch($36.h) {
        case 0: /* Left */ return {h: 0 /* Left */, a1: $36.a1};
        case 1: /* Right */ {
         switch($37.h) {
          case 1: /* Right */ return {h: 1 /* Right */, a1: $36.a1($37.a1)};
          case 0: /* Left */ return {h: 0 /* Left */, a1: $37.a1};
         }
        }
       }
      };
      const $2a = {a1: $2b, a2: a => $33 => ({h: 1 /* Right */, a1: $33}), a3: $35};
      const $29 = {a1: $2a, a2: csegen_22(), a3: csegen_23()};
      const $27 = Pacillus_Idris2LSP_GetType_convertInList2ListIn($29, Prelude_Types_List_mapAppend({h: 0}, $47 => Pacillus_Idris2LSP_GetType_json2OpMap($47), yraw));
      const $4b = y => {
       const $51 = b => a => func => $52 => {
        switch($52.h) {
         case 0: /* Left */ return {h: 0 /* Left */, a1: $52.a1};
         case 1: /* Right */ return {h: 1 /* Right */, a1: func($52.a1)};
        }
       };
       const $5b = b => a => $5c => $5d => {
        switch($5c.h) {
         case 0: /* Left */ return {h: 0 /* Left */, a1: $5c.a1};
         case 1: /* Right */ {
          switch($5d.h) {
           case 1: /* Right */ return {h: 1 /* Right */, a1: $5c.a1($5d.a1)};
           case 0: /* Left */ return {h: 0 /* Left */, a1: $5d.a1};
          }
         }
        }
       };
       const $50 = {a1: $51, a2: a => $59 => ({h: 1 /* Right */, a1: $59}), a3: $5b};
       const $4f = {a1: $50, a2: csegen_22(), a3: csegen_23()};
       const $4d = Pacillus_Idris2LSP_GetType_convertInList2ListIn($4f, Prelude_Types_List_mapAppend({h: 0}, $6d => Pacillus_Idris2LSP_GetType_n__4530_5272_convstrarr($0.a1, $0, $6d), zraw));
       return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29($4d, z => ({h: 1 /* Right */, a1: {a1: x, a2: {a1: y, a2: z}}}));
      };
      return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29($27, $4b);
     };
     return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_GetType_findFirst('Error : Failed to find member \"sigs\" in input JSON', $1f => Pacillus_Idris2LSP_GetType_n__4530_5271_conv3($0.a1, $0, $1f), $0.a1), $25);
    };
    return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_GetType_findFirst('Error : Failed to find member \"ops\" in input JSON', $13 => Pacillus_Idris2LSP_GetType_n__4530_5270_conv2($0.a1, $0, $13), $0.a1), $19);
   };
   return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_GetType_findFirst('Error : Failed to find member \"expr\" in input JSON', $7 => Pacillus_Idris2LSP_GetType_n__4530_5269_conv1($0.a1, $0, $7), $0.a1), $d);
  }
  default: return {h: 0 /* Left */, a1: 'Invalid input JSON form'};
 }
}

/* Pacillus.Idris2LSP.GetType.inferType : String -> InOperatorMap -> List String -> String */
function Pacillus_Idris2LSP_GetType_inferType($0, $1, $2) {
 const $3 = Pacillus_Idris2LSP_Syntax_SimpleExpr_parse($1, $0);
 const $7 = Prelude_Types_List_mapAppend({h: 0}, $b => Pacillus_Idris2LSP_TypeRecons_TypeRecons_parseSig($1, $b), $2);
 const $16 = b => a => func => $17 => {
  switch($17.h) {
   case 0: /* Left */ return {h: 0 /* Left */, a1: $17.a1};
   case 1: /* Right */ return {h: 1 /* Right */, a1: func($17.a1)};
  }
 };
 const $20 = b => a => $21 => $22 => {
  switch($21.h) {
   case 0: /* Left */ return {h: 0 /* Left */, a1: $21.a1};
   case 1: /* Right */ {
    switch($22.h) {
     case 1: /* Right */ return {h: 1 /* Right */, a1: $21.a1($22.a1)};
     case 0: /* Left */ return {h: 0 /* Left */, a1: $22.a1};
    }
   }
  }
 };
 const $15 = {a1: $16, a2: a => $1e => ({h: 1 /* Right */, a1: $1e}), a3: $20};
 const $14 = {a1: $15, a2: csegen_22(), a3: csegen_23()};
 const $12 = Pacillus_Idris2LSP_GetType_convertInList2ListIn($14, $7);
 const $10 = Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29($12, sigs => Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29($3, $33 => Pacillus_Idris2LSP_TypeRecons_TypeRecons_getPartialType(sigs, $33)));
 switch($10.h) {
  case 0: /* Left */ return $10.a1;
  case 1: /* Right */ return Pacillus_Idris2LSP_TypeRecons_TypeRecons_show_Show_ReconsTree($10.a1);
 }
}

/* Pacillus.Idris2LSP.GetType.findFirst : String -> (a -> Either String b) -> List a -> Either String b */
function Pacillus_Idris2LSP_GetType_findFirst($0, $1, $2) {
 switch($2.h) {
  case 0: /* nil */ return {h: 0 /* Left */, a1: $0};
  case undefined: /* cons */ return Prelude_Types_either(() => $7 => Pacillus_Idris2LSP_GetType_findFirst($0, $1, $2.a2), () => $d => ({h: 1 /* Right */, a1: $d}), $1($2.a1));
 }
}

/* Pacillus.Idris2LSP.GetType.convertInList2ListIn : Monad f => List (f b) -> f (List b) */
function Pacillus_Idris2LSP_GetType_convertInList2ListIn($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return $0.a1.a2(undefined)({h: 0});
  case undefined: /* cons */ return $0.a2(undefined)(undefined)($1.a1)(x => $0.a2(undefined)(undefined)(Pacillus_Idris2LSP_GetType_convertInList2ListIn($0, $1.a2))(xs => $0.a1.a2(undefined)({a1: x, a2: xs})));
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.with block in getArwType */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_with__getArwType_4796($0, $1, $2) {
 const $9 = argsig => {
  const $f = retsig => {
   let $10;
   switch(Pacillus_Idris2LSP_TypeRecons_TypeRecons_n__6295_4800_isType($1.a3, $1.a2, $2, $0, Pacillus_Idris2LSP_TypeRecons_TypeRecons_getSubgoal(argsig))) {
    case 1: {
     $10 = Pacillus_Idris2LSP_TypeRecons_TypeRecons_n__6295_4800_isType($1.a3, $1.a2, $2, $0, Pacillus_Idris2LSP_TypeRecons_TypeRecons_getSubgoal(retsig));
     break;
    }
    case 0: {
     $10 = 0;
     break;
    }
   }
   switch($10) {
    case 1: return {h: 1 /* Right */, a1: {h: 1 /* Subgoal */, a1: {a1: argsig, a2: {a1: retsig, a2: {h: 0}}}, a2: {a1: {h: 2 /* ArwTerm */, a1: $0}, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_n__6295_4801_tycnst($1.a3, $1.a2, $2, $0)}}};
    case 0: return {h: 0 /* Left */, a1: 'Found none \"Type\" identifier in arrow'};
   }
  };
  return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getPartialType($2, $1.a3), $f);
 };
 return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getPartialType($2, $1.a2), $9);
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.with block in getAppliedType */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_with__getAppliedType_4461($0, $1, $2, $3) {
 switch($1.h) {
  case 2: /* ArwTerm */ {
   switch($3.h) {
    case undefined: /* cons */ {
     const $6 = {a1: $2, a2: $3.a1};
     return Pacillus_Idris2LSP_TypeRecons_TypeRecons_case__withx20blockx20inx20getAppliedType_4471($1.a1, $3.a2, $3.a1, $2, $0, $6, Pacillus_Idris2LSP_Syntax_SimpleExpr_forgetSig($1.a1));
    }
    default: {
     switch($0.h) {
      case 2: /* ArwTerm */ {
       switch($3.h) {
        case undefined: /* cons */ return {h: 0 /* Left */, a1: 'unexpected error: arrow changed by labeling implicit'};
        default: return {h: 0 /* Left */, a1: 'left side not an appliable form'};
       }
      }
      default: return {h: 0 /* Left */, a1: 'left side not an appliable form'};
     }
    }
   }
  }
  default: {
   switch($0.h) {
    case 2: /* ArwTerm */ {
     switch($3.h) {
      case undefined: /* cons */ return {h: 0 /* Left */, a1: 'unexpected error: arrow changed by labeling implicit'};
      default: return {h: 0 /* Left */, a1: 'left side not an appliable form'};
     }
    }
    default: return {h: 0 /* Left */, a1: 'left side not an appliable form'};
   }
  }
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.with block in parseSig */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_with__parseSig_3256($0, $1, $2) {
 switch($1.h) {
  case undefined: /* just */ return Pacillus_Idris2LSP_TypeRecons_TypeRecons_parseSignature($2, $1.a1);
  case 0: /* nothing */ return {h: 0 /* Left */, a1: 'Failed to lex.'};
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.case block in with block in getAppliedType */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_case__withx20blockx20inx20getAppliedType_4471($0, $1, $2, $3, $4, $5, $6) {
 const $8 = Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicit($1);
 return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_unify({a1: {a1: $6.a2, a2: $8}, a2: {h: 0}}), cons => Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_applyConstraints(cons, $6.a3), applied => ({h: 1 /* Right */, a1: {a1: {h: 1 /* AppTerm */, a1: $5}, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_n__5926_4489_assign($0, $1, $2, $3, $4, $5, $6.a3, $6.a2, $8, applied, $2)}})));
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.case block in parseSignature */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_case__parseSignature_3187($0, $1, $2) {
 switch($2.h) {
  case 1: /* Right */ {
   switch($2.a1.a2.h) {
    case 0: /* nil */ return {h: 1 /* Right */, a1: $2.a1.a1};
    default: return {h: 0 /* Left */, a1: ('contains tokens that were not consumed\n'+Prelude_Show_show_Show_x28Listx20x24ax29(csegen_28(), $2.a1.a2))};
   }
  }
  case 0: /* Left */ return {h: 0 /* Left */, a1: Data_List1_show_Show_x28List1x20x24ax29(csegen_31(), $2.a1)};
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.6295:4801:tycnst */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_n__6295_4801_tycnst($0, $1, $2, $3) {
 return {h: 0 /* IdTerm */, a1: 'Type'};
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.6104:4875:tycnst */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_n__6104_4875_tycnst($0, $1, $2) {
 return {h: 0 /* IdTerm */, a1: 'Type'};
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.6112:4922:mkpair */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_n__6112_4922_mkpair($0, $1, $2, $3, $4) {
 return {h: 8 /* PrTerm */, a1: {a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_getSpSigExpr(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getSubgoal($3)), a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_getSpSigExpr(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getSubgoal($4))}};
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.6295:4800:isType */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_n__6295_4800_isType($0, $1, $2, $3, $4) {
 return Pacillus_Idris2LSP_Syntax_SimpleExpr_exprEquality(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getSpSigExpr($4), Pacillus_Idris2LSP_TypeRecons_TypeRecons_n__6295_4801_tycnst($0, $1, $2, $3));
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.5926:4489:assign */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_n__5926_4489_assign($0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a) {
 switch($0.h) {
  case 0: /* ExExArr */ return $9;
  case 1: /* SiExArr */ return Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($9, {h: 0 /* IdTerm */, a1: $0.a1.a1}, $a);
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.show */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_show_Show_ReconsTree($0) {
 switch($0.h) {
  case 0: /* Start */ return Pacillus_Idris2LSP_TypeRecons_TypeRecons_show_Show_PartialExprSignature($0.a1);
  case 1: /* Subgoal */ {
   const $4 = Prelude_Types_List_mapAppend({h: 0}, $8 => ('| '+$8), Prelude_Types_foldl_Foldable_List($e => $f => Prelude_Types_List_tailRecAppend($e, $f), {h: 0}, Prelude_Types_List_mapAppend({h: 0}, $18 => Data_String_lines(Pacillus_Idris2LSP_TypeRecons_TypeRecons_show_Show_ReconsTree($18)), $0.a1)));
   return Data_String_fastUnlines(Prelude_Types_List_tailRecAppend($4, {a1: '----------', a2: {a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_show_Show_PartialExprSignature($0.a2), a2: {h: 0}}}));
  }
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.show */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_show_Show_PartialExprSignature($0) {
 return (Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(0n, $0.a1)+(' : '+Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(0n, $0.a2)));
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.uniVarExprR : Constraints -> SimpleExpr -> SimpleExpr -> Either String Constraints */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_uniVarExprR($0, $1, $2) {
 return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_unify(Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignR($0, $2, $1)), unified => ({h: 1 /* Right */, a1: {a1: {a1: $1, a2: $2}, a2: unified}}));
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.uniVarExprL : Constraints -> SimpleExpr -> SimpleExpr -> Either String Constraints */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_uniVarExprL($0, $1, $2) {
 return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_unify(Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignL($0, $1, $2)), unified => ({h: 1 /* Right */, a1: {a1: {a1: $1, a2: $2}, a2: unified}}));
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.toSpSig : Signature -> PartialExprSignature */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_toSpSig($0) {
 return {a1: {h: 0 /* IdTerm */, a1: $0.a1}, a2: $0.a2};
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.showUniError : SimpleExpr -> SimpleExpr -> String */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_showUniError($0, $1) {
 return ('unification failed between '+(Pacillus_Idris2LSP_Syntax_SimpleExpr_show_Show_SimpleExpr($0)+(' and '+Pacillus_Idris2LSP_Syntax_SimpleExpr_show_Show_SimpleExpr($1))));
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.parseSignature : InOperatorMap -> List (WithBounds SimpleExprToken) -> Either String Signature */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_parseSignature($0, $1) {
 return Pacillus_Idris2LSP_TypeRecons_TypeRecons_case__parseSignature_3187($1, $0, Text_Parser_Core_parse(1, Pacillus_Idris2LSP_Syntax_SimpleExpr_signature(Pacillus_Idris2LSP_Syntax_SimpleExpr_opTable($0)), Prelude_Types_List_filterAppend({h: 0}, csegen_34(), $1)));
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.parseSig : InOperatorMap -> String -> Either String Signature */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_parseSig($0, $1) {
 return Pacillus_Idris2LSP_TypeRecons_TypeRecons_with__parseSig_3256($1, Pacillus_Idris2LSP_Syntax_Lexer_lexSimpleExpr($1), $0);
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.labelImplicitPr : List Identifier -> Pair -> Pair */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitPr($0, $1) {
 return {a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a1), a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a2)};
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.labelImplicitMain : List Identifier -> SimpleExpr -> SimpleExpr */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitMain($0, $1) {
 switch($1.h) {
  case 2: /* ArwTerm */ {
   switch($1.a1.h) {
    case 0: /* ExExArr */ {
     switch($1.a1.a1) {
      case 0: return {h: 2 /* ArwTerm */, a1: {h: 0 /* ExExArr */, a1: 0, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a1.a2), a3: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitMain($0, $1.a1.a3)}};
      default: return Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1);
     }
    }
    case 1: /* SiExArr */ {
     switch($1.a1.a1.h) {
      case undefined: /* cons */ return {h: 2 /* ArwTerm */, a1: {h: 1 /* SiExArr */, a1: {a1: $1.a1.a1.a1, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a1.a1.a2)}, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitMain({a1: $1.a1.a1.a1, a2: $0}, $1.a1.a2)}};
      default: return Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1);
     }
    }
    default: return Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1);
   }
  }
  default: return Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1);
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.labelImplicitExp : List Identifier -> SimpleExpr -> SimpleExpr */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1) {
 switch($1.h) {
  case 0: /* IdTerm */ {
   let $3;
   switch(Pacillus_Idris2LSP_TypeRecons_TypeRecons_isHeadLower($1.a1)) {
    case 1: {
     switch(Prelude_Types_elem(csegen_49(), {a1: $d => $e => Pacillus_Idris2LSP_Syntax_SimpleExpr_x3dx3d_Eq_Identifier($d, $e), a2: $13 => $14 => Pacillus_Idris2LSP_Syntax_SimpleExpr_x2fx3d_Eq_Identifier($13, $14)}, $1.a1, $0)) {
      case 1: {
       $3 = 0;
       break;
      }
      case 0: {
       $3 = 1;
       break;
      }
     }
     break;
    }
    case 0: {
     $3 = 0;
     break;
    }
   }
   switch($3) {
    case 1: return {h: 0 /* IdTerm */, a1: ('?'+$1.a1)};
    case 0: return {h: 0 /* IdTerm */, a1: $1.a1};
   }
  }
  case 1: /* AppTerm */ return {h: 1 /* AppTerm */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitApp($0, $1.a1)};
  case 2: /* ArwTerm */ return {h: 2 /* ArwTerm */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitArw($0, $1.a1)};
  case 3: /* EqTerm */ return {h: 3 /* EqTerm */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitEq($0, $1.a1)};
  case 8: /* PrTerm */ return {h: 8 /* PrTerm */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitPr($0, $1.a1)};
  case 4: /* IntegerLiteral */ return {h: 4 /* IntegerLiteral */, a1: $1.a1};
  case 5: /* DoubleLiteral */ return {h: 5 /* DoubleLiteral */, a1: $1.a1};
  case 6: /* CharLiteral */ return {h: 6 /* CharLiteral */, a1: $1.a1};
  case 7: /* StringLiteral */ return {h: 7 /* StringLiteral */, a1: $1.a1};
  case 9: /* UnitTerm */ return {h: 9 /* UnitTerm */};
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.labelImplicitEq : List Identifier -> Equality -> Equality */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitEq($0, $1) {
 return {a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a1), a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a2)};
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.labelImplicitArw : List Identifier -> Arrow False -> Arrow False */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitArw($0, $1) {
 switch($1.h) {
  case 0: /* ExExArr */ return {h: 0 /* ExExArr */, a1: 0, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a2), a3: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a3)};
  case 1: /* SiExArr */ return {h: 1 /* SiExArr */, a1: {a1: $1.a1.a1, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a1.a2)}, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a2)};
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.labelImplicitApp : List Identifier -> Application -> Application */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitApp($0, $1) {
 switch($1.a1.h) {
  case 0: /* IdTerm */ return {a1: {h: 0 /* IdTerm */, a1: $1.a1.a1}, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a2)};
  default: return {a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a1), a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitExp($0, $1.a2)};
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.labelImplicit : SimpleExpr -> SimpleExpr */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicit($0) {
 return Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicitMain({h: 0}, $0);
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.isImplicitVar : Identifier -> Bool */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_isImplicitVar($0) {
 const $1 = Prelude_Types_fastUnpack($0);
 switch($1.h) {
  case 0: /* nil */ return 0;
  case undefined: /* cons */ return Prelude_EqOrd_x3dx3d_Eq_Char($1.a1, '?');
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.isHeadLower : String -> Bool */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_isHeadLower($0) {
 const $1 = Prelude_Types_fastUnpack($0);
 switch($1.h) {
  case 0: /* nil */ return 0;
  case undefined: /* cons */ return Prelude_Types_isLower($1.a1);
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.getSubgoal : ReconsTree -> PartialExprSignature */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_getSubgoal($0) {
 switch($0.h) {
  case 0: /* Start */ return $0.a1;
  case 1: /* Subgoal */ return $0.a2;
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.getSpSigExpr : PartialExprSignature -> SimpleExpr */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_getSpSigExpr($0) {
 return $0.a2;
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.getPrType : List Signature -> Pair -> Either String ReconsTree */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_getPrType($0, $1) {
 return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getPartialType($0, $1.a1), xsig => Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getPartialType($0, $1.a2), ysig => ({h: 1 /* Right */, a1: {h: 1 /* Subgoal */, a1: {a1: xsig, a2: {a1: ysig, a2: {h: 0}}}, a2: {a1: {h: 8 /* PrTerm */, a1: {a1: $1.a1, a2: $1.a2}}, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_n__6112_4922_mkpair($1.a2, $1.a1, $0, xsig, ysig)}}})));
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.getPartialType : List Signature -> SimpleExpr -> Either String ReconsTree */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_getPartialType($0, $1) {
 switch($1.h) {
  case 0: /* IdTerm */ return Pacillus_Idris2LSP_TypeRecons_TypeRecons_getIdType($0, $1.a1);
  case 1: /* AppTerm */ return Pacillus_Idris2LSP_TypeRecons_TypeRecons_getAppType($0, $1.a1);
  case 2: /* ArwTerm */ return Pacillus_Idris2LSP_TypeRecons_TypeRecons_getArwType($0, $1.a1);
  case 3: /* EqTerm */ return Pacillus_Idris2LSP_TypeRecons_TypeRecons_getEqType($0, $1.a1);
  case 4: /* IntegerLiteral */ return {h: 1 /* Right */, a1: {h: 0 /* Start */, a1: {a1: {h: 0 /* IdTerm */, a1: Prelude_Show_show_Show_Integer($1.a1)}, a2: {h: 0 /* IdTerm */, a1: 'Integer'}}}};
  case 5: /* DoubleLiteral */ return {h: 1 /* Right */, a1: {h: 0 /* Start */, a1: {a1: {h: 0 /* IdTerm */, a1: Prelude_Show_show_Show_Double($1.a1)}, a2: {h: 0 /* IdTerm */, a1: 'Double'}}}};
  case 6: /* CharLiteral */ return {h: 1 /* Right */, a1: {h: 0 /* Start */, a1: {a1: {h: 0 /* IdTerm */, a1: Prelude_Show_show_Show_Char($1.a1)}, a2: {h: 0 /* IdTerm */, a1: 'Char'}}}};
  case 7: /* StringLiteral */ return {h: 1 /* Right */, a1: {h: 0 /* Start */, a1: {a1: {h: 0 /* IdTerm */, a1: Prelude_Show_show_Show_String($1.a1)}, a2: {h: 0 /* IdTerm */, a1: 'String'}}}};
  case 8: /* PrTerm */ return Pacillus_Idris2LSP_TypeRecons_TypeRecons_getPrType($0, $1.a1);
  case 9: /* UnitTerm */ {
   const $32 = {a1: {h: 0 /* IdTerm */, a1: 'MkUnit'}, a2: {h: 9 /* UnitTerm */}};
   return {h: 1 /* Right */, a1: {h: 0 /* Start */, a1: $32}};
  }
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.getEqType : List Signature -> Equality -> Either String ReconsTree */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_getEqType($0, $1) {
 return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getPartialType($0, $1.a1), lsig => Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getPartialType($0, $1.a2), rsig => ({h: 1 /* Right */, a1: {h: 1 /* Subgoal */, a1: {a1: lsig, a2: {a1: rsig, a2: {h: 0}}}, a2: {a1: {h: 3 /* EqTerm */, a1: {a1: $1.a1, a2: $1.a2}}, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_n__6104_4875_tycnst($1.a2, $1.a1, $0)}}})));
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.getArwType : List Signature -> Arrow False -> Either String ReconsTree */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_getArwType($0, $1) {
 return Pacillus_Idris2LSP_TypeRecons_TypeRecons_with__getArwType_4796($1, Pacillus_Idris2LSP_Syntax_SimpleExpr_forgetSig($1), $0);
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.getAppliedType : PartialExprSignature -> PartialExprSignature -> Either String PartialExprSignature */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_getAppliedType($0, $1) {
 return Pacillus_Idris2LSP_TypeRecons_TypeRecons_with__getAppliedType_4461($0.a2, Pacillus_Idris2LSP_TypeRecons_TypeRecons_labelImplicit($0.a2), $0.a1, $1);
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.getAppType : List Signature -> Application -> Either String ReconsTree */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_getAppType($0, $1) {
 return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getPartialType($0, $1.a1), ftree => Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getPartialType($0, $1.a2), xtree => Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getAppliedType(Pacillus_Idris2LSP_TypeRecons_TypeRecons_getSubgoal(ftree), Pacillus_Idris2LSP_TypeRecons_TypeRecons_getSubgoal(xtree)), appty => ({h: 1 /* Right */, a1: {h: 1 /* Subgoal */, a1: {a1: ftree, a2: {a1: xtree, a2: {h: 0}}}, a2: appty}}))));
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.assignSig : Signature -> SimpleExpr -> SimpleExpr -> Signature */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignSig($0, $1, $2) {
 return {a1: $0.a1, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a2, $1, $2)};
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.assignR : Constraints -> SimpleExpr -> SimpleExpr -> Constraints */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignR($0, $1, $2) {
 switch($0.h) {
  case 0: /* nil */ return {h: 0};
  case undefined: /* cons */ return {a1: {a1: $0.a1.a1, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a1.a2, $1, $2)}, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignR($0.a2, $1, $2)};
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.assignPr : Pair -> SimpleExpr -> SimpleExpr -> Pair */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignPr($0, $1, $2) {
 return {a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a1, $1, $2), a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a2, $1, $2)};
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.assignL : Constraints -> SimpleExpr -> SimpleExpr -> Constraints */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignL($0, $1, $2) {
 switch($0.h) {
  case 0: /* nil */ return {h: 0};
  case undefined: /* cons */ return {a1: {a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a1.a1, $1, $2), a2: $0.a1.a2}, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignL($0.a2, $1, $2)};
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.assignExpr : SimpleExpr -> SimpleExpr -> SimpleExpr -> SimpleExpr */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0, $1, $2) {
 switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_exprEquality($0, $1)) {
  case 1: return $2;
  case 0: {
   switch($0.h) {
    case 0: /* IdTerm */ return {h: 0 /* IdTerm */, a1: $0.a1};
    case 1: /* AppTerm */ return {h: 1 /* AppTerm */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignApp($0.a1, $1, $2)};
    case 2: /* ArwTerm */ return {h: 2 /* ArwTerm */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignArw($0.a1, $1, $2)};
    case 3: /* EqTerm */ return {h: 3 /* EqTerm */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignEq($0.a1, $1, $2)};
    case 4: /* IntegerLiteral */ return {h: 4 /* IntegerLiteral */, a1: $0.a1};
    case 5: /* DoubleLiteral */ return {h: 5 /* DoubleLiteral */, a1: $0.a1};
    case 6: /* CharLiteral */ return {h: 6 /* CharLiteral */, a1: $0.a1};
    case 7: /* StringLiteral */ return {h: 7 /* StringLiteral */, a1: $0.a1};
    case 8: /* PrTerm */ return {h: 8 /* PrTerm */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignPr($0.a1, $1, $2)};
    case 9: /* UnitTerm */ return {h: 9 /* UnitTerm */};
   }
  }
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.assignEq : Equality -> SimpleExpr -> SimpleExpr -> Equality */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignEq($0, $1, $2) {
 return {a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a1, $1, $2), a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a2, $1, $2)};
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.assignArw : Arrow b -> SimpleExpr -> SimpleExpr -> Arrow b */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignArw($0, $1, $2) {
 switch($0.h) {
  case 0: /* ExExArr */ return {h: 0 /* ExExArr */, a1: $0.a1, a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a2, $1, $2), a3: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a3, $1, $2)};
  case 1: /* SiExArr */ return {h: 1 /* SiExArr */, a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignSig($0.a1, $1, $2), a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a2, $1, $2)};
 }
}

/* Pacillus.Idris2LSP.TypeRecons.TypeRecons.assignApp : Application -> SimpleExpr -> SimpleExpr -> Application */
function Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignApp($0, $1, $2) {
 return {a1: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a1, $1, $2), a2: Pacillus_Idris2LSP_TypeRecons_TypeRecons_assignExpr($0.a2, $1, $2)};
}

/* Pacillus.Idris2LSP.Syntax.Lexer.tokValue */
function Pacillus_Idris2LSP_Syntax_Lexer_tokValue_TokenKind_SimpleExprTokenKind($0, $1) {
 switch($0) {
  case 0: return $1;
  case 1: return undefined;
  case 2: return undefined;
  case 3: return undefined;
  case 4: return $1;
  case 5: return undefined;
  case 6: return undefined;
  case 7: return undefined;
  case 8: return undefined;
  case 9: return undefined;
  case 10: return undefined;
  case 11: return Data_Maybe_fromMaybe(() => 0n, Data_String_parseInteger(csegen_17(), csegen_19(), $1));
  case 13: return Data_Maybe_fromMaybe(() => Number(0n), Data_String_parseDouble($1));
  case 12: {
   const $12 = Prelude_Types_fastUnpack($1);
   switch($12.h) {
    case 0: /* nil */ return '\0';
    case undefined: /* cons */ {
     switch($12.a2.h) {
      case 0: /* nil */ return '\0';
      case undefined: /* cons */ return $12.a2.a1;
     }
    }
   }
  }
  case 14: return _substr(1, _sub32s($1.length, 2), $1);
 }
}

/* Pacillus.Idris2LSP.Syntax.Lexer.show */
function Pacillus_Idris2LSP_Syntax_Lexer_show_Show_SimpleExprTokenKind($0) {
 switch($0) {
  case 0: return 'SESymbol';
  case 1: return 'SEIgnore';
  case 2: return 'SELParen';
  case 3: return 'SERParen';
  case 4: return 'SEIdentifier';
  case 5: return 'SEBackquote';
  case 6: return 'SEArrow';
  case 7: return 'SEEqual';
  case 8: return 'SEColon';
  case 9: return 'SEComma';
  case 10: return 'SEDollar';
  case 11: return 'SEIntLiteral';
  case 13: return 'SEDoubleLiteral';
  case 12: return 'SECharLiteral';
  case 14: return 'SEStringLiteral';
 }
}

/* Pacillus.Idris2LSP.Syntax.Lexer.show */
function Pacillus_Idris2LSP_Syntax_Lexer_show_Show_SimpleExprToken($0) {
 return ('Tok kind: '+(Pacillus_Idris2LSP_Syntax_Lexer_show_Show_SimpleExprTokenKind($0.a1)+(' text: '+$0.a2)));
}

/* Pacillus.Idris2LSP.Syntax.Lexer.showPrec */
function Pacillus_Idris2LSP_Syntax_Lexer_showPrec_Show_SimpleExprToken($0, $1) {
 return Pacillus_Idris2LSP_Syntax_Lexer_show_Show_SimpleExprToken($1);
}

/* Pacillus.Idris2LSP.Syntax.Lexer.TokType */
function Pacillus_Idris2LSP_Syntax_Lexer_TokType_TokenKind_SimpleExprTokenKind($0) {
 switch($0) {
  case 4: return {h: 'String'};
  case 0: return {h: 'String'};
  case 11: return {h: 'Integer'};
  case 13: return {h: 'Double'};
  case 12: return {h: 'Char'};
  case 14: return {h: 'String'};
  default: return {h: 'Builtin.Unit'};
 }
}

/* Pacillus.Idris2LSP.Syntax.Lexer.== */
function Pacillus_Idris2LSP_Syntax_Lexer_x3dx3d_Eq_SimpleExprTokenKind($0, $1) {
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
  case 3: {
   switch($1) {
    case 3: return 1;
    default: return 0;
   }
  }
  case 4: {
   switch($1) {
    case 4: return 1;
    default: return 0;
   }
  }
  case 5: {
   switch($1) {
    case 5: return 1;
    default: return 0;
   }
  }
  case 6: {
   switch($1) {
    case 6: return 1;
    default: return 0;
   }
  }
  case 7: {
   switch($1) {
    case 7: return 1;
    default: return 0;
   }
  }
  case 8: {
   switch($1) {
    case 8: return 1;
    default: return 0;
   }
  }
  case 9: {
   switch($1) {
    case 9: return 1;
    default: return 0;
   }
  }
  case 10: {
   switch($1) {
    case 10: return 1;
    default: return 0;
   }
  }
  case 11: {
   switch($1) {
    case 11: return 1;
    default: return 0;
   }
  }
  case 13: {
   switch($1) {
    case 13: return 1;
    default: return 0;
   }
  }
  case 12: {
   switch($1) {
    case 12: return 1;
    default: return 0;
   }
  }
  case 14: {
   switch($1) {
    case 14: return 1;
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Pacillus.Idris2LSP.Syntax.Lexer./= */
function Pacillus_Idris2LSP_Syntax_Lexer_x2fx3d_Eq_SimpleExprTokenKind($0, $1) {
 switch(Pacillus_Idris2LSP_Syntax_Lexer_x3dx3d_Eq_SimpleExprTokenKind($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Pacillus.Idris2LSP.Syntax.Lexer.symbolLexer : Lexer */
const Pacillus_Idris2LSP_Syntax_Lexer_symbolLexer = __lazy(function () {
 return Text_Lexer_some(Text_Lexer_Core_pred($4 => Pacillus_Idris2LSP_Syntax_Lexer_isOpChar($4)));
});

/* Pacillus.Idris2LSP.Syntax.Lexer.simpleExprTokenMap : TokenMap SimpleExprToken */
const Pacillus_Idris2LSP_Syntax_Lexer_simpleExprTokenMap = __lazy(function () {
 const $19 = s => {
  const $1a = Data_List_lookup(csegen_62(), s, Pacillus_Idris2LSP_Syntax_Lexer_reservedSyms());
  switch($1a.h) {
   case undefined: /* just */ return {a1: $1a.a1, a2: s};
   case 0: /* nothing */ return {a1: 0, a2: s};
  }
 };
 const $16 = {a1: Pacillus_Idris2LSP_Syntax_Lexer_symbolLexer(), a2: $19};
 const $15 = {a1: $16, a2: {h: 0}};
 const $13 = Prelude_Types_List_tailRecAppend($15, Text_Lexer_toTokenMap({a1: {a1: Text_Lexer_exact('('), a2: 2}, a2: {a1: {a1: Text_Lexer_exact(')'), a2: 3}, a2: {a1: {a1: Text_Lexer_exact('`'), a2: 5}, a2: {a1: {a1: Text_Lexer_exact(','), a2: 9}, a2: {a1: {a1: Text_Lexer_digits(), a2: 11}, a2: {a1: {a1: Pacillus_Idris2LSP_Syntax_Lexer_doubleLit(), a2: 13}, a2: {a1: {a1: Text_Lexer_charLit(), a2: 12}, a2: {a1: {a1: Text_Lexer_stringLit(), a2: 14}, a2: {h: 0}}}}}}}}}));
 const $9 = Prelude_Types_List_tailRecAppend(Text_Lexer_toTokenMap({a1: {a1: Pacillus_Idris2LSP_Syntax_Lexer_idLexer(), a2: 4}, a2: {h: 0}}), $13);
 return Prelude_Types_List_tailRecAppend(Text_Lexer_toTokenMap({a1: {a1: Text_Lexer_spaces(), a2: 1}, a2: {h: 0}}), $9);
});

/* Pacillus.Idris2LSP.Syntax.Lexer.reservedSyms : List (String, SimpleExprTokenKind) */
const Pacillus_Idris2LSP_Syntax_Lexer_reservedSyms = __lazy(function () {
 return {a1: {a1: '->', a2: 6}, a2: {a1: {a1: '=', a2: 7}, a2: {a1: {a1: ':', a2: 8}, a2: {a1: {a1: '$', a2: 10}, a2: {h: 0}}}}};
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
 return Prelude_Types_elem(csegen_49(), csegen_91(), $0, Prelude_Types_fastUnpack(':!#$%&*+./<=>?@\u{5c}^|-~'));
}

/* Pacillus.Idris2LSP.Syntax.Lexer.idLexer : Lexer */
const Pacillus_Idris2LSP_Syntax_Lexer_idLexer = __lazy(function () {
 return {h: 5 /* SeqEmpty */, a1: Text_Lexer_many({h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: Text_Lexer_alpha(), a2: () => Text_Lexer_many(Text_Lexer_alphaNum())}, a2: () => Text_Lexer_is('.')}), a2: Pacillus_Idris2LSP_Syntax_Lexer_nameLexer()};
});

/* Pacillus.Idris2LSP.Syntax.Lexer.doubleLit : Lexer */
const Pacillus_Idris2LSP_Syntax_Lexer_doubleLit = __lazy(function () {
 return {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: Text_Lexer_digits(), a2: () => Text_Lexer_is('.')}, a2: () => Text_Lexer_digits()}, a2: () => Text_Lexer_opt({h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: Text_Lexer_is('e'), a2: () => Text_Lexer_opt(Text_Lexer_Core_x3cx7cx3e(Text_Lexer_is('-'), Text_Lexer_is('+')))}, a2: () => Text_Lexer_digits()})};
});

/* Data.String.with block in parseInteger,parseIntTrimmed */
function Data_String_with__parseIntegerx2cparseIntTrimmed_9982($0, $1, $2, $3, $4, $5) {
 switch($4) {
  case '': {
   switch($5.h) {
    case 0: /* nil */ return {h: 0};
    default: {
     switch(Prelude_EqOrd_x3dx3d_Eq_Char($5.a1, '-')) {
      case 1: return Prelude_Types_map_Functor_Maybe(y => $2.a2($1.a3(y)), Data_String_parseNumWithoutSign(Prelude_Types_fastUnpack($5.a2), 0n));
      case 0: {
       switch(Prelude_EqOrd_x3dx3d_Eq_Char($5.a1, '+')) {
        case 1: return Prelude_Types_map_Functor_Maybe($21 => $1.a3($21), Data_String_parseNumWithoutSign(Prelude_Types_fastUnpack($5.a2), BigInt(0)));
        case 0: {
         let $2c;
         switch(Prelude_EqOrd_x3ex3d_Ord_Char($5.a1, '0')) {
          case 1: {
           $2c = Prelude_EqOrd_x3cx3d_Ord_Char($5.a1, '9');
           break;
          }
          case 0: {
           $2c = 0;
           break;
          }
         }
         switch($2c) {
          case 1: return Prelude_Types_map_Functor_Maybe($36 => $1.a3($36), Data_String_parseNumWithoutSign(Prelude_Types_fastUnpack($5.a2), BigInt(_sub32s(_truncInt32($5.a1.codePointAt(0)), _truncInt32('0'.codePointAt(0))))));
          case 0: return {h: 0};
         }
        }
       }
      }
     }
    }
   }
  }
  default: {
   switch(Prelude_EqOrd_x3dx3d_Eq_Char($5.a1, '-')) {
    case 1: return Prelude_Types_map_Functor_Maybe(y => $2.a2($1.a3(y)), Data_String_parseNumWithoutSign(Prelude_Types_fastUnpack($5.a2), 0n));
    case 0: {
     switch(Prelude_EqOrd_x3dx3d_Eq_Char($5.a1, '+')) {
      case 1: return Prelude_Types_map_Functor_Maybe($5e => $1.a3($5e), Data_String_parseNumWithoutSign(Prelude_Types_fastUnpack($5.a2), BigInt(0)));
      case 0: {
       let $69;
       switch(Prelude_EqOrd_x3ex3d_Ord_Char($5.a1, '0')) {
        case 1: {
         $69 = Prelude_EqOrd_x3cx3d_Ord_Char($5.a1, '9');
         break;
        }
        case 0: {
         $69 = 0;
         break;
        }
       }
       switch($69) {
        case 1: return Prelude_Types_map_Functor_Maybe($73 => $1.a3($73), Data_String_parseNumWithoutSign(Prelude_Types_fastUnpack($5.a2), BigInt(_sub32s(_truncInt32($5.a1.codePointAt(0)), _truncInt32('0'.codePointAt(0))))));
        case 0: return {h: 0};
       }
      }
     }
    }
   }
  }
 }
}

/* Data.String.with block in asList */
function Data_String_with__asList_9518($0, $1) {
 switch($0) {
  case '': {
   switch($1.h) {
    case 0: /* nil */ return {h: 0 /* Nil */};
    default: return {h: 1 /* :: */, a1: $1.a1, a2: $1.a2, a3: () => Data_String_asList($1.a2)};
   }
  }
  default: return {h: 1 /* :: */, a1: $1.a1, a2: $1.a2, a3: () => Data_String_asList($1.a2)};
 }
}

/* Data.String.case block in case block in parseDouble,wfe */
function Data_String_case__casex20blockx20inx20parseDoublex2cwfe_10493($0, $1, $2, $3) {
 switch($3.h) {
  case undefined: /* cons */ {
   switch($3.a1) {
    case '': {
     switch($3.a2.h) {
      case undefined: /* cons */ {
       switch($3.a2.a2.h) {
        case 0: /* nil */ return {h: 0};
        default: {
         switch($3.a2.h) {
          case undefined: /* cons */ {
           switch($3.a2.a2.h) {
            case 0: /* nil */ {
             const $17 = w => {
              const $2f = f => {
               const $38 = e => {
                let $3c;
                switch(Prelude_EqOrd_x3c_Ord_Double(w, Number(0n))) {
                 case 1: {
                  $3c = (-(f));
                  break;
                 }
                 case 0: {
                  $3c = f;
                  break;
                 }
                }
                const $3b = {a1: $3c, a2: e};
                const $39 = {a1: w, a2: $3b};
                return {a1: $39};
               };
               return Prelude_Types_x3ex3ex3d_Monad_Maybe(Data_String_parseInteger(csegen_17(), csegen_19(), $3.a2.a1), $38);
              };
              return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($1c => ($1c/Data_String_n__4756_10163_natpow(10.0, Prelude_Types_String_length($3.a1))), Prelude_Types_map_Functor_Maybe($27 => Number($27), Data_String_parseNumWithoutSign(Prelude_Types_fastUnpack($3.a1), 0n))), $2f);
             };
             return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($e => Number($e), Data_String_parseInteger(csegen_17(), csegen_19(), $1)), $17);
            }
            default: return {h: 0};
           }
          }
          case 0: /* nil */ {
           const $51 = w => {
            const $69 = f => {
             let $6d;
             switch(Prelude_EqOrd_x3c_Ord_Double(w, Number(0n))) {
              case 1: {
               $6d = (-(f));
               break;
              }
              case 0: {
               $6d = f;
               break;
              }
             }
             const $6c = {a1: $6d, a2: 0n};
             const $6a = {a1: w, a2: $6c};
             return {a1: $6a};
            };
            return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($56 => ($56/Data_String_n__4756_10163_natpow(10.0, Prelude_Types_String_length($3.a1))), Prelude_Types_map_Functor_Maybe($61 => Number($61), Data_String_parseNumWithoutSign(Prelude_Types_fastUnpack($3.a1), 0n))), $69);
           };
           return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($48 => Number($48), Data_String_parseInteger(csegen_17(), csegen_19(), $1)), $51);
          }
          default: return {h: 0};
         }
        }
       }
      }
      default: {
       switch($3.a2.h) {
        case undefined: /* cons */ {
         switch($3.a2.a2.h) {
          case 0: /* nil */ {
           const $84 = w => {
            const $9c = f => {
             const $a5 = e => {
              let $a9;
              switch(Prelude_EqOrd_x3c_Ord_Double(w, Number(0n))) {
               case 1: {
                $a9 = (-(f));
                break;
               }
               case 0: {
                $a9 = f;
                break;
               }
              }
              const $a8 = {a1: $a9, a2: e};
              const $a6 = {a1: w, a2: $a8};
              return {a1: $a6};
             };
             return Prelude_Types_x3ex3ex3d_Monad_Maybe(Data_String_parseInteger(csegen_17(), csegen_19(), $3.a2.a1), $a5);
            };
            return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($89 => ($89/Data_String_n__4756_10163_natpow(10.0, Prelude_Types_String_length($3.a1))), Prelude_Types_map_Functor_Maybe($94 => Number($94), Data_String_parseNumWithoutSign(Prelude_Types_fastUnpack($3.a1), 0n))), $9c);
           };
           return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($7b => Number($7b), Data_String_parseInteger(csegen_17(), csegen_19(), $1)), $84);
          }
          default: return {h: 0};
         }
        }
        case 0: /* nil */ {
         const $be = w => {
          const $d6 = f => {
           let $da;
           switch(Prelude_EqOrd_x3c_Ord_Double(w, Number(0n))) {
            case 1: {
             $da = (-(f));
             break;
            }
            case 0: {
             $da = f;
             break;
            }
           }
           const $d9 = {a1: $da, a2: 0n};
           const $d7 = {a1: w, a2: $d9};
           return {a1: $d7};
          };
          return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($c3 => ($c3/Data_String_n__4756_10163_natpow(10.0, Prelude_Types_String_length($3.a1))), Prelude_Types_map_Functor_Maybe($ce => Number($ce), Data_String_parseNumWithoutSign(Prelude_Types_fastUnpack($3.a1), 0n))), $d6);
         };
         return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($b5 => Number($b5), Data_String_parseInteger(csegen_17(), csegen_19(), $1)), $be);
        }
        default: return {h: 0};
       }
      }
     }
    }
    default: {
     switch($3.a2.h) {
      case undefined: /* cons */ {
       switch($3.a2.a2.h) {
        case 0: /* nil */ {
         const $f1 = w => {
          const $109 = f => {
           const $112 = e => {
            let $116;
            switch(Prelude_EqOrd_x3c_Ord_Double(w, Number(0n))) {
             case 1: {
              $116 = (-(f));
              break;
             }
             case 0: {
              $116 = f;
              break;
             }
            }
            const $115 = {a1: $116, a2: e};
            const $113 = {a1: w, a2: $115};
            return {a1: $113};
           };
           return Prelude_Types_x3ex3ex3d_Monad_Maybe(Data_String_parseInteger(csegen_17(), csegen_19(), $3.a2.a1), $112);
          };
          return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($f6 => ($f6/Data_String_n__4756_10163_natpow(10.0, Prelude_Types_String_length($3.a1))), Prelude_Types_map_Functor_Maybe($101 => Number($101), Data_String_parseNumWithoutSign(Prelude_Types_fastUnpack($3.a1), 0n))), $109);
         };
         return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($e8 => Number($e8), Data_String_parseInteger(csegen_17(), csegen_19(), $1)), $f1);
        }
        default: return {h: 0};
       }
      }
      case 0: /* nil */ {
       const $12b = w => {
        const $143 = f => {
         let $147;
         switch(Prelude_EqOrd_x3c_Ord_Double(w, Number(0n))) {
          case 1: {
           $147 = (-(f));
           break;
          }
          case 0: {
           $147 = f;
           break;
          }
         }
         const $146 = {a1: $147, a2: 0n};
         const $144 = {a1: w, a2: $146};
         return {a1: $144};
        };
        return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($130 => ($130/Data_String_n__4756_10163_natpow(10.0, Prelude_Types_String_length($3.a1))), Prelude_Types_map_Functor_Maybe($13b => Number($13b), Data_String_parseNumWithoutSign(Prelude_Types_fastUnpack($3.a1), 0n))), $143);
       };
       return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($122 => Number($122), Data_String_parseInteger(csegen_17(), csegen_19(), $1)), $12b);
      }
      default: return {h: 0};
     }
    }
   }
  }
  default: return {h: 0};
 }
}

/* Data.String.case block in parseDouble,wfe */
function Data_String_case__parseDoublex2cwfe_10330($0, $1) {
 switch($1.h) {
  case undefined: /* cons */ {
   switch($1.a2.h) {
    case 0: /* nil */ {
     const $4 = Data_String_split(csegen_104(), $1.a1);
     switch($4.h) {
      case undefined: /* cons */ {
       switch($4.a2.h) {
        case undefined: /* cons */ {
         switch($4.a2.a2.h) {
          case 0: /* nil */ return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($f => Number($f), Data_String_parseInteger(csegen_17(), csegen_19(), $4.a1)), w => Prelude_Types_x3ex3ex3d_Monad_Maybe(Data_String_parseInteger(csegen_17(), csegen_19(), $4.a2.a1), e => ({a1: {a1: w, a2: {a1: 0.0, a2: e}}})));
          default: return {h: 0};
         }
        }
        case 0: /* nil */ return Prelude_Types_x3ex3ex3d_Monad_Maybe(Prelude_Types_map_Functor_Maybe($2b => Number($2b), Data_String_parseInteger(csegen_17(), csegen_19(), $4.a1)), w => ({a1: {a1: w, a2: {a1: 0.0, a2: 0n}}}));
        default: return {h: 0};
       }
      }
      default: return {h: 0};
     }
    }
    case undefined: /* cons */ {
     switch($1.a2.a2.h) {
      case 0: /* nil */ return Data_String_case__casex20blockx20inx20parseDoublex2cwfe_10493($0, $1.a1, $1.a2.a1, Data_String_split(csegen_104(), $1.a2.a1));
      default: return {h: 0};
     }
    }
    default: return {h: 0};
   }
  }
  default: return {h: 0};
 }
}

/* Data.String.4756:10164:wfe */
function Data_String_n__4756_10164_wfe($0) {
 return Data_String_case__parseDoublex2cwfe_10330($0, Data_String_split($6 => Prelude_EqOrd_x3dx3d_Eq_Char($6, '.'), $0));
}

/* Data.String.3844:9250:unlines' */
function Data_String_n__3844_9250_unlinesx27($0) {
 switch($0.h) {
  case 0: /* nil */ return {h: 0};
  case undefined: /* cons */ return {a1: $0.a1, a2: {a1: '\n', a2: Data_String_n__3844_9250_unlinesx27($0.a2)}};
 }
}

/* Data.String.4552:9976:parseIntTrimmed */
function Data_String_n__4552_9976_parseIntTrimmed($0, $1, $2, $3) {
 return Data_String_with__parseIntegerx2cparseIntTrimmed_9982(undefined, $0, $1, $3, $3, Data_String_strM($3));
}

/* Data.String.4761:10174:num */
function Data_String_n__4761_10174_num($0, $1, $2, $3) {
 switch($3) {
  case 0n: return 1.0;
  default: {
   switch(Prelude_EqOrd_x3c_Ord_Integer($3, 0n)) {
    case 1: return (Number($2)*Data_String_n__4761_10174_num($0, $1, $2, ($3+1n)));
    case 0: return (Number($2)*Data_String_n__4761_10174_num($0, $1, $2, ($3-1n)));
   }
  }
 }
}

/* Data.String.4756:10163:natpow */
function Data_String_n__4756_10163_natpow($0, $1) {
 switch($1) {
  case 0n: return 1.0;
  default: {
   const $3 = ($1-1n);
   return ($0*Data_String_n__4756_10163_natpow($0, $3));
  }
 }
}

/* Data.String.4756:10162:mkDouble */
function Data_String_n__4756_10162_mkDouble($0) {
 switch($0.h) {
  case undefined: /* just */ {
   const $4 = Data_String_n__4756_10161_intPow(10n, $0.a1.a2.a2);
   return {a1: (($0.a1.a1*$4)+($0.a1.a2.a1*$4))};
  }
  case 0: /* nothing */ return {h: 0};
 }
}

/* Data.String.4756:10161:intPow */
function Data_String_n__4756_10161_intPow($0, $1) {
 switch(Prelude_EqOrd_x3e_Ord_Integer($1, 0n)) {
  case 1: return Data_String_n__4761_10174_num($1, $0, $0, $1);
  case 0: return (Number(1n)/Data_String_n__4761_10174_num($1, $0, $0, $1));
 }
}

/* Data.String.trim : String -> String */
function Data_String_trim($0) {
 return Data_String_ltrim(_strReverse(Data_String_ltrim(_strReverse($0))));
}

/* Data.String.strM : (x : String) -> StrM x */
function Data_String_strM($0) {
 switch($0) {
  case '': return {h: 0};
  default: return {a1: ($0.charAt(0)), a2: ($0.slice(1))};
 }
}

/* Data.String.split : (Char -> Bool) -> String -> List1 String */
function Data_String_split($0, $1) {
 return Data_List1_map_Functor_List1($4 => Prelude_Types_fastPack($4), Data_List_split($0, Prelude_Types_fastUnpack($1)));
}

/* Data.String.parseInteger : Num a => Neg a => String -> Maybe a */
function Data_String_parseInteger($0, $1, $2) {
 return Data_String_n__4552_9976_parseIntTrimmed($0, $1, $2, Data_String_trim($2));
}

/* Data.String.parseDouble : String -> Maybe Double */
function Data_String_parseDouble($0) {
 return Data_String_n__4756_10162_mkDouble(Data_String_n__4756_10164_wfe(Data_String_trim($0)));
}

/* Data.String.ltrim : String -> String */
function Data_String_ltrim($0) {
 return Data_String_with__ltrim_9542($0, Data_String_asList($0));
}

/* Data.String.lines' : List Char -> List (List Char) */
function Data_String_linesx27($0) {
 return Data_String_n__3977_9380_linesHelp($0, {h: 0}, $0);
}

/* Data.String.lines : String -> List String */
function Data_String_lines($0) {
 return Prelude_Types_List_mapAppend({h: 0}, $4 => Prelude_Types_fastPack($4), Data_String_linesx27(Prelude_Types_fastUnpack($0)));
}

/* Data.String.fastUnlines : List String -> String */
function Data_String_fastUnlines($0) {
 return Prelude_Types_fastConcat(Data_String_n__3844_9250_unlinesx27($0));
}

/* Data.String.asList : (str : String) -> AsList str */
function Data_String_asList($0) {
 return Data_String_with__asList_9518($0, Data_String_strM($0));
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

/* Prelude.Types.join */
function Prelude_Types_join_Monad_x28Eitherx20x24ex29($0) {
 return Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29($0, $4 => $4);
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

/* Prelude.Types.>>= */
function Prelude_Types_x3ex3ex3d_Monad_x28Eitherx20x24ex29($0, $1) {
 switch($0.h) {
  case 0: /* Left */ return {h: 0 /* Left */, a1: $0.a1};
  case 1: /* Right */ return $1($0.a1);
 }
}

/* Prelude.Types.>= */
function Prelude_Types_x3ex3d_Ord_Nat($0, $1) {
 return Prelude_EqOrd_x2fx3d_Eq_Ordering(Prelude_EqOrd_compare_Ord_Integer($0, $1), 0);
}

/* Prelude.Types.<|> */
function Prelude_Types_x3cx7cx3e_Alternative_Maybe($0, $1) {
 switch($0.h) {
  case undefined: /* just */ return {a1: $0.a1};
  case 0: /* nothing */ return $1();
 }
}

/* Prelude.Types.toUpper : Char -> Char */
function Prelude_Types_toUpper($0) {
 switch(Prelude_Types_isLower($0)) {
  case 1: return _truncToChar(_sub32s(_truncInt32($0.codePointAt(0)), 32));
  case 0: return $0;
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

/* Prelude.Types.isOctDigit : Char -> Bool */
function Prelude_Types_isOctDigit($0) {
 switch(Prelude_EqOrd_x3ex3d_Ord_Char($0, '0')) {
  case 1: return Prelude_EqOrd_x3cx3d_Ord_Char($0, '7');
  case 0: return 0;
 }
}

/* Prelude.Types.isLower : Char -> Bool */
function Prelude_Types_isLower($0) {
 switch(Prelude_EqOrd_x3ex3d_Ord_Char($0, 'a')) {
  case 1: return Prelude_EqOrd_x3cx3d_Ord_Char($0, 'z');
  case 0: return 0;
 }
}

/* Prelude.Types.isHexDigit : Char -> Bool */
function Prelude_Types_isHexDigit($0) {
 switch(Prelude_Types_isDigit($0)) {
  case 1: return 1;
  case 0: {
   let $4;
   switch(Prelude_EqOrd_x3cx3d_Ord_Char('a', $0)) {
    case 1: {
     $4 = Prelude_EqOrd_x3cx3d_Ord_Char($0, 'f');
     break;
    }
    case 0: {
     $4 = 0;
     break;
    }
   }
   switch($4) {
    case 1: return 1;
    case 0: {
     switch(Prelude_EqOrd_x3cx3d_Ord_Char('A', $0)) {
      case 1: return Prelude_EqOrd_x3cx3d_Ord_Char($0, 'F');
      case 0: return 0;
     }
    }
   }
  }
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

/* Prelude.Types.either : Lazy (a -> c) -> Lazy (b -> c) -> Either a b -> c */
function Prelude_Types_either($0, $1, $2) {
 switch($2.h) {
  case 0: /* Left */ return $0()($2.a1);
  case 1: /* Right */ return $1()($2.a1);
 }
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

/* Prelude.EqOrd.min */
function Prelude_EqOrd_min_Ord_Char($0, $1) {
 switch(Prelude_EqOrd_x3c_Ord_Char($0, $1)) {
  case 1: return $0;
  case 0: return $1;
 }
}

/* Prelude.EqOrd.min */
function Prelude_EqOrd_min_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3) {
 switch(Prelude_EqOrd_x3c_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3)) {
  case 1: return $2;
  case 0: return $3;
 }
}

/* Prelude.EqOrd.max */
function Prelude_EqOrd_max_Ord_Int($0, $1) {
 switch(Prelude_EqOrd_x3e_Ord_Int($0, $1)) {
  case 1: return $0;
  case 0: return $1;
 }
}

/* Prelude.EqOrd.max */
function Prelude_EqOrd_max_Ord_Char($0, $1) {
 switch(Prelude_EqOrd_x3e_Ord_Char($0, $1)) {
  case 1: return $0;
  case 0: return $1;
 }
}

/* Prelude.EqOrd.max */
function Prelude_EqOrd_max_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3) {
 switch(Prelude_EqOrd_x3e_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3)) {
  case 1: return $2;
  case 0: return $3;
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

/* Prelude.EqOrd.compare */
function Prelude_EqOrd_compare_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3) {
 switch($0.a1.a2($2.a1)($3.a1)) {
  case 1: return $0.a2($2.a1)($3.a1);
  case 0: return $1.a2($2.a2)($3.a2);
 }
}

/* Prelude.EqOrd.> */
function Prelude_EqOrd_x3e_Ord_Integer($0, $1) {
 switch((($0>$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.> */
function Prelude_EqOrd_x3e_Ord_Int($0, $1) {
 switch((($0>$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.> */
function Prelude_EqOrd_x3e_Ord_Char($0, $1) {
 switch((($0>$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.> */
function Prelude_EqOrd_x3e_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3) {
 return Prelude_EqOrd_x3dx3d_Eq_Ordering(Prelude_EqOrd_compare_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3), 2);
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
function Prelude_EqOrd_x3dx3d_Eq_Double($0, $1) {
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

/* Prelude.EqOrd.< */
function Prelude_EqOrd_x3c_Ord_Double($0, $1) {
 switch((($0<$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.< */
function Prelude_EqOrd_x3c_Ord_Char($0, $1) {
 switch((($0<$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.< */
function Prelude_EqOrd_x3c_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3) {
 return Prelude_EqOrd_x3dx3d_Eq_Ordering(Prelude_EqOrd_compare_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3), 0);
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

/* Prelude.Show.2437:11793:asciiTab */
function Prelude_Show_n__2437_11793_asciiTab($0) {
 return {a1: 'NUL', a2: {a1: 'SOH', a2: {a1: 'STX', a2: {a1: 'ETX', a2: {a1: 'EOT', a2: {a1: 'ENQ', a2: {a1: 'ACK', a2: {a1: 'BEL', a2: {a1: 'BS', a2: {a1: 'HT', a2: {a1: 'LF', a2: {a1: 'VT', a2: {a1: 'FF', a2: {a1: 'CR', a2: {a1: 'SO', a2: {a1: 'SI', a2: {a1: 'DLE', a2: {a1: 'DC1', a2: {a1: 'DC2', a2: {a1: 'DC3', a2: {a1: 'DC4', a2: {a1: 'NAK', a2: {a1: 'SYN', a2: {a1: 'ETB', a2: {a1: 'CAN', a2: {a1: 'EM', a2: {a1: 'SUB', a2: {a1: 'ESC', a2: {a1: 'FS', a2: {a1: 'GS', a2: {a1: 'RS', a2: {a1: 'US', a2: {h: 0}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}};
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_String($0) {
 return ('\"'+Prelude_Show_showLitString(Prelude_Types_fastUnpack($0))('\"'));
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_Integer($0) {
 return Prelude_Show_showPrec_Show_Integer({h: 0 /* Open */}, $0);
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_Int($0) {
 return Prelude_Show_showPrec_Show_Int({h: 0 /* Open */}, $0);
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_Double($0) {
 return Prelude_Show_showPrec_Show_Double({h: 0 /* Open */}, $0);
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_Char($0) {
 switch($0) {
  case '\'': return '\'\u{5c}\'\'';
  default: return ('\''+Prelude_Show_showLitChar($0)('\''));
 }
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_Bool($0) {
 switch($0) {
  case 1: return 'True';
  case 0: return 'False';
 }
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_x28Listx20x24ax29($0, $1) {
 return ('['+(Prelude_Show_n__3219_12514_showx27($0, $1, '', $1)+']'));
}

/* Prelude.Show.showPrec */
function Prelude_Show_showPrec_Show_Integer($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

/* Prelude.Show.showPrec */
function Prelude_Show_showPrec_Show_Int($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

/* Prelude.Show.showPrec */
function Prelude_Show_showPrec_Show_Double($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

/* Prelude.Show.showPrec */
function Prelude_Show_showPrec_Show_Bool($0, $1) {
 return Prelude_Show_show_Show_Bool($1);
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

/* Prelude.Show.showLitString : List Char -> String -> String */
function Prelude_Show_showLitString($0) {
 return $1 => {
  switch($0.h) {
   case 0: /* nil */ return $1;
   case undefined: /* cons */ {
    switch($0.a1) {
     case '\"': return ('\u{5c}\"'+Prelude_Show_showLitString($0.a2)($1));
     default: return Prelude_Show_showLitChar($0.a1)(Prelude_Show_showLitString($0.a2)($1));
    }
   }
  }
 };
}

/* Prelude.Show.showLitChar : Char -> String -> String */
function Prelude_Show_showLitChar($0) {
 switch($0) {
  case '\u{7}': return $2 => ('\u{5c}a'+$2);
  case '\u{8}': return $5 => ('\u{5c}b'+$5);
  case '\u{c}': return $8 => ('\u{5c}f'+$8);
  case '\n': return $b => ('\u{5c}n'+$b);
  case '\r': return $e => ('\u{5c}r'+$e);
  case '\u{9}': return $11 => ('\u{5c}t'+$11);
  case '\u{b}': return $14 => ('\u{5c}v'+$14);
  case '\u{e}': return $17 => Prelude_Show_protectEsc($1a => Prelude_EqOrd_x3dx3d_Eq_Char($1a, 'H'), '\u{5c}SO', $17);
  case '\u{7f}': return $20 => ('\u{5c}DEL'+$20);
  case '\u{5c}': return $23 => ('\u{5c}\u{5c}'+$23);
  default: {
   return $26 => {
    const $27 = Prelude_Types_getAt(Prelude_Types_prim__integerToNat(BigInt($0.codePointAt(0))), Prelude_Show_n__2437_11793_asciiTab($0));
    switch($27.h) {
     case undefined: /* just */ return ('\u{5c}'+($27.a1+$26));
     case 0: /* nothing */ {
      switch(Prelude_EqOrd_x3e_Ord_Char($0, '\u{7f}')) {
       case 1: return ('\u{5c}'+Prelude_Show_protectEsc($3c => Prelude_Types_isDigit($3c), Prelude_Show_show_Show_Int(_truncInt32($0.codePointAt(0))), $26));
       case 0: return ($0+$26);
      }
     }
    }
   };
  }
 }
}

/* Prelude.Show.showCon : Prec -> String -> String -> String */
function Prelude_Show_showCon($0, $1, $2) {
 return Prelude_Show_showParens(Prelude_Show_x3ex3d_Ord_Prec($0, {h: 6 /* App */}), ($1+$2));
}

/* Prelude.Show.showArg : Show a => a -> String */
function Prelude_Show_showArg($0, $1) {
 return (' '+$0.a2({h: 6 /* App */})($1));
}

/* Prelude.Show.protectEsc : (Char -> Bool) -> String -> String -> String */
function Prelude_Show_protectEsc($0, $1, $2) {
 let $5;
 switch(Prelude_Show_firstCharIs($0, $2)) {
  case 1: {
   $5 = '\u{5c}&';
   break;
  }
  case 0: {
   $5 = '';
   break;
  }
 }
 const $4 = ($5+$2);
 return ($1+$4);
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

/* Data.Maybe.fromMaybe : Lazy a -> Maybe a -> a */
function Data_Maybe_fromMaybe($0, $1) {
 switch($1.h) {
  case 0: /* nothing */ return $0();
  case undefined: /* just */ return $1.a1;
 }
}

/* Data.List1.show */
function Data_List1_show_Show_x28List1x20x24ax29($0, $1) {
 return Prelude_Show_show_Show_x28Listx20x24ax29($0, Data_List1_forget($1));
}

/* Data.List1.map */
function Data_List1_map_Functor_List1($0, $1) {
 return {a1: $0($1.a1), a2: Prelude_Types_List_mapAppend({h: 0}, $0, $1.a2)};
}

/* Data.List1.== */
function Data_List1_x3dx3d_Eq_x28List1x20x24ax29($0, $1, $2) {
 switch($0.a1($1.a1)($2.a1)) {
  case 1: return Prelude_Types_x3dx3d_Eq_x28Listx20x24ax29($0, $1.a2, $2.a2);
  case 0: return 0;
 }
}

/* Data.List1.singleton : a -> List1 a */
function Data_List1_singleton($0) {
 return {a1: $0, a2: {h: 0}};
}

/* Data.List1.last : List1 a -> a */
function Data_List1_last($0) {
 return Data_List1_n__3038_2661_loop($0.a1, $0.a2, $0.a1, $0.a2);
}

/* Data.List1.forget : List1 a -> List a */
function Data_List1_forget($0) {
 return {a1: $0.a1, a2: $0.a2};
}

/* Data.List1.cons : a -> List1 a -> List1 a */
function Data_List1_cons($0, $1) {
 return {a1: $0, a2: Data_List1_forget($1)};
}

/* Data.List1.appendl : List1 a -> List a -> List1 a */
function Data_List1_appendl($0, $1) {
 return {a1: $0.a1, a2: Prelude_Types_List_tailRecAppend($0.a2, $1)};
}

/* Data.List1.(++) : List1 a -> List1 a -> List1 a */
function Data_List1_x2bx2b($0, $1) {
 return Data_List1_appendl($0, Data_List1_forget($1));
}

/* Data.List.7824:8305:split */
function Data_List_n__7824_8305_split($0, $1, $2) {
 return Data_List_n__7824_8306_splitRec($0, $1, $2, $2, $9 => $9);
}

/* Data.List.8530:8993:go */
function Data_List_n__8530_8993_go($0, $1, $2, $3, $4) {
 switch($4.h) {
  case 0: /* nil */ return {a1: Data_List1_singleton($3), a2: {h: 0}};
  case undefined: /* cons */ {
   const $a = Data_List_n__8530_8993_go($0, $1, $2, $4.a1, $4.a2);
   switch($2($3)($4.a1)) {
    case 1: return {a1: Data_List1_cons($3, $a.a1), a2: $a.a2};
    case 0: return {a1: Data_List1_singleton($3), a2: {a1: $a.a1, a2: $a.a2}};
   }
  }
 }
}

/* Data.List.uncons' : List a -> Maybe (a, List a) */
function Data_List_unconsx27($0) {
 switch($0.h) {
  case 0: /* nil */ return {h: 0};
  case undefined: /* cons */ return {a1: {a1: $0.a1, a2: $0.a2}};
 }
}

/* Data.List.split : (a -> Bool) -> List a -> List1 (List a) */
function Data_List_split($0, $1) {
 const $2 = Data_List_break$($0, $1);
 switch($2.a2.h) {
  case 0: /* nil */ return Data_List1_singleton($2.a1);
  case undefined: /* cons */ return {a1: $2.a1, a2: Data_List1_forget(Data_List_split($0, $2.a2.a2))};
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

/* Data.List.sortBy : (a -> a -> Ordering) -> List a -> List a */
function Data_List_sortBy($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return {h: 0};
  case undefined: /* cons */ {
   switch($1.a2.h) {
    case 0: /* nil */ return {a1: $1.a1, a2: {h: 0}};
    default: {
     const $6 = Data_List_n__7824_8305_split($1, $0, $1);
     return Data_List_mergeBy($0, Data_List_sortBy($0, $6.a1), Data_List_sortBy($0, $6.a2));
    }
   }
  }
  default: {
   const $15 = Data_List_n__7824_8305_split($1, $0, $1);
   return Data_List_mergeBy($0, Data_List_sortBy($0, $15.a1), Data_List_sortBy($0, $15.a2));
  }
 }
}

/* Data.List.mergeBy : (a -> a -> Ordering) -> List a -> List a -> List a */
function Data_List_mergeBy($0, $1, $2) {
 switch($1.h) {
  case 0: /* nil */ return $2;
  default: {
   switch($2.h) {
    case 0: /* nil */ return $1;
    default: {
     switch($0($1.a1)($2.a1)) {
      case 0: return {a1: $1.a1, a2: Data_List_mergeBy($0, $1.a2, {a1: $2.a1, a2: $2.a2})};
      default: return {a1: $2.a1, a2: Data_List_mergeBy($0, {a1: $1.a1, a2: $1.a2}, $2.a2)};
     }
    }
   }
  }
 }
}

/* Data.List.lookup : Eq a => a -> List (a, b) -> Maybe b */
function Data_List_lookup($0, $1, $2) {
 return Data_List_lookupBy($5 => $6 => $0.a1($5)($6), $1, $2);
}

/* Data.List.head' : List a -> Maybe a */
function Data_List_headx27($0) {
 return Prelude_Types_map_Functor_Maybe($3 => Builtin_fst($3), Data_List_unconsx27($0));
}

/* Data.List.groupBy : (a -> a -> Bool) -> List a -> List (List1 a) */
function Data_List_groupBy($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return {h: 0};
  case undefined: /* cons */ {
   const $3 = Data_List_n__8530_8993_go($1.a1, $1.a2, $0, $1.a1, $1.a2);
   return {a1: $3.a1, a2: $3.a2};
  }
 }
}

/* Data.List.break : (a -> Bool) -> List a -> (List a, List a) */
function Data_List_break$($0, $1) {
 const $3 = $4 => {
  switch($0($4)) {
   case 1: return 0;
   case 0: return 1;
  }
 };
 return Data_List_span($3, $1);
}

/* Text.Lexer.4595:1473:lexStr */
function Text_Lexer_n__4595_1473_lexStr($0) {
 switch($0.h) {
  case 0: /* nil */ return Text_Lexer_Core_fail();
  case undefined: /* cons */ return Text_Lexer_Core_x3cx7cx3e(Text_Lexer_exact($0.a1), Text_Lexer_n__4595_1473_lexStr($0.a2));
 }
}

/* Text.Lexer.4595:1472:control */
const Text_Lexer_n__4595_1472_control = __lazy(function () {
 return Text_Lexer_Core_x3cx7cx3e(Text_Lexer_n__4595_1473_lexStr({a1: 'NUL', a2: {a1: 'SOH', a2: {a1: 'STX', a2: {a1: 'ETX', a2: {a1: 'EOT', a2: {a1: 'ENQ', a2: {a1: 'ACK', a2: {a1: 'BEL', a2: {a1: 'BS', a2: {a1: 'HT', a2: {a1: 'LF', a2: {a1: 'VT', a2: {a1: 'FF', a2: {a1: 'CR', a2: {a1: 'SO', a2: {a1: 'SI', a2: {a1: 'DLE', a2: {a1: 'DC1', a2: {a1: 'DC2', a2: {a1: 'DC3', a2: {a1: 'DC4', a2: {a1: 'NAK', a2: {a1: 'SYN', a2: {a1: 'ETB', a2: {a1: 'CAN', a2: {a1: 'EM', a2: {a1: 'SUB', a2: {a1: 'ESC', a2: {a1: 'FS', a2: {a1: 'GS', a2: {a1: 'RS', a2: {a1: 'US', a2: {a1: 'SP', a2: {a1: 'DEL', a2: {h: 0}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}), Text_Lexer_Core_x3cx7cx3e({h: 4 /* SeqEat */, a1: Text_Lexer_is('x'), a2: () => Text_Lexer_hexDigits()}, Text_Lexer_Core_x3cx7cx3e({h: 4 /* SeqEat */, a1: Text_Lexer_is('o'), a2: () => Text_Lexer_octDigits()}, Text_Lexer_digits())));
});

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

/* Text.Lexer.range : Char -> Char -> Lexer */
function Text_Lexer_range($0, $1) {
 const $3 = x => {
  switch(Prelude_EqOrd_x3ex3d_Ord_Char(x, Prelude_EqOrd_min_Ord_Char($0, $1))) {
   case 1: return Prelude_EqOrd_x3cx3d_Ord_Char(x, Prelude_EqOrd_max_Ord_Char($0, $1));
   case 0: return 0;
  }
 };
 return Text_Lexer_Core_pred($3);
}

/* Text.Lexer.quote : Lexer -> Lexer -> Lexer */
function Text_Lexer_quote($0, $1) {
 return Text_Lexer_surround($0, $0, $1);
}

/* Text.Lexer.opt : Lexer -> Recognise False */
function Text_Lexer_opt($0) {
 return Text_Lexer_Core_x3cx7cx3e($0, Text_Lexer_Core_empty());
}

/* Text.Lexer.oneOf : String -> Lexer */
function Text_Lexer_oneOf($0) {
 return Text_Lexer_Core_pred(x => Prelude_Types_elem(csegen_49(), csegen_91(), x, Prelude_Types_fastUnpack($0)));
}

/* Text.Lexer.octDigits : Lexer */
const Text_Lexer_octDigits = __lazy(function () {
 return Text_Lexer_some(Text_Lexer_octDigit());
});

/* Text.Lexer.octDigit : Lexer */
const Text_Lexer_octDigit = __lazy(function () {
 return Text_Lexer_Core_pred($2 => Prelude_Types_isOctDigit($2));
});

/* Text.Lexer.non : Lexer -> Lexer */
function Text_Lexer_non($0) {
 return {h: 5 /* SeqEmpty */, a1: Text_Lexer_Core_reject($0), a2: Text_Lexer_any()};
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

/* Text.Lexer.like : Char -> Lexer */
function Text_Lexer_like($0) {
 return Text_Lexer_Core_pred(y => Prelude_EqOrd_x3dx3d_Eq_Char(Prelude_Types_toUpper($0), Prelude_Types_toUpper(y)));
}

/* Text.Lexer.isNot : Char -> Lexer */
function Text_Lexer_isNot($0) {
 return Text_Lexer_Core_pred($3 => Prelude_EqOrd_x2fx3d_Eq_Char($3, $0));
}

/* Text.Lexer.is : Char -> Lexer */
function Text_Lexer_is($0) {
 return Text_Lexer_Core_pred($3 => Prelude_EqOrd_x3dx3d_Eq_Char($3, $0));
}

/* Text.Lexer.hexDigits : Lexer */
const Text_Lexer_hexDigits = __lazy(function () {
 return Text_Lexer_some(Text_Lexer_hexDigit());
});

/* Text.Lexer.hexDigit : Lexer */
const Text_Lexer_hexDigit = __lazy(function () {
 return Text_Lexer_Core_pred($2 => Prelude_Types_isHexDigit($2));
});

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

/* Text.Lexer.count : (q : Quantity) -> Lexer -> Recognise (isSucc (min q)) */
function Text_Lexer_count($0, $1) {
 switch($0.a1) {
  case 0n: {
   switch($0.a2.h) {
    case 0: /* nothing */ return Text_Lexer_many($1);
    case undefined: /* just */ {
     switch($0.a2.a1) {
      case 0n: return Text_Lexer_Core_empty();
      default: {
       const $9 = ($0.a2.a1-1n);
       return Text_Lexer_opt({h: 4 /* SeqEat */, a1: $1, a2: () => Text_Lexer_count(Text_Quantity_atMost($9), $1)});
      }
     }
    }
   }
  }
  default: {
   const $15 = ($0.a1-1n);
   switch($0.a2.h) {
    case 0: /* nothing */ return {h: 4 /* SeqEat */, a1: $1, a2: () => Text_Lexer_count(Text_Quantity_atLeast($15), $1)};
    case undefined: /* just */ {
     switch($0.a2.a1) {
      case 0n: return Text_Lexer_Core_fail();
      default: {
       const $22 = ($0.a2.a1-1n);
       return {h: 4 /* SeqEat */, a1: $1, a2: () => Text_Lexer_count(Text_Quantity_between($15, $22), $1)};
      }
     }
    }
   }
  }
 }
}

/* Text.Lexer.control : Lexer */
const Text_Lexer_control = __lazy(function () {
 return Text_Lexer_Core_pred($2 => Prelude_Types_isControl($2));
});

/* Text.Lexer.charLit : Lexer */
const Text_Lexer_charLit = __lazy(function () {
 return {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: Text_Lexer_is('\''), a2: () => Text_Lexer_Core_x3cx7cx3e(Text_Lexer_escape(Text_Lexer_is('\u{5c}'), Text_Lexer_Core_x3cx7cx3e(Text_Lexer_n__4595_1472_control(), Text_Lexer_any())), Text_Lexer_isNot('\''))}, a2: () => Text_Lexer_is('\'')};
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

/* Text.Quantity.exactly : Nat -> Quantity */
function Text_Quantity_exactly($0) {
 return {a1: $0, a2: {a1: $0}};
}

/* Text.Quantity.between : Nat -> Nat -> Quantity */
function Text_Quantity_between($0, $1) {
 return {a1: $0, a2: {a1: $1}};
}

/* Text.Quantity.atMost : Nat -> Quantity */
function Text_Quantity_atMost($0) {
 return {a1: 0n, a2: {a1: $0}};
}

/* Text.Quantity.atLeast : Nat -> Quantity */
function Text_Quantity_atLeast($0) {
 return {a1: $0, a2: {h: 0}};
}

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

/* Text.Bounded.show */
function Text_Bounded_show_Show_Bounds($0) {
 return Text_Bounded_showPrec_Show_Bounds({h: 0 /* Open */}, $0);
}

/* Text.Bounded.show */
function Text_Bounded_show_Show_x28WithBoundsx20x24tyx29($0, $1) {
 return Text_Bounded_showPrec_Show_x28WithBoundsx20x24tyx29($0, {h: 0 /* Open */}, $1);
}

/* Text.Bounded.showPrec */
function Text_Bounded_showPrec_Show_Bounds($0, $1) {
 return Prelude_Show_showCon($0, 'MkBounds', Prelude_Types_foldMap_Foldable_List(csegen_202(), $b => $b, {a1: Prelude_Show_showArg(csegen_204(), $1.a1), a2: {a1: Prelude_Show_showArg(csegen_204(), $1.a2), a2: {a1: Prelude_Show_showArg(csegen_204(), $1.a3), a2: {a1: Prelude_Show_showArg(csegen_204(), $1.a4), a2: {h: 0}}}}}));
}

/* Text.Bounded.showPrec */
function Text_Bounded_showPrec_Show_x28WithBoundsx20x24tyx29($0, $1, $2) {
 return Prelude_Show_showCon($1, 'MkBounded', Prelude_Types_foldMap_Foldable_List(csegen_202(), $c => $c, {a1: Prelude_Show_showArg({a1: x => Prelude_Show_show_Show_Bool(x), a2: d => x => Prelude_Show_showPrec_Show_Bool(d, x)}, $2.a2), a2: {a1: Prelude_Show_showArg($0, $2.a1), a2: {a1: Prelude_Show_showArg({a1: x => Text_Bounded_show_Show_Bounds(x), a2: d => x => Text_Bounded_showPrec_Show_Bounds(d, x)}, $2.a3), a2: {h: 0}}}}));
}

/* Text.Bounded.map */
function Text_Bounded_map_Functor_WithBounds($0, $1) {
 return {a1: $0($1.a1), a2: $1.a2, a3: $1.a3};
}

/* Text.Bounded.startBounds : Bounds -> (Int, Int) */
function Text_Bounded_startBounds($0) {
 return {a1: $0.a1, a2: $0.a2};
}

/* Text.Bounded.start : WithBounds ty -> (Int, Int) */
function Text_Bounded_start($0) {
 return Text_Bounded_startBounds($0.a3);
}

/* Text.Bounded.removeIrrelevance : WithBounds ty -> WithBounds ty */
function Text_Bounded_removeIrrelevance($0) {
 return {a1: $0.a1, a2: 1, a3: $0.a3};
}

/* Text.Bounded.mergeBounds : WithBounds ty -> WithBounds ty' -> WithBounds ty' */
function Text_Bounded_mergeBounds($0, $1) {
 switch($0.h) {
  case undefined: /* record */ {
   switch($0.a2) {
    case 1: {
     switch($1.h) {
      case undefined: /* record */ {
       switch($1.a2) {
        case 1: return Text_Bounded_irrelevantBounds($1.a1);
        default: return $1;
       }
      }
      default: return $1;
     }
    }
    default: {
     switch($1.h) {
      case undefined: /* record */ {
       switch($1.a2) {
        case 1: return Text_Bounded_map_Functor_WithBounds($c => $1.a1, $0);
        default: {
         const $e = Prelude_EqOrd_min_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_219(), csegen_219(), Text_Bounded_start($0), Text_Bounded_start($1));
         const $1a = Prelude_EqOrd_max_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_219(), csegen_219(), Text_Bounded_end($0), Text_Bounded_end($1));
         return {a1: $1.a1, a2: 0, a3: {a1: $e.a1, a2: $e.a2, a3: $1a.a1, a4: $1a.a2}};
        }
       }
      }
      default: {
       const $2e = Prelude_EqOrd_min_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_219(), csegen_219(), Text_Bounded_start($0), Text_Bounded_start($1));
       const $3a = Prelude_EqOrd_max_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_219(), csegen_219(), Text_Bounded_end($0), Text_Bounded_end($1));
       return {a1: $1.a1, a2: 0, a3: {a1: $2e.a1, a2: $2e.a2, a3: $3a.a1, a4: $3a.a2}};
      }
     }
    }
   }
  }
  default: {
   switch($1.h) {
    case undefined: /* record */ {
     switch($1.a2) {
      case 1: return Text_Bounded_map_Functor_WithBounds($52 => $1.a1, $0);
      default: {
       const $54 = Prelude_EqOrd_min_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_219(), csegen_219(), Text_Bounded_start($0), Text_Bounded_start($1));
       const $60 = Prelude_EqOrd_max_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_219(), csegen_219(), Text_Bounded_end($0), Text_Bounded_end($1));
       return {a1: $1.a1, a2: 0, a3: {a1: $54.a1, a2: $54.a2, a3: $60.a1, a4: $60.a2}};
      }
     }
    }
    default: {
     const $74 = Prelude_EqOrd_min_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_219(), csegen_219(), Text_Bounded_start($0), Text_Bounded_start($1));
     const $80 = Prelude_EqOrd_max_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_219(), csegen_219(), Text_Bounded_end($0), Text_Bounded_end($1));
     return {a1: $1.a1, a2: 0, a3: {a1: $74.a1, a2: $74.a2, a3: $80.a1, a4: $80.a2}};
    }
   }
  }
 }
}

/* Text.Bounded.irrelevantBounds : ty -> WithBounds ty */
function Text_Bounded_irrelevantBounds($0) {
 return {a1: $0, a2: 1, a3: {a1: -1, a2: -1, a3: -1, a4: -1}};
}

/* Text.Bounded.endBounds : Bounds -> (Int, Int) */
function Text_Bounded_endBounds($0) {
 return {a1: $0.a3, a2: $0.a4};
}

/* Text.Bounded.end : WithBounds ty -> (Int, Int) */
function Text_Bounded_end($0) {
 return Text_Bounded_endBounds($0.a3);
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.case block in parseSimpleExpr */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_case__parseSimpleExpr_5223($0, $1, $2) {
 switch($2.h) {
  case 1: /* Right */ {
   switch($2.a1.a2.h) {
    case 0: /* nil */ return {h: 1 /* Right */, a1: $2.a1.a1};
    default: return {h: 0 /* Left */, a1: Prelude_Show_show_Show_x28Listx20x24ax29(csegen_28(), $2.a1.a2)};
   }
  }
  case 0: /* Left */ return {h: 0 /* Left */, a1: Data_List1_show_Show_x28List1x20x24ax29(csegen_31(), $2.a1)};
 }
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.case block in == */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_case__x3dx3d_3371($0, $1, $2) {
 switch($2.a1.a2.h) {
  case 0: /* nil */ {
   switch($2.a2.a2.h) {
    case 0: /* nil */ return Prelude_EqOrd_x3dx3d_Eq_String($2.a1.a1, $2.a2.a1);
    case undefined: /* cons */ return Prelude_EqOrd_x3dx3d_Eq_String($2.a2.a1, Data_List1_last($2.a2));
   }
  }
  case undefined: /* cons */ {
   switch($2.a2.a2.h) {
    case 0: /* nil */ return Prelude_EqOrd_x3dx3d_Eq_String(Data_List1_last($2.a1), $2.a2.a1);
    case undefined: /* cons */ return Data_List1_x3dx3d_Eq_x28List1x20x24ax29(csegen_62(), $2.a1, $2.a2);
   }
  }
 }
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.5425:4148:sorting */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4148_sorting($0, $1, $2) {
 return Prelude_EqOrd_compare_Ord_Integer($2.a1.a1, $1.a1.a1);
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.5425:4147:sorted */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4147_sorted($0) {
 return Data_List_sortBy($3 => $4 => Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4148_sorting($0, $3, $4), Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4142_grouped($0));
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.5425:4146:norm_oprec */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4146_norm_oprec($0, $1) {
 return {a1: $1.a2, a2: {h: 2 /* Infix */, a1: Pacillus_Idris2LSP_Syntax_SimpleExpr_infixOperator($1.a1), a2: $1.a3}};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.5425:4145:norm_opmap */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4145_norm_opmap($0) {
 return Prelude_Types_List_mapAppend({h: 0}, $4 => Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4146_norm_oprec($0, $4), $0);
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.5425:4144:merged_norm */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4144_merged_norm($0) {
 return {a1: {a1: 1n, a2: {h: 2 /* Infix */, a1: Pacillus_Idris2LSP_Syntax_SimpleExpr_infixFunction(), a2: 0}}, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4145_norm_opmap($0)};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.5425:4143:grouping */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4143_grouping($0, $1, $2) {
 return (($1.a1===$2.a1)?1:0);
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.5425:4142:grouped */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4142_grouped($0) {
 return Data_List_groupBy($3 => $4 => Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4143_grouping($0, $3, $4), Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4144_merged_norm($0));
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.5425:4141:forgetAll */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4141_forgetAll($0, $1) {
 return Prelude_Types_List_mapAppend({h: 0}, $5 => Builtin_snd($5), Data_List1_forget($1));
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.show */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_show_Show_SimpleExpr($0) {
 return Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(0n, $0);
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.show */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_show_Show_Identifier($0) {
 return Data_List1_last(Data_String_split(csegen_222(), $0));
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.== */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_x3dx3d_Eq_Identifier($0, $1) {
 return Pacillus_Idris2LSP_Syntax_SimpleExpr_case__x3dx3d_3371($0, $1, {a1: Data_String_split(csegen_222(), $0), a2: Data_String_split(csegen_222(), $1)});
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr./= */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_x2fx3d_Eq_Identifier($0, $1) {
 switch(Pacillus_Idris2LSP_Syntax_SimpleExpr_x3dx3d_Eq_Identifier($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.term : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_term($0) {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: {h: 10 /* ThenEat */, a1: 1, a2: csegen_228(), a3: () => ({h: 10 /* ThenEat */, a1: 0, a2: csegen_229(), a3: () => ({h: 0 /* Empty */, a1: {h: 9 /* UnitTerm */}})})}, a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $14 => ({h: 8 /* PrTerm */, a1: $14}), Pacillus_Idris2LSP_Syntax_SimpleExpr_pair($0)), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: {h: 8 /* SeqEat */, a1: 0, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_identifier(), a3: () => id => ({h: 0 /* Empty */, a1: {h: 0 /* IdTerm */, a1: id}})}, a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: Pacillus_Idris2LSP_Syntax_SimpleExpr_literal(), a4: () => Pacillus_Idris2LSP_Syntax_SimpleExpr_paren($0)})})})};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.simpleExprInf2App : String -> SimpleExpr -> SimpleExpr -> SimpleExpr */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_simpleExprInf2App($0, $1, $2) {
 const $3 = {h: 0 /* IdTerm */, a1: $0};
 const $5 = {h: 1 /* AppTerm */, a1: {a1: $3, a2: $1}};
 return {h: 1 /* AppTerm */, a1: {a1: $5, a2: $2}};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.simpleExpr : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_simpleExpr($0) {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $7 => ({h: 2 /* ArwTerm */, a1: $7}), Pacillus_Idris2LSP_Syntax_SimpleExpr_arrow($0)), a4: () => Pacillus_Idris2LSP_Syntax_SimpleExpr_operation($0)};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.signature : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True Signature */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_signature($0) {
 return {h: 8 /* SeqEat */, a1: 1, a2: csegen_233(), a3: () => id => ({h: 10 /* ThenEat */, a1: 1, a2: Text_Parser_match(csegen_224(), csegen_227(), 8), a3: () => ({h: 8 /* SeqEat */, a1: 0, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_simpleExpr($0), a3: () => e => ({h: 0 /* Empty */, a1: {a1: id, a2: e}})})})};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.showSimpleExpr : Nat -> SimpleExpr -> String */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr($0, $1) {
 switch($1.h) {
  case 0: /* IdTerm */ return Pacillus_Idris2LSP_Syntax_SimpleExpr_show_Show_Identifier($1.a1);
  case 1: /* AppTerm */ return Pacillus_Idris2LSP_Syntax_SimpleExpr_showApp($0, $1.a1);
  case 2: /* ArwTerm */ return Pacillus_Idris2LSP_Syntax_SimpleExpr_showArw($0, $1.a1);
  case 3: /* EqTerm */ return Pacillus_Idris2LSP_Syntax_SimpleExpr_showEq($0, $1.a1);
  case 8: /* PrTerm */ return Pacillus_Idris2LSP_Syntax_SimpleExpr_showPr($0, $1.a1);
  case 4: /* IntegerLiteral */ return Prelude_Show_show_Show_Integer($1.a1);
  case 5: /* DoubleLiteral */ return Prelude_Show_show_Show_Double($1.a1);
  case 6: /* CharLiteral */ return Prelude_Show_show_Show_Char($1.a1);
  case 7: /* StringLiteral */ return Prelude_Show_show_Show_String($1.a1);
  case 9: /* UnitTerm */ return '()';
 }
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.showPrNoBracket : Pair -> String */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_showPrNoBracket($0) {
 switch($0.a2.h) {
  case 8: /* PrTerm */ return (Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(0n, $0.a1)+(', '+Pacillus_Idris2LSP_Syntax_SimpleExpr_showPrNoBracket($0.a2.a1)));
  default: return (Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(0n, $0.a1)+(', '+Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(0n, $0.a2)));
 }
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.showPr : Nat -> Pair -> String */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_showPr($0, $1) {
 return ('('+(Pacillus_Idris2LSP_Syntax_SimpleExpr_showPrNoBracket($1)+')'));
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.showEq : Nat -> Equality -> String */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_showEq($0, $1) {
 return Prelude_Show_showParens(Prelude_Types_x3ex3d_Ord_Nat($0, 1n), (Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(1n, $1.a1)+(' = '+Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(1n, $1.a2))));
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.showArw : Nat -> Arrow False -> String */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_showArw($0, $1) {
 switch($1.h) {
  case 0: /* ExExArr */ return Prelude_Show_showParens(Prelude_Types_x3ex3d_Ord_Nat($0, 2n), (Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(2n, $1.a2)+(' -> '+Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(1n, $1.a3))));
  case 1: /* SiExArr */ return Prelude_Show_showParens(Prelude_Types_x3ex3d_Ord_Nat($0, 2n), ('('+(Pacillus_Idris2LSP_Syntax_SimpleExpr_show_Show_Identifier($1.a1.a1)+(' : '+(Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(0n, $1.a1.a2)+(') -> '+Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(2n, $1.a2)))))));
 }
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.showApp : Nat -> Application -> String */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_showApp($0, $1) {
 return Prelude_Show_showParens(Prelude_Types_x3ex3d_Ord_Nat($0, 3n), (Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(2n, $1.a1)+(' '+Pacillus_Idris2LSP_Syntax_SimpleExpr_showSimpleExpr(3n, $1.a2))));
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.parseSimpleExpr : InOperatorMap -> List (WithBounds SimpleExprToken) -> Either String SimpleExpr */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_parseSimpleExpr($0, $1) {
 return Pacillus_Idris2LSP_Syntax_SimpleExpr_case__parseSimpleExpr_5223($1, $0, Text_Parser_Core_parse(1, Pacillus_Idris2LSP_Syntax_SimpleExpr_simpleExpr(Pacillus_Idris2LSP_Syntax_SimpleExpr_opTable($0)), Prelude_Types_List_filterAppend({h: 0}, csegen_34(), $1)));
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.parse : InOperatorMap -> String -> Either String SimpleExpr */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_parse($0, $1) {
 const $2 = Pacillus_Idris2LSP_Syntax_Lexer_lexSimpleExpr($1);
 switch($2.h) {
  case undefined: /* just */ return Pacillus_Idris2LSP_Syntax_SimpleExpr_parseSimpleExpr($0, $2.a1);
  case 0: /* nothing */ return {h: 0 /* Left */, a1: 'Failed to lex.'};
 }
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.paren : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_paren($0) {
 return {h: 10 /* ThenEat */, a1: 1, a2: csegen_228(), a3: () => ({h: 8 /* SeqEat */, a1: 1, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_simpleExpr($0), a3: () => e => ({h: 10 /* ThenEat */, a1: 0, a2: csegen_229(), a3: () => ({h: 0 /* Empty */, a1: e})})})};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.pairSub : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True Pair */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_pairSub($0) {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: {h: 8 /* SeqEat */, a1: 1, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_simpleExpr($0), a3: () => e => ({h: 10 /* ThenEat */, a1: 1, a2: csegen_236(), a3: () => ({h: 8 /* SeqEat */, a1: 0, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_pairSub($0), a3: () => p => ({h: 0 /* Empty */, a1: {a1: e, a2: {h: 8 /* PrTerm */, a1: p}}})})})}, a4: () => ({h: 8 /* SeqEat */, a1: 1, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_simpleExpr($0), a3: () => e1 => ({h: 10 /* ThenEat */, a1: 1, a2: csegen_236(), a3: () => ({h: 8 /* SeqEat */, a1: 0, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_simpleExpr($0), a3: () => e2 => ({h: 0 /* Empty */, a1: {a1: e1, a2: e2}})})})})};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.pair : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True Pair */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_pair($0) {
 return {h: 10 /* ThenEat */, a1: 1, a2: csegen_228(), a3: () => ({h: 8 /* SeqEat */, a1: 1, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_pairSub($0), a3: () => p => ({h: 10 /* ThenEat */, a1: 0, a2: csegen_229(), a3: () => ({h: 0 /* Empty */, a1: p})})})};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.operation : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_operation($0) {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: Text_Parser_Expression_buildExpressionParser(Prelude_Types_List_tailRecAppend($0, {a1: {a1: {h: 2 /* Infix */, a1: Pacillus_Idris2LSP_Syntax_SimpleExpr_equality(), a2: 0}, a2: {a1: {h: 2 /* Infix */, a1: Pacillus_Idris2LSP_Syntax_SimpleExpr_appOp(), a2: 2}, a2: {h: 0}}}, a2: {h: 0}}), {h: 12 /* Alt */, a1: 1, a2: 1, a3: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $1c => ({h: 1 /* AppTerm */, a1: $1c}), Pacillus_Idris2LSP_Syntax_SimpleExpr_app($0)), a4: () => Pacillus_Idris2LSP_Syntax_SimpleExpr_term($0)}), a4: () => Pacillus_Idris2LSP_Syntax_SimpleExpr_term($0)};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.opTable : InOperatorMap -> OperatorTable state SimpleExprToken SimpleExpr */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_opTable($0) {
 return Pacillus_Idris2LSP_Syntax_SimpleExpr_dynOperatorTable($0);
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.literal : Grammar state SimpleExprToken True SimpleExpr */
const Pacillus_Idris2LSP_Syntax_SimpleExpr_literal = __lazy(function () {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: {h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_match(csegen_224(), csegen_227(), 11), a3: () => n => ({h: 0 /* Empty */, a1: {h: 4 /* IntegerLiteral */, a1: n}})}, a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: {h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_match(csegen_224(), csegen_227(), 13), a3: () => n => ({h: 0 /* Empty */, a1: {h: 5 /* DoubleLiteral */, a1: n}})}, a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: {h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_match(csegen_224(), csegen_227(), 12), a3: () => c => ({h: 0 /* Empty */, a1: {h: 6 /* CharLiteral */, a1: c}})}, a4: () => ({h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_match(csegen_224(), csegen_227(), 14), a3: () => s => ({h: 0 /* Empty */, a1: {h: 7 /* StringLiteral */, a1: s}})})})})};
});

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.infixOperator : String -> Grammar state SimpleExprToken True (SimpleExpr -> SimpleExpr -> SimpleExpr) */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_infixOperator($0) {
 return {h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_match(csegen_224(), csegen_227(), 0), a3: () => sym => ({h: 11 /* ThenEmpty */, a1: 0, a2: 0, a3: Text_Parser_when(Prelude_EqOrd_x2fx3d_Eq_String(sym, $0), () => ({h: 4 /* Fail */, a1: {h: 0}, a2: 0, a3: 'not a matching operator'})), a4: {h: 0 /* Empty */, a1: $18 => $19 => Pacillus_Idris2LSP_Syntax_SimpleExpr_simpleExprInf2App(('('+(sym+')')), $18, $19)}})};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.infixFunction : Grammar state SimpleExprToken True (SimpleExpr -> SimpleExpr -> SimpleExpr) */
const Pacillus_Idris2LSP_Syntax_SimpleExpr_infixFunction = __lazy(function () {
 return {h: 10 /* ThenEat */, a1: 1, a2: csegen_253(), a3: () => ({h: 8 /* SeqEat */, a1: 1, a2: csegen_233(), a3: () => id => ({h: 10 /* ThenEat */, a1: 0, a2: csegen_253(), a3: () => ({h: 0 /* Empty */, a1: $d => $e => Pacillus_Idris2LSP_Syntax_SimpleExpr_simpleExprInf2App(id, $d, $e)})})})};
});

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.ignored : WithBounds SimpleExprToken -> Bool */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_ignored($0) {
 switch($0.h) {
  case undefined: /* record */ {
   switch($0.a1.h) {
    case undefined: /* cons */ {
     switch($0.a1.a1) {
      case 1: return 1;
      default: return 0;
     }
    }
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.identifier : Grammar state SimpleExprToken True Identifier */
const Pacillus_Idris2LSP_Syntax_SimpleExpr_identifier = __lazy(function () {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $6 => $6, csegen_233()), a4: () => ({h: 10 /* ThenEat */, a1: 1, a2: csegen_228(), a3: () => ({h: 8 /* SeqEat */, a1: 1, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_identifier(), a3: () => id => ({h: 10 /* ThenEat */, a1: 0, a2: csegen_229(), a3: () => ({h: 0 /* Empty */, a1: id})})})})};
});

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.forgetSig : Arrow b -> Arrow True */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_forgetSig($0) {
 switch($0.h) {
  case 0: /* ExExArr */ return {h: 0 /* ExExArr */, a1: 1, a2: $0.a2, a3: $0.a3};
  case 1: /* SiExArr */ return {h: 0 /* ExExArr */, a1: 1, a2: $0.a1.a2, a3: $0.a2};
 }
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.equality : Grammar state SimpleExprToken True (SimpleExpr -> SimpleExpr -> SimpleExpr) */
const Pacillus_Idris2LSP_Syntax_SimpleExpr_equality = __lazy(function () {
 return {h: 10 /* ThenEat */, a1: 0, a2: Text_Parser_match(csegen_224(), csegen_227(), 7), a3: () => ({h: 0 /* Empty */, a1: x => y => ({h: 3 /* EqTerm */, a1: {a1: x, a2: y}})})};
});

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.dynOperatorTable : InOperatorMap -> OperatorTable state SimpleExprToken SimpleExpr */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_dynOperatorTable($0) {
 return Prelude_Types_List_mapAppend({h: 0}, $4 => Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4141_forgetAll($0, $4), Pacillus_Idris2LSP_Syntax_SimpleExpr_n__5425_4147_sorted($0));
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.arrow : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True (Arrow False) */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_arrow($0) {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: {h: 8 /* SeqEat */, a1: 1, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_operation($0), a3: () => e1 => ({h: 10 /* ThenEat */, a1: 1, a2: csegen_266(), a3: () => ({h: 8 /* SeqEat */, a1: 0, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_simpleExpr($0), a3: () => e2 => ({h: 0 /* Empty */, a1: {h: 0 /* ExExArr */, a1: 0, a2: e1, a3: e2}})})})}, a4: () => ({h: 10 /* ThenEat */, a1: 1, a2: csegen_228(), a3: () => ({h: 8 /* SeqEat */, a1: 1, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_signature($0), a3: () => sig => ({h: 10 /* ThenEat */, a1: 1, a2: csegen_229(), a3: () => ({h: 10 /* ThenEat */, a1: 1, a2: csegen_266(), a3: () => ({h: 8 /* SeqEat */, a1: 0, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_simpleExpr($0), a3: () => e => ({h: 0 /* Empty */, a1: {h: 1 /* SiExArr */, a1: sig, a2: e}})})})})})})};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.appSub2 : OperatorTable state SimpleExprToken SimpleExpr -> Application -> Grammar state SimpleExprToken True Application */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_appSub2($0, $1) {
 return {h: 8 /* SeqEat */, a1: 0, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_term($0), a3: () => t => Pacillus_Idris2LSP_Syntax_SimpleExpr_appSub1($0, {a1: {h: 1 /* AppTerm */, a1: $1}, a2: t})};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.appSub1 : OperatorTable state SimpleExprToken SimpleExpr -> Application -> Grammar state SimpleExprToken False Application */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_appSub1($0, $1) {
 return {h: 12 /* Alt */, a1: 1, a2: 0, a3: Pacillus_Idris2LSP_Syntax_SimpleExpr_appSub2($0, $1), a4: () => ({h: 0 /* Empty */, a1: $1})};
}

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.appOp : Grammar state SimpleExprToken True (SimpleExpr -> SimpleExpr -> SimpleExpr) */
const Pacillus_Idris2LSP_Syntax_SimpleExpr_appOp = __lazy(function () {
 return {h: 10 /* ThenEat */, a1: 0, a2: Text_Parser_match(csegen_224(), csegen_227(), 10), a3: () => ({h: 0 /* Empty */, a1: x => y => ({h: 1 /* AppTerm */, a1: {a1: x, a2: y}})})};
});

/* Pacillus.Idris2LSP.Syntax.SimpleExpr.app : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True Application */
function Pacillus_Idris2LSP_Syntax_SimpleExpr_app($0) {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: {h: 8 /* SeqEat */, a1: 1, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_identifier(), a3: () => id => ({h: 8 /* SeqEat */, a1: 0, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_term($0), a3: () => t => Pacillus_Idris2LSP_Syntax_SimpleExpr_appSub1($0, {a1: {h: 0 /* IdTerm */, a1: id}, a2: t})})}, a4: () => ({h: 8 /* SeqEat */, a1: 1, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_paren($0), a3: () => a => ({h: 8 /* SeqEat */, a1: 0, a2: Pacillus_Idris2LSP_Syntax_SimpleExpr_term($0), a3: () => t => Pacillus_Idris2LSP_Syntax_SimpleExpr_appSub1($0, {a1: a, a2: t})})})};
}

/* Text.Parser.Expression.case block in buildExpressionParser,level,parseThese */
function Text_Parser_Expression_case__buildExpressionParserx2clevelx2cparseThese_3953($0, $1, $2, $3, $4) {
 return {h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_Expression_n__3811_3993_termP($0, $1, $2, $3, $4.a1, $4.a2.a1, $4.a2.a2.a1, $4.a2.a2.a2.a1, $4.a2.a2.a2.a2), a3: () => x => ({h: 12 /* Alt */, a1: 1, a2: 0, a3: Text_Parser_Expression_n__3811_3991_rassocP($0, $1, $2, $3, $4.a1, $4.a2.a1, $4.a2.a2.a1, $4.a2.a2.a2.a1, $4.a2.a2.a2.a2, x), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 0, a3: Text_Parser_Expression_n__3811_3986_lassocP($0, $1, $2, $3, $4.a1, $4.a2.a1, $4.a2.a2.a1, $4.a2.a2.a2.a1, $4.a2.a2.a2.a2, x), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 0, a3: Text_Parser_Expression_n__3811_3988_nassocP($0, $1, $2, $3, $4.a1, $4.a2.a1, $4.a2.a2.a1, $4.a2.a2.a2.a1, $4.a2.a2.a2.a2, x), a4: () => ({h: 0 /* Empty */, a1: x})})})})};
}

/* Text.Parser.Expression.3811:3993:termP */
function Text_Parser_Expression_n__3811_3993_termP($0, $1, $2, $3, $4, $5, $6, $7, $8) {
 return {h: 9 /* SeqEmpty */, a1: 0, a2: 1, a3: Text_Parser_Expression_n__3811_3990_prefixP($0, $1, $2, $3, $4, $5, $6, $7, $8), a4: f => ({h: 8 /* SeqEat */, a1: 0, a2: $3, a3: () => x => ({h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: Text_Parser_Expression_n__3811_3989_postfixP($0, $1, $2, $3, $4, $5, $6, $7, $8), a4: g => ({h: 0 /* Empty */, a1: g(f(x))})})})};
}

/* Text.Parser.Expression.3459:3628:sortedOps */
function Text_Parser_Expression_n__3459_3628_sortedOps($0, $1, $2, $3) {
 return Prelude_Types_foldr_Foldable_List($6 => $7 => Text_Parser_Expression_n__3459_3627_separate($0, $1, $2, $3, $6, $7), {a1: {h: 0}, a2: {a1: {h: 0}, a2: {a1: {h: 0}, a2: {a1: {h: 0}, a2: {h: 0}}}}}, $2);
}

/* Text.Parser.Expression.3459:3627:separate */
function Text_Parser_Expression_n__3459_3627_separate($0, $1, $2, $3, $4, $5) {
 switch($4.h) {
  case 2: /* Infix */ {
   switch($4.a2) {
    case 1: return {a1: {a1: $4.a1, a2: $5.a1}, a2: {a1: $5.a2.a1, a2: {a1: $5.a2.a2.a1, a2: {a1: $5.a2.a2.a2.a1, a2: $5.a2.a2.a2.a2}}}};
    case 2: return {a1: $5.a1, a2: {a1: {a1: $4.a1, a2: $5.a2.a1}, a2: {a1: $5.a2.a2.a1, a2: {a1: $5.a2.a2.a2.a1, a2: $5.a2.a2.a2.a2}}}};
    case 0: return {a1: $5.a1, a2: {a1: $5.a2.a1, a2: {a1: {a1: $4.a1, a2: $5.a2.a2.a1}, a2: {a1: $5.a2.a2.a2.a1, a2: $5.a2.a2.a2.a2}}}};
   }
  }
  case 0: /* Prefix */ return {a1: $5.a1, a2: {a1: $5.a2.a1, a2: {a1: $5.a2.a2.a1, a2: {a1: {a1: $4.a1, a2: $5.a2.a2.a2.a1}, a2: $5.a2.a2.a2.a2}}}};
  case 1: /* Postfix */ return {a1: $5.a1, a2: {a1: $5.a2.a1, a2: {a1: $5.a2.a2.a1, a2: {a1: $5.a2.a2.a2.a1, a2: {a1: $4.a1, a2: $5.a2.a2.a2.a2}}}}};
 }
}

/* Text.Parser.Expression.3811:3992:rassocP1 */
function Text_Parser_Expression_n__3811_3992_rassocP1($0, $1, $2, $3, $4, $5, $6, $7, $8, $9) {
 return {h: 12 /* Alt */, a1: 1, a2: 0, a3: Text_Parser_Expression_n__3811_3991_rassocP($0, $1, $2, $3, $4, $5, $6, $7, $8, $9), a4: () => ({h: 0 /* Empty */, a1: $9})};
}

/* Text.Parser.Expression.3811:3991:rassocP */
function Text_Parser_Expression_n__3811_3991_rassocP($0, $1, $2, $3, $4, $5, $6, $7, $8, $9) {
 return {h: 8 /* SeqEat */, a1: 1, a2: Text_Parser_choice(csegen_49(), 1, $5), a3: () => f => ({h: 8 /* SeqEat */, a1: 0, a2: {h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_Expression_n__3811_3993_termP($0, $1, $2, $3, $4, $5, $6, $7, $8), a3: () => $21 => Text_Parser_Expression_n__3811_3992_rassocP1($0, $1, $2, $3, $4, $5, $6, $7, $8, $21)}, a3: () => y => ({h: 0 /* Empty */, a1: f($9)(y)})})};
}

/* Text.Parser.Expression.3811:3990:prefixP */
function Text_Parser_Expression_n__3811_3990_prefixP($0, $1, $2, $3, $4, $5, $6, $7, $8) {
 return {h: 12 /* Alt */, a1: 1, a2: 0, a3: Text_Parser_choice(csegen_49(), 1, $7), a4: () => ({h: 0 /* Empty */, a1: $13 => $13})};
}

/* Text.Parser.Expression.3811:3989:postfixP */
function Text_Parser_Expression_n__3811_3989_postfixP($0, $1, $2, $3, $4, $5, $6, $7, $8) {
 return {h: 12 /* Alt */, a1: 1, a2: 0, a3: Text_Parser_choice(csegen_49(), 1, $8), a4: () => ({h: 0 /* Empty */, a1: $13 => $13})};
}

/* Text.Parser.Expression.3459:3626:parseThese */
function Text_Parser_Expression_n__3459_3626_parseThese($0, $1, $2, $3) {
 return Text_Parser_Expression_case__buildExpressionParserx2clevelx2cparseThese_3953($0, $1, $2, $3, Text_Parser_Expression_n__3459_3628_sortedOps($0, $1, $2, $3));
}

/* Text.Parser.Expression.3811:3988:nassocP */
function Text_Parser_Expression_n__3811_3988_nassocP($0, $1, $2, $3, $4, $5, $6, $7, $8, $9) {
 return {h: 8 /* SeqEat */, a1: 1, a2: Text_Parser_choice(csegen_49(), 1, $6), a3: () => f => ({h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_Expression_n__3811_3993_termP($0, $1, $2, $3, $4, $5, $6, $7, $8), a3: () => y => ({h: 0 /* Empty */, a1: f($9)(y)})})};
}

/* Text.Parser.Expression.3440:3613:level */
function Text_Parser_Expression_n__3440_3613_level($0, $1, $2, $3) {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: Text_Parser_Expression_n__3459_3626_parseThese($0, $1, $3, $2), a4: () => $2};
}

/* Text.Parser.Expression.3811:3987:lassocP1 */
function Text_Parser_Expression_n__3811_3987_lassocP1($0, $1, $2, $3, $4, $5, $6, $7, $8, $9) {
 return {h: 12 /* Alt */, a1: 1, a2: 0, a3: Text_Parser_Expression_n__3811_3986_lassocP($0, $1, $2, $3, $4, $5, $6, $7, $8, $9), a4: () => ({h: 0 /* Empty */, a1: $9})};
}

/* Text.Parser.Expression.3811:3986:lassocP */
function Text_Parser_Expression_n__3811_3986_lassocP($0, $1, $2, $3, $4, $5, $6, $7, $8, $9) {
 return {h: 8 /* SeqEat */, a1: 1, a2: Text_Parser_choice(csegen_49(), 1, $4), a3: () => f => ({h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_Expression_n__3811_3993_termP($0, $1, $2, $3, $4, $5, $6, $7, $8), a3: () => y => Text_Parser_Expression_n__3811_3987_lassocP1($0, $1, $2, $3, $4, $5, $6, $7, $8, f($9)(y))})};
}

/* Text.Parser.Expression.buildExpressionParser : OperatorTable state k a -> Grammar state k True a -> Grammar state k True a */
function Text_Parser_Expression_buildExpressionParser($0, $1) {
 return Prelude_Types_foldl_Foldable_List($4 => $5 => Text_Parser_Expression_n__3440_3613_level($1, $0, $4, $5), $1, $0);
}

/* Text.Parser.when : Bool -> Lazy (Grammar state token False ()) -> Grammar state token False () */
function Text_Parser_when($0, $1) {
 switch($0) {
  case 1: return $1();
  case 0: return {h: 0 /* Empty */, a1: undefined};
 }
}

/* Text.Parser.some : Grammar state tok True a -> Grammar state tok True (List1 a) */
function Text_Parser_some($0) {
 return {h: 8 /* SeqEat */, a1: 0, a2: $0, a3: () => $4 => ({h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: Text_Parser_many($0), a4: $b => ({h: 0 /* Empty */, a1: {a1: $4, a2: $b}})})};
}

/* Text.Parser.sepBy1 : Grammar state tok True s -> Grammar state tok c a -> Grammar state tok c (List1 a) */
function Text_Parser_sepBy1($0, $1, $2) {
 return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 9 /* SeqEmpty */, a1: 0, a2: $0, a3: {h: 0 /* Empty */, a1: $a => $b => ({a1: $a, a2: $b})}, a4: f => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, f, $2)}, a4: f => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(0, f, Text_Parser_many({h: 9 /* SeqEmpty */, a1: 1, a2: $0, a3: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $20 => $21 => $21, $1), a4: $24 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, $24, $2)}))};
}

/* Text.Parser.sepBy : Grammar state tok True s -> Grammar state tok c a -> Grammar state tok False (List a) */
function Text_Parser_sepBy($0, $1, $2) {
 return Text_Parser_option($0, {h: 0}, Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, $a => Data_List1_forget($a), Text_Parser_sepBy1($0, $1, $2)));
}

/* Text.Parser.option : a -> Grammar state tok c a -> Grammar state tok False a */
function Text_Parser_option($0, $1, $2) {
 switch($0) {
  case 0: return {h: 12 /* Alt */, a1: 0, a2: 0, a3: $2, a4: () => ({h: 0 /* Empty */, a1: $1})};
  case 1: return {h: 12 /* Alt */, a1: 1, a2: 0, a3: $2, a4: () => ({h: 0 /* Empty */, a1: $1})};
 }
}

/* Text.Parser.match : {auto conArg : TokenKind k} -> Eq k => (kind : k) ->
Grammar state (Token k) True (TokType kind) */
function Text_Parser_match($0, $1, $2) {
 const $4 = t => {
  switch($1.a1(t.a1)($2)) {
   case 1: return {a1: $0.a2($2)(t.a2)};
   case 0: return {h: 0};
  }
 };
 return {h: 1 /* Terminal */, a1: 'Unrecognised input', a2: $4};
}

/* Text.Parser.many : Grammar state tok True a -> Grammar state tok False (List a) */
function Text_Parser_many($0) {
 return Text_Parser_option(1, {h: 0}, Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $8 => Data_List1_forget($8), Text_Parser_some($0)));
}

/* Text.Parser.choiceMap : (a -> Grammar state tok c b) -> Foldable t => t a -> Grammar state tok c b */
function Text_Parser_choiceMap($0, $1, $2, $3) {
 return $2.a1(undefined)(undefined)(x => acc => ({h: 12 /* Alt */, a1: $0, a2: $0, a3: $1(x), a4: () => acc}))({h: 4 /* Fail */, a1: {h: 0}, a2: 0, a3: 'No more options'})($3);
}

/* Text.Parser.choice : Foldable t => t (Grammar state tok c a) -> Grammar state tok c a */
function Text_Parser_choice($0, $1, $2) {
 return Text_Parser_choiceMap($1, $6 => $6, $0, $2);
}

/* Text.Parser.between : Grammar state tok True l -> Grammar state tok True r -> Grammar state tok c a -> Grammar state tok True a */
function Text_Parser_between($0, $1, $2, $3) {
 return {h: 9 /* SeqEmpty */, a1: 1, a2: 1, a3: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $a => $b => $a, {h: 9 /* SeqEmpty */, a1: 1, a2: $0, a3: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $13 => $14 => $14, $1), a4: f => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, f, $3)}), a4: f => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, f, $2)};
}

/* Text.Parser.Core.case block in doParse */
function Text_Parser_Core_case__doParse_5194($0, $1, $2, $3, $4, $5) {
 switch($5.h) {
  case 0: /* Failure */ return {h: 0 /* Failure */, a1: $5.a1, a2: $5.a2, a3: $5.a3};
  case 1: /* Res */ return {h: 1 /* Res */, a1: $5.a1, a2: $5.a2, a3: Text_Bounded_map_Functor_WithBounds($f => $5.a3, $5.a3), a4: $5.a4};
 }
}

/* Text.Parser.Core.case block in doParse */
function Text_Parser_Core_case__doParse_4881($0, $1, $2, $3, $4, $5, $6, $7) {
 switch($7.h) {
  case 0: /* Failure */ return {h: 0 /* Failure */, a1: $7.a1, a2: $7.a2, a3: $7.a3};
  case 1: /* Res */ return Text_Parser_Core_mergeWith($7.a3, Text_Parser_Core_doParse($0, $7.a1, $7.a2, $3()($7.a3.a1), $7.a4));
 }
}

/* Text.Parser.Core.case block in doParse */
function Text_Parser_Core_case__doParse_4767($0, $1, $2, $3, $4, $5, $6, $7, $8) {
 switch($8.h) {
  case 0: /* Failure */ return {h: 0 /* Failure */, a1: $8.a1, a2: $8.a2, a3: $8.a3};
  case 1: /* Res */ return Text_Parser_Core_mergeWith($8.a3, Text_Parser_Core_doParse($0, $8.a1, $8.a2, $4($8.a3.a1), $8.a4));
 }
}

/* Text.Parser.Core.case block in case block in case block in doParse */
function Text_Parser_Core_case__casex20blockx20inx20casex20blockx20inx20doParse_4529($0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b) {
 switch($b.h) {
  case 0: /* Failure */ {
   let $d;
   switch($b.a1) {
    case 1: {
     $d = 1;
     break;
    }
    case 0: {
     $d = $b.a2;
     break;
    }
   }
   switch($d) {
    case 1: return {h: 0 /* Failure */, a1: $b.a1, a2: $b.a2, a3: $b.a3};
    case 0: return {h: 0 /* Failure */, a1: $6, a2: 0, a3: Data_List1_x2bx2b($7, $b.a3)};
   }
  }
  case 1: /* Res */ return {h: 1 /* Res */, a1: $b.a1, a2: $6, a3: $b.a3, a4: $b.a4};
 }
}

/* Text.Parser.Core.case block in doParse */
function Text_Parser_Core_case__doParse_4413($0, $1, $2, $3, $4, $5, $6, $7, $8) {
 switch($8.h) {
  case 0: /* Failure */ {
   let $a;
   switch($8.a1) {
    case 1: {
     $a = 1;
     break;
    }
    case 0: {
     $a = $8.a2;
     break;
    }
   }
   switch($a) {
    case 1: return {h: 0 /* Failure */, a1: $7, a2: $8.a2, a3: $8.a3};
    case 0: return Text_Parser_Core_case__casex20blockx20inx20casex20blockx20inx20doParse_4529($0, $2, $3, $4, $5, $6, $7, $8.a3, $8.a2, $8.a1, $1, Text_Parser_Core_doParse($0, $1, 0, $3(), $6));
   }
  }
  case 1: /* Res */ return {h: 1 /* Res */, a1: $8.a1, a2: $7, a3: $8.a3, a4: $8.a4};
 }
}

/* Text.Parser.Core.case block in doParse */
function Text_Parser_Core_case__doParse_4048($0, $1, $2, $3, $4, $5) {
 switch($5.h) {
  case 0: /* Failure */ return {h: 0 /* Failure */, a1: $5.a1, a2: 1, a3: $5.a3};
  default: return $5;
 }
}

/* Text.Parser.Core.case block in doParse */
function Text_Parser_Core_case__doParse_3951($0, $1, $2, $3, $4, $5) {
 switch($5.h) {
  case 0: /* Failure */ return {h: 0 /* Failure */, a1: $5.a1, a2: 0, a3: $5.a3};
  default: return $5;
 }
}

/* Text.Parser.Core.show */
function Text_Parser_Core_show_Show_x28ParsingErrorx20x24tokx29($0, $1) {
 switch($1.a2.h) {
  case 0: /* nothing */ return ('PARSING ERROR: '+$1.a1);
  case undefined: /* just */ return ('PARSING ERROR: '+($1.a1+Prelude_Types_foldMap_Foldable_List(csegen_202(), $f => $f, {a1: ' @ L', a2: {a1: Prelude_Show_show_Show_Int($1.a2.a1.a1), a2: {a1: ':', a2: {a1: Prelude_Show_show_Show_Int($1.a2.a1.a2), a2: {a1: '-L', a2: {a1: Prelude_Show_show_Show_Int($1.a2.a1.a3), a2: {a1: ':', a2: {a1: Prelude_Show_show_Show_Int($1.a2.a1.a4), a2: {h: 0}}}}}}}}})));
 }
}

/* Text.Parser.Core.showPrec */
function Text_Parser_Core_showPrec_Show_x28ParsingErrorx20x24tokx29($0, $1, $2) {
 return Text_Parser_Core_show_Show_x28ParsingErrorx20x24tokx29($0, $2);
}

/* Text.Parser.Core.map */
function Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, $1, $2) {
 switch($0) {
  case 0: {
   switch($2.h) {
    case 0: /* Empty */ return {h: 0 /* Empty */, a1: $1($2.a1)};
    default: {
     switch($2.h) {
      case 4: /* Fail */ return {h: 4 /* Fail */, a1: $2.a1, a2: $2.a2, a3: $2.a3};
      case 5: /* Try */ return {h: 5 /* Try */, a1: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, $1, $2.a1)};
      case 7: /* MustWork */ return {h: 7 /* MustWork */, a1: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, $1, $2.a1)};
      default: {
       switch($0) {
        case 1: {
         switch($2.h) {
          case 1: /* Terminal */ return {h: 1 /* Terminal */, a1: $2.a1, a2: $1a => Prelude_Types_map_Functor_Maybe($1, $2.a2($1a))};
          default: {
           switch($2.h) {
            case 12: /* Alt */ return {h: 12 /* Alt */, a1: $2.a1, a2: $2.a2, a3: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3), a4: () => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4())};
            default: {
             switch($0) {
              case 1: {
               switch($2.h) {
                case 8: /* SeqEat */ return {h: 8 /* SeqEat */, a1: $2.a1, a2: $2.a2, a3: () => val => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3()(val))};
                default: {
                 switch($2.h) {
                  case 9: /* SeqEmpty */ return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: val => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4(val))};
                  default: {
                   switch($0) {
                    case 1: {
                     switch($2.h) {
                      case 10: /* ThenEat */ return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: () => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3())};
                      default: {
                       switch($2.h) {
                        case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                        case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $5d => ({h: 0 /* Empty */, a1: $1($5d)})};
                        default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $65 => ({h: 0 /* Empty */, a1: $1($65)})};
                       }
                      }
                     }
                    }
                    default: {
                     switch($2.h) {
                      case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                      case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $77 => ({h: 0 /* Empty */, a1: $1($77)})};
                      default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $7f => ({h: 0 /* Empty */, a1: $1($7f)})};
                     }
                    }
                   }
                  }
                 }
                }
               }
              }
              default: {
               switch($2.h) {
                case 9: /* SeqEmpty */ return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: val => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4(val))};
                default: {
                 switch($0) {
                  case 1: {
                   switch($2.h) {
                    case 10: /* ThenEat */ return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: () => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3())};
                    default: {
                     switch($2.h) {
                      case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                      case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $a6 => ({h: 0 /* Empty */, a1: $1($a6)})};
                      default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $ae => ({h: 0 /* Empty */, a1: $1($ae)})};
                     }
                    }
                   }
                  }
                  default: {
                   switch($2.h) {
                    case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                    case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $c0 => ({h: 0 /* Empty */, a1: $1($c0)})};
                    default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $c8 => ({h: 0 /* Empty */, a1: $1($c8)})};
                   }
                  }
                 }
                }
               }
              }
             }
            }
           }
          }
         }
        }
        default: {
         switch($2.h) {
          case 12: /* Alt */ return {h: 12 /* Alt */, a1: $2.a1, a2: $2.a2, a3: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3), a4: () => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4())};
          default: {
           switch($0) {
            case 1: {
             switch($2.h) {
              case 8: /* SeqEat */ return {h: 8 /* SeqEat */, a1: $2.a1, a2: $2.a2, a3: () => val => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3()(val))};
              default: {
               switch($2.h) {
                case 9: /* SeqEmpty */ return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: val => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4(val))};
                default: {
                 switch($0) {
                  case 1: {
                   switch($2.h) {
                    case 10: /* ThenEat */ return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: () => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3())};
                    default: {
                     switch($2.h) {
                      case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                      case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $109 => ({h: 0 /* Empty */, a1: $1($109)})};
                      default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $111 => ({h: 0 /* Empty */, a1: $1($111)})};
                     }
                    }
                   }
                  }
                  default: {
                   switch($2.h) {
                    case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                    case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $123 => ({h: 0 /* Empty */, a1: $1($123)})};
                    default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $12b => ({h: 0 /* Empty */, a1: $1($12b)})};
                   }
                  }
                 }
                }
               }
              }
             }
            }
            default: {
             switch($2.h) {
              case 9: /* SeqEmpty */ return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: val => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4(val))};
              default: {
               switch($0) {
                case 1: {
                 switch($2.h) {
                  case 10: /* ThenEat */ return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: () => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3())};
                  default: {
                   switch($2.h) {
                    case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                    case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $152 => ({h: 0 /* Empty */, a1: $1($152)})};
                    default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $15a => ({h: 0 /* Empty */, a1: $1($15a)})};
                   }
                  }
                 }
                }
                default: {
                 switch($2.h) {
                  case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                  case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $16c => ({h: 0 /* Empty */, a1: $1($16c)})};
                  default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $174 => ({h: 0 /* Empty */, a1: $1($174)})};
                 }
                }
               }
              }
             }
            }
           }
          }
         }
        }
       }
      }
     }
    }
   }
  }
  default: {
   switch($2.h) {
    case 4: /* Fail */ return {h: 4 /* Fail */, a1: $2.a1, a2: $2.a2, a3: $2.a3};
    case 5: /* Try */ return {h: 5 /* Try */, a1: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, $1, $2.a1)};
    case 7: /* MustWork */ return {h: 7 /* MustWork */, a1: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, $1, $2.a1)};
    default: {
     switch($0) {
      case 1: {
       switch($2.h) {
        case 1: /* Terminal */ return {h: 1 /* Terminal */, a1: $2.a1, a2: $18a => Prelude_Types_map_Functor_Maybe($1, $2.a2($18a))};
        default: {
         switch($2.h) {
          case 12: /* Alt */ return {h: 12 /* Alt */, a1: $2.a1, a2: $2.a2, a3: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3), a4: () => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4())};
          default: {
           switch($0) {
            case 1: {
             switch($2.h) {
              case 8: /* SeqEat */ return {h: 8 /* SeqEat */, a1: $2.a1, a2: $2.a2, a3: () => val => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3()(val))};
              default: {
               switch($2.h) {
                case 9: /* SeqEmpty */ return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: val => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4(val))};
                default: {
                 switch($0) {
                  case 1: {
                   switch($2.h) {
                    case 10: /* ThenEat */ return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: () => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3())};
                    default: {
                     switch($2.h) {
                      case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                      case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $1cd => ({h: 0 /* Empty */, a1: $1($1cd)})};
                      default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $1d5 => ({h: 0 /* Empty */, a1: $1($1d5)})};
                     }
                    }
                   }
                  }
                  default: {
                   switch($2.h) {
                    case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                    case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $1e7 => ({h: 0 /* Empty */, a1: $1($1e7)})};
                    default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $1ef => ({h: 0 /* Empty */, a1: $1($1ef)})};
                   }
                  }
                 }
                }
               }
              }
             }
            }
            default: {
             switch($2.h) {
              case 9: /* SeqEmpty */ return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: val => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4(val))};
              default: {
               switch($0) {
                case 1: {
                 switch($2.h) {
                  case 10: /* ThenEat */ return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: () => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3())};
                  default: {
                   switch($2.h) {
                    case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                    case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $216 => ({h: 0 /* Empty */, a1: $1($216)})};
                    default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $21e => ({h: 0 /* Empty */, a1: $1($21e)})};
                   }
                  }
                 }
                }
                default: {
                 switch($2.h) {
                  case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                  case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $230 => ({h: 0 /* Empty */, a1: $1($230)})};
                  default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $238 => ({h: 0 /* Empty */, a1: $1($238)})};
                 }
                }
               }
              }
             }
            }
           }
          }
         }
        }
       }
      }
      default: {
       switch($2.h) {
        case 12: /* Alt */ return {h: 12 /* Alt */, a1: $2.a1, a2: $2.a2, a3: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3), a4: () => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4())};
        default: {
         switch($0) {
          case 1: {
           switch($2.h) {
            case 8: /* SeqEat */ return {h: 8 /* SeqEat */, a1: $2.a1, a2: $2.a2, a3: () => val => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3()(val))};
            default: {
             switch($2.h) {
              case 9: /* SeqEmpty */ return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: val => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4(val))};
              default: {
               switch($0) {
                case 1: {
                 switch($2.h) {
                  case 10: /* ThenEat */ return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: () => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3())};
                  default: {
                   switch($2.h) {
                    case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                    case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $279 => ({h: 0 /* Empty */, a1: $1($279)})};
                    default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $281 => ({h: 0 /* Empty */, a1: $1($281)})};
                   }
                  }
                 }
                }
                default: {
                 switch($2.h) {
                  case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                  case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $293 => ({h: 0 /* Empty */, a1: $1($293)})};
                  default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $29b => ({h: 0 /* Empty */, a1: $1($29b)})};
                 }
                }
               }
              }
             }
            }
           }
          }
          default: {
           switch($2.h) {
            case 9: /* SeqEmpty */ return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: val => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4(val))};
            default: {
             switch($0) {
              case 1: {
               switch($2.h) {
                case 10: /* ThenEat */ return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: () => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, $1, $2.a3())};
                default: {
                 switch($2.h) {
                  case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                  case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $2c2 => ({h: 0 /* Empty */, a1: $1($2c2)})};
                  default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $2ca => ({h: 0 /* Empty */, a1: $1($2ca)})};
                 }
                }
               }
              }
              default: {
               switch($2.h) {
                case 11: /* ThenEmpty */ return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, $1, $2.a4)};
                case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $2dc => ({h: 0 /* Empty */, a1: $1($2dc)})};
                default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $2e4 => ({h: 0 /* Empty */, a1: $1($2e4)})};
               }
              }
             }
            }
           }
          }
         }
        }
       }
      }
     }
    }
   }
  }
 }
}

/* Text.Parser.Core.parse : Grammar () tok c ty -> List (WithBounds tok) -> Either (List1 (ParsingError tok)) (ty,
List (WithBounds tok)) */
function Text_Parser_Core_parse($0, $1, $2) {
 const $3 = Text_Parser_Core_doParse($6 => $7 => (undefined), undefined, 0, $1, $2);
 switch($3.h) {
  case 0: /* Failure */ return {h: 0 /* Left */, a1: $3.a3};
  case 1: /* Res */ return {h: 1 /* Right */, a1: {a1: $3.a3.a1, a2: $3.a4}};
 }
}

/* Text.Parser.Core.mergeWith : WithBounds ty -> ParseResult state tok sy -> ParseResult state tok sy */
function Text_Parser_Core_mergeWith($0, $1) {
 switch($1.h) {
  case 1: /* Res */ return {h: 1 /* Res */, a1: $1.a1, a2: $1.a2, a3: Text_Bounded_mergeBounds($0, $1.a3), a4: $1.a4};
  default: return $1;
 }
}

/* Text.Parser.Core.doParse : Semigroup state =>
state -> Bool -> Grammar state tok c ty -> List (WithBounds tok) -> ParseResult state tok ty */
function Text_Parser_Core_doParse($0, $1, $2, $3, $4) {
 switch($3.h) {
  case 0: /* Empty */ return {h: 1 /* Res */, a1: $1, a2: $2, a3: Text_Bounded_irrelevantBounds($3.a1), a4: $4};
  case 4: /* Fail */ return {h: 0 /* Failure */, a1: $2, a2: $3.a2, a3: {a1: {a1: $3.a3, a2: Prelude_Types_x3cx7cx3e_Alternative_Maybe($3.a1, () => Prelude_Types_map_Functor_Maybe($17 => $17.a3, Data_List_headx27($4)))}, a2: {h: 0}}};
  case 5: /* Try */ return Text_Parser_Core_case__doParse_3951($0, $1, $3.a1, $4, $2, Text_Parser_Core_doParse($0, $1, $2, $3.a1, $4));
  case 6: /* Commit */ return {h: 1 /* Res */, a1: $1, a2: 1, a3: Text_Bounded_irrelevantBounds(undefined), a4: $4};
  case 7: /* MustWork */ return Text_Parser_Core_case__doParse_4048($0, $1, $3.a1, $4, $2, Text_Parser_Core_doParse($0, $1, $2, $3.a1, $4));
  case 1: /* Terminal */ {
   switch($4.h) {
    case 0: /* nil */ return {h: 0 /* Failure */, a1: $2, a2: 0, a3: csegen_277()};
    case undefined: /* cons */ {
     const $42 = $3.a2($4.a1.a1);
     switch($42.h) {
      case 0: /* nothing */ return {h: 0 /* Failure */, a1: $2, a2: 0, a3: {a1: {a1: $3.a1, a2: {a1: $4.a1.a3}}, a2: {h: 0}}};
      case undefined: /* just */ return {h: 1 /* Res */, a1: $1, a2: $2, a3: Text_Bounded_map_Functor_WithBounds($54 => $42.a1, $4.a1), a4: $4.a2};
     }
    }
   }
  }
  case 3: /* EOF */ {
   switch($4.h) {
    case 0: /* nil */ return {h: 1 /* Res */, a1: $1, a2: $2, a3: Text_Bounded_irrelevantBounds(undefined), a4: {h: 0}};
    case undefined: /* cons */ return {h: 0 /* Failure */, a1: $2, a2: 0, a3: {a1: {a1: 'Expected end of input', a2: {a1: $4.a1.a3}}, a2: {h: 0}}};
   }
  }
  case 2: /* NextIs */ {
   switch($4.h) {
    case 0: /* nil */ return {h: 0 /* Failure */, a1: $2, a2: 0, a3: csegen_277()};
    case undefined: /* cons */ {
     switch($3.a2($4.a1.a1)) {
      case 1: return {h: 1 /* Res */, a1: $1, a2: $2, a3: Text_Bounded_removeIrrelevance($4.a1), a4: {a1: $4.a1, a2: $4.a2}};
      case 0: return {h: 0 /* Failure */, a1: $2, a2: 0, a3: {a1: {a1: $3.a1, a2: {a1: $4.a1.a3}}, a2: {h: 0}}};
     }
    }
   }
  }
  case 12: /* Alt */ return Text_Parser_Core_case__doParse_4413($0, $1, $3.a2, $3.a4, $3.a1, $3.a3, $4, $2, Text_Parser_Core_doParse($0, $1, 0, $3.a3, $4));
  case 9: /* SeqEmpty */ return Text_Parser_Core_case__doParse_4767($0, $3.a1, $3.a2, $1, $3.a4, $3.a3, $4, $2, Text_Parser_Core_doParse($0, $1, $2, $3.a3, $4));
  case 8: /* SeqEat */ return Text_Parser_Core_case__doParse_4881($0, $3.a1, $1, $3.a3, $3.a2, $4, $2, Text_Parser_Core_doParse($0, $1, $2, $3.a2, $4));
  case 11: /* ThenEmpty */ {
   const $b0 = Text_Parser_Core_doParse($0, $1, $2, $3.a3, $4);
   switch($b0.h) {
    case 0: /* Failure */ return {h: 0 /* Failure */, a1: $b0.a1, a2: $b0.a2, a3: $b0.a3};
    case 1: /* Res */ return Text_Parser_Core_mergeWith($b0.a3, Text_Parser_Core_doParse($0, $b0.a1, $b0.a2, $3.a4, $b0.a4));
   }
  }
  case 10: /* ThenEat */ {
   const $c3 = Text_Parser_Core_doParse($0, $1, $2, $3.a2, $4);
   switch($c3.h) {
    case 0: /* Failure */ return {h: 0 /* Failure */, a1: $c3.a1, a2: $c3.a2, a3: $c3.a3};
    case 1: /* Res */ return Text_Parser_Core_mergeWith($c3.a3, Text_Parser_Core_doParse($0, $c3.a1, $c3.a2, $3.a3(), $c3.a4));
   }
  }
  case 13: /* Bounds */ return Text_Parser_Core_case__doParse_5194($0, $1, $3.a1, $4, $2, Text_Parser_Core_doParse($0, $1, $2, $3.a1, $4));
  case 14: /* Position */ {
   switch($4.h) {
    case 0: /* nil */ return {h: 0 /* Failure */, a1: $2, a2: 0, a3: csegen_277()};
    case undefined: /* cons */ return {h: 1 /* Res */, a1: $1, a2: $2, a3: Text_Bounded_irrelevantBounds($4.a1.a3), a4: {a1: $4.a1, a2: $4.a2}};
   }
  }
  case 15: /* Act */ return {h: 1 /* Res */, a1: $0($1)($3.a1), a2: $2, a3: Text_Bounded_irrelevantBounds(undefined), a4: $4};
 }
}

/* System.getArgs : HasIO io => io (List String) */
function System_getArgs($0) {
 const $12 = n => {
  switch(Prelude_EqOrd_x3e_Ord_Int(n, Number(_truncBigInt32(0n)))) {
   case 1: return Prelude_Basics_flip($1a => $1b => Prelude_Types_traverse_Traversable_List($0.a1.a1, $1a, $1b), Prelude_Types_rangeFromTo_Range_x24a({a1: {a1: csegen_280(), a2: $29 => $2a => Prelude_Num_div_Integral_Int($29, $2a), a3: $2f => $30 => Prelude_Num_mod_Integral_Int($2f, $30)}, a2: {a1: csegen_219(), a2: {a1: csegen_280(), a2: $3b => _sub32s(0, $3b), a3: $3f => $40 => _sub32s($3f, $40)}}}, 0, _sub32s(n, 1)), $48 => $0.a2(undefined)($4e => System_prim__getArg($48, $4e)));
   case 0: return $0.a1.a1.a2(undefined)({h: 0});
  }
 };
 return $0.a1.a2(undefined)(undefined)($0.a2(undefined)($f => System_prim__getArgCount($f)))($12);
}

/* Language.JSON.parse : String -> Maybe JSON */
function Language_JSON_parse($0) {
 return Prelude_Types_x3ex3ex3d_Monad_Maybe(Language_JSON_Lexer_lexJSON($0), $6 => Language_JSON_Parser_parseJSON($6));
}

/* Data.String.Extra.index : Nat -> String -> Maybe Char */
function Data_String_Extra_index($0, $1) {
 return Data_String_Extra_with__index_3642($1, Prelude_Types_fastUnpack($1), $0);
}

/* Language.JSON.Parser.3829:2407:values */
const Language_JSON_Parser_n__3829_2407_values = __lazy(function () {
 return Text_Parser_sepBy(1, Language_JSON_Parser_punct({h: 0 /* Comma */}), Language_JSON_Parser_json());
});

/* Language.JSON.Parser.3826:2316:properties */
const Language_JSON_Parser_n__3826_2316_properties = __lazy(function () {
 return Text_Parser_sepBy(1, Language_JSON_Parser_punct({h: 0 /* Comma */}), {h: 8 /* SeqEat */, a1: 1, a2: Language_JSON_Parser_rawString(), a3: () => key => ({h: 10 /* ThenEat */, a1: 1, a2: Language_JSON_Parser_punct({h: 1 /* Colon */}), a3: () => ({h: 8 /* SeqEat */, a1: 0, a2: Language_JSON_Parser_json(), a3: () => value => ({h: 0 /* Empty */, a1: {a1: key, a2: value}})})})});
});

/* Language.JSON.Parser.string : Grammar state JSONToken True JSON */
const Language_JSON_Parser_string = __lazy(function () {
 return Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $3 => ({h: 3 /* JString */, a1: $3}), Language_JSON_Parser_rawString());
});

/* Language.JSON.Parser.rawString : Grammar state JSONToken True String */
const Language_JSON_Parser_rawString = __lazy(function () {
 return {h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_match(csegen_294(), csegen_297(), {h: 2 /* JTString */}), a3: () => mstr => {
  switch(mstr.h) {
   case undefined: /* just */ return {h: 0 /* Empty */, a1: mstr.a1};
   case 0: /* nothing */ return {h: 4 /* Fail */, a1: {h: 0}, a2: 0, a3: 'invalid string'};
  }
 }};
});

/* Language.JSON.Parser.punct : Punctuation -> Grammar state JSONToken True () */
function Language_JSON_Parser_punct($0) {
 return Text_Parser_match(csegen_294(), csegen_297(), {h: 4 /* JTPunct */, a1: $0});
}

/* Language.JSON.Parser.parseJSON : List (WithBounds JSONToken) -> Maybe JSON */
function Language_JSON_Parser_parseJSON($0) {
 const $9 = $a => {
  switch(Language_JSON_Tokens_ignored($a)) {
   case 1: return 0;
   case 0: return 1;
  }
 };
 const $6 = Prelude_Types_List_filterAppend({h: 0}, $9, $0);
 const $1 = Text_Parser_Core_parse(1, Language_JSON_Parser_json(), $6);
 switch($1.h) {
  case 1: /* Right */ {
   switch($1.a1.h) {
    case undefined: /* cons */ {
     switch($1.a1.a2.h) {
      case 0: /* nil */ return {a1: $1.a1.a1};
      default: return {h: 0};
     }
    }
    default: return {h: 0};
   }
  }
  default: return {h: 0};
 }
}

/* Language.JSON.Parser.object : Grammar state JSONToken True JSON */
const Language_JSON_Parser_object = __lazy(function () {
 return {h: 10 /* ThenEat */, a1: 1, a2: Language_JSON_Parser_punct({h: 3 /* Curly */, a1: 0}), a3: () => ({h: 11 /* ThenEmpty */, a1: 0, a2: 1, a3: {h: 6 /* Commit */}, a4: {h: 9 /* SeqEmpty */, a1: 0, a2: 1, a3: Language_JSON_Parser_n__3826_2316_properties(), a4: props => ({h: 10 /* ThenEat */, a1: 0, a2: Language_JSON_Parser_punct({h: 3 /* Curly */, a1: 1}), a3: () => ({h: 0 /* Empty */, a1: {h: 5 /* JObject */, a1: props}})})}})};
});

/* Language.JSON.Parser.number : Grammar state JSONToken True JSON */
const Language_JSON_Parser_number = __lazy(function () {
 return Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $3 => ({h: 2 /* JNumber */, a1: $3}), Text_Parser_match(csegen_294(), csegen_297(), {h: 1 /* JTNumber */}));
});

/* Language.JSON.Parser.null : Grammar state JSONToken True JSON */
const Language_JSON_Parser_null$ = __lazy(function () {
 return Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $3 => ({h: 0 /* JNull */}), Text_Parser_match(csegen_294(), csegen_297(), {h: 3 /* JTNull */}));
});

/* Language.JSON.Parser.json : Grammar state JSONToken True JSON */
const Language_JSON_Parser_json = __lazy(function () {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: Language_JSON_Parser_object(), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: Language_JSON_Parser_array(), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: Language_JSON_Parser_string(), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: Language_JSON_Parser_boolean(), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: Language_JSON_Parser_number(), a4: () => Language_JSON_Parser_null$()})})})})};
});

/* Language.JSON.Parser.boolean : Grammar state JSONToken True JSON */
const Language_JSON_Parser_boolean = __lazy(function () {
 return Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $3 => ({h: 1 /* JBoolean */, a1: $3}), Text_Parser_match(csegen_294(), csegen_297(), {h: 0 /* JTBoolean */}));
});

/* Language.JSON.Parser.array : Grammar state JSONToken True JSON */
const Language_JSON_Parser_array = __lazy(function () {
 return {h: 10 /* ThenEat */, a1: 1, a2: Language_JSON_Parser_punct({h: 2 /* Square */, a1: 0}), a3: () => ({h: 11 /* ThenEmpty */, a1: 0, a2: 1, a3: {h: 6 /* Commit */}, a4: {h: 9 /* SeqEmpty */, a1: 0, a2: 1, a3: Language_JSON_Parser_n__3829_2407_values(), a4: vals => ({h: 10 /* ThenEat */, a1: 0, a2: Language_JSON_Parser_punct({h: 2 /* Square */, a1: 1}), a3: () => ({h: 0 /* Empty */, a1: {h: 4 /* JArray */, a1: vals}})})}})};
});

/* Language.JSON.Tokens.tokValue */
function Language_JSON_Tokens_tokValue_TokenKind_JSONTokenKind($0, $1) {
 switch($0.h) {
  case 0: /* JTBoolean */ return Prelude_EqOrd_x3dx3d_Eq_String($1, 'true');
  case 1: /* JTNumber */ return _numberOfString($1);
  case 2: /* JTString */ return Language_JSON_String_stringValue($1);
  case 3: /* JTNull */ return undefined;
  case 4: /* JTPunct */ return undefined;
  case 5: /* JTIgnore */ return undefined;
 }
}

/* Language.JSON.Tokens.TokType */
function Language_JSON_Tokens_TokType_TokenKind_JSONTokenKind($0) {
 switch($0.h) {
  case 0: /* JTBoolean */ return {h: 'Prelude.Basics.Bool'};
  case 1: /* JTNumber */ return {h: 'Double'};
  case 2: /* JTString */ return {h: 'Prelude.Types.Maybe', a1: {h: 'String'}};
  case 3: /* JTNull */ return {h: 'Builtin.Unit'};
  case 4: /* JTPunct */ return {h: 'Builtin.Unit'};
  case 5: /* JTIgnore */ return {h: 'Builtin.Unit'};
 }
}

/* Language.JSON.Tokens.== */
function Language_JSON_Tokens_x3dx3d_Eq_Punctuation($0, $1) {
 switch($0.h) {
  case 0: /* Comma */ {
   switch($1.h) {
    case 0: /* Comma */ return 1;
    default: return 0;
   }
  }
  case 1: /* Colon */ {
   switch($1.h) {
    case 1: /* Colon */ return 1;
    default: return 0;
   }
  }
  case 2: /* Square */ {
   switch($1.h) {
    case 2: /* Square */ return Language_JSON_Tokens_x3dx3d_Eq_Bracket($0.a1, $1.a1);
    default: return 0;
   }
  }
  case 3: /* Curly */ {
   switch($1.h) {
    case 3: /* Curly */ return Language_JSON_Tokens_x3dx3d_Eq_Bracket($0.a1, $1.a1);
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Language.JSON.Tokens.== */
function Language_JSON_Tokens_x3dx3d_Eq_JSONTokenKind($0, $1) {
 switch($0.h) {
  case 0: /* JTBoolean */ {
   switch($1.h) {
    case 0: /* JTBoolean */ return 1;
    default: return 0;
   }
  }
  case 1: /* JTNumber */ {
   switch($1.h) {
    case 1: /* JTNumber */ return 1;
    default: return 0;
   }
  }
  case 2: /* JTString */ {
   switch($1.h) {
    case 2: /* JTString */ return 1;
    default: return 0;
   }
  }
  case 3: /* JTNull */ {
   switch($1.h) {
    case 3: /* JTNull */ return 1;
    default: return 0;
   }
  }
  case 4: /* JTPunct */ {
   switch($1.h) {
    case 4: /* JTPunct */ return Language_JSON_Tokens_x3dx3d_Eq_Punctuation($0.a1, $1.a1);
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Language.JSON.Tokens.== */
function Language_JSON_Tokens_x3dx3d_Eq_Bracket($0, $1) {
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
  default: return 0;
 }
}

/* Language.JSON.Tokens./= */
function Language_JSON_Tokens_x2fx3d_Eq_JSONTokenKind($0, $1) {
 switch(Language_JSON_Tokens_x3dx3d_Eq_JSONTokenKind($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Language.JSON.Tokens.ignored : WithBounds JSONToken -> Bool */
function Language_JSON_Tokens_ignored($0) {
 switch($0.h) {
  case undefined: /* record */ {
   switch($0.a1.h) {
    case undefined: /* cons */ {
     switch($0.a1.a1.h) {
      case 5: /* JTIgnore */ return 1;
      default: return 0;
     }
    }
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Language.JSON.String.stringValue : String -> Maybe String */
function Language_JSON_String_stringValue($0) {
 return Prelude_Types_x3ex3ex3d_Monad_Maybe(Language_JSON_String_Lexer_lexString($0), $6 => Language_JSON_String_Parser_parseString($6));
}

/* Language.JSON.String.permissiveStringLit : Lexer */
const Language_JSON_String_permissiveStringLit = __lazy(function () {
 return {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: Language_JSON_String_Lexer_quo(), a2: () => Text_Lexer_manyUntil(Language_JSON_String_Lexer_quo(), Text_Lexer_Core_x3cx7cx3e(Language_JSON_String_Lexer_esc(Text_Lexer_any()), Text_Lexer_any()))}, a2: () => Text_Lexer_opt(Language_JSON_String_Lexer_quo())};
});

/* Language.JSON.String.Tokens.3455:1167:hexVal */
function Language_JSON_String_Tokens_n__3455_1167_hexVal($0, $1) {
 switch(Prelude_EqOrd_x3ex3d_Ord_Char($1, 'A')) {
  case 1: return _add32s(_sub32s(_truncInt32($1.codePointAt(0)), _truncInt32('A'.codePointAt(0))), 10);
  case 0: return _sub32s(_truncInt32($1.codePointAt(0)), _truncInt32('0'.codePointAt(0)));
 }
}

/* Language.JSON.String.Tokens.tokValue */
function Language_JSON_String_Tokens_tokValue_TokenKind_JSONStringTokenKind($0, $1) {
 switch($0) {
  case 0: return undefined;
  case 1: return Language_JSON_String_Tokens_charValue($1);
  case 2: return Language_JSON_String_Tokens_simpleEscapeValue($1);
  case 3: return Language_JSON_String_Tokens_unicodeEscapeValue($1);
 }
}

/* Language.JSON.String.Tokens.TokType */
function Language_JSON_String_Tokens_TokType_TokenKind_JSONStringTokenKind($0) {
 switch($0) {
  case 0: return {h: 'Builtin.Unit'};
  case 1: return {h: 'Char'};
  case 2: return {h: 'Char'};
  case 3: return {h: 'Char'};
 }
}

/* Language.JSON.String.Tokens.== */
function Language_JSON_String_Tokens_x3dx3d_Eq_JSONStringTokenKind($0, $1) {
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
  case 3: {
   switch($1) {
    case 3: return 1;
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Language.JSON.String.Tokens./= */
function Language_JSON_String_Tokens_x2fx3d_Eq_JSONStringTokenKind($0, $1) {
 switch(Language_JSON_String_Tokens_x3dx3d_Eq_JSONStringTokenKind($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Language.JSON.String.Tokens.unicodeEscapeValue : String -> Char */
function Language_JSON_String_Tokens_unicodeEscapeValue($0) {
 return Language_JSON_String_Tokens_n__3455_1166_fromHex($0, Data_List_drop(2n, Prelude_Types_fastUnpack($0)), 0);
}

/* Language.JSON.String.Tokens.simpleEscapeValue : String -> Char */
function Language_JSON_String_Tokens_simpleEscapeValue($0) {
 const $1 = Data_String_Extra_index(1n, $0);
 switch($1.h) {
  case 0: /* nothing */ return '\0';
  case undefined: /* just */ {
   switch($1.a1) {
    case '\"': return '\"';
    case '\u{5c}': return '\u{5c}';
    case '/': return '/';
    case 'b': return '\u{8}';
    case 'f': return '\u{c}';
    case 'n': return '\n';
    case 'r': return '\r';
    case 't': return '\u{9}';
    default: return '\0';
   }
  }
 }
}

/* Language.JSON.String.Tokens.charValue : String -> Char */
function Language_JSON_String_Tokens_charValue($0) {
 const $1 = Data_String_Extra_index(0n, $0);
 switch($1.h) {
  case 0: /* nothing */ return '\0';
  case undefined: /* just */ return $1.a1;
 }
}

/* Language.JSON.String.Parser.stringChar : Grammar state JSONStringToken True Char */
const Language_JSON_String_Parser_stringChar = __lazy(function () {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: Text_Parser_match(csegen_326(), csegen_329(), 1), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: Text_Parser_match(csegen_326(), csegen_329(), 2), a4: () => Text_Parser_match(csegen_326(), csegen_329(), 3)})};
});

/* Language.JSON.String.Parser.quotedString : Grammar state JSONStringToken True String */
const Language_JSON_String_Parser_quotedString = __lazy(function () {
 const $0 = Text_Parser_match(csegen_326(), csegen_329(), 0);
 return {h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_between(0, $0, $0, Text_Parser_many(Language_JSON_String_Parser_stringChar())), a3: () => chars => ({h: 11 /* ThenEmpty */, a1: 0, a2: 0, a3: {h: 3 /* EOF */}, a4: {h: 0 /* Empty */, a1: Prelude_Types_fastPack(chars)}})};
});

/* Language.JSON.String.Parser.parseString : List (WithBounds JSONStringToken) -> Maybe String */
function Language_JSON_String_Parser_parseString($0) {
 const $1 = Text_Parser_Core_parse(1, Language_JSON_String_Parser_quotedString(), $0);
 switch($1.h) {
  case 1: /* Right */ {
   switch($1.a1.h) {
    case undefined: /* cons */ {
     switch($1.a1.a2.h) {
      case 0: /* nil */ return {a1: $1.a1.a1};
      default: return {h: 0};
     }
    }
    default: return {h: 0};
   }
  }
  default: return {h: 0};
 }
}

/* Language.JSON.String.Lexer.unicodeEscape : Lexer */
const Language_JSON_String_Lexer_unicodeEscape = __lazy(function () {
 return Language_JSON_String_Lexer_esc({h: 4 /* SeqEat */, a1: Text_Lexer_is('u'), a2: () => Text_Lexer_count(Text_Quantity_exactly(4n), Text_Lexer_hexDigit())});
});

/* Language.JSON.String.Lexer.simpleEscape : Lexer */
const Language_JSON_String_Lexer_simpleEscape = __lazy(function () {
 return Language_JSON_String_Lexer_esc(Text_Lexer_oneOf('\"\u{5c}/bfnrt'));
});

/* Language.JSON.String.Lexer.quo : Lexer */
const Language_JSON_String_Lexer_quo = __lazy(function () {
 return Text_Lexer_is('\"');
});

/* Language.JSON.String.Lexer.lexString : String -> Maybe (List (WithBounds JSONStringToken)) */
function Language_JSON_String_Lexer_lexString($0) {
 const $1 = Text_Lexer_Core_lex(Language_JSON_String_Lexer_jsonStringTokenMap(), $0);
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

/* Language.JSON.String.Lexer.legalChar : Lexer */
const Language_JSON_String_Lexer_legalChar = __lazy(function () {
 return Text_Lexer_non(Text_Lexer_Core_x3cx7cx3e(Language_JSON_String_Lexer_quo(), Text_Lexer_Core_x3cx7cx3e(Text_Lexer_is('\u{5c}'), Text_Lexer_control())));
});

/* Language.JSON.String.Lexer.jsonStringTokenMap : TokenMap JSONStringToken */
const Language_JSON_String_Lexer_jsonStringTokenMap = __lazy(function () {
 return Text_Lexer_toTokenMap({a1: {a1: Language_JSON_String_Lexer_quo(), a2: 0}, a2: {a1: {a1: Language_JSON_String_Lexer_unicodeEscape(), a2: 3}, a2: {a1: {a1: Language_JSON_String_Lexer_simpleEscape(), a2: 2}, a2: {a1: {a1: Language_JSON_String_Lexer_legalChar(), a2: 1}, a2: {h: 0}}}}});
});

/* Language.JSON.String.Lexer.esc : Lexer -> Lexer */
function Language_JSON_String_Lexer_esc($0) {
 return Text_Lexer_escape(Text_Lexer_is('\u{5c}'), $0);
}

/* Language.JSON.Lexer.numberLit : Lexer */
const Language_JSON_Lexer_numberLit = __lazy(function () {
 const $0 = Text_Lexer_is('-');
 const $3 = Text_Lexer_Core_x3cx7cx3e(Text_Lexer_is('0'), {h: 4 /* SeqEat */, a1: Text_Lexer_range('1', '9'), a2: () => Text_Lexer_many(Text_Lexer_digit())});
 const $11 = {h: 4 /* SeqEat */, a1: Text_Lexer_is('.'), a2: () => Text_Lexer_digits()};
 const $17 = {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: Text_Lexer_like('e'), a2: () => Text_Lexer_opt(Text_Lexer_oneOf('+-'))}, a2: () => Text_Lexer_digits()};
 return {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: {h: 5 /* SeqEmpty */, a1: Text_Lexer_opt($0), a2: $3}, a2: () => Text_Lexer_opt($11)}, a2: () => Text_Lexer_opt($17)};
});

/* Language.JSON.Lexer.lexJSON : String -> Maybe (List (WithBounds JSONToken)) */
function Language_JSON_Lexer_lexJSON($0) {
 const $1 = Text_Lexer_Core_lex(Language_JSON_Lexer_jsonTokenMap(), $0);
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

/* Language.JSON.Lexer.jsonTokenMap : TokenMap JSONToken */
const Language_JSON_Lexer_jsonTokenMap = __lazy(function () {
 return Text_Lexer_toTokenMap({a1: {a1: Text_Lexer_spaces(), a2: {h: 5 /* JTIgnore */}}, a2: {a1: {a1: Text_Lexer_is(','), a2: {h: 4 /* JTPunct */, a1: {h: 0 /* Comma */}}}, a2: {a1: {a1: Text_Lexer_is(':'), a2: {h: 4 /* JTPunct */, a1: {h: 1 /* Colon */}}}, a2: {a1: {a1: Text_Lexer_is('['), a2: {h: 4 /* JTPunct */, a1: {h: 2 /* Square */, a1: 0}}}, a2: {a1: {a1: Text_Lexer_is(']'), a2: {h: 4 /* JTPunct */, a1: {h: 2 /* Square */, a1: 1}}}, a2: {a1: {a1: Text_Lexer_is('{'), a2: {h: 4 /* JTPunct */, a1: {h: 3 /* Curly */, a1: 0}}}, a2: {a1: {a1: Text_Lexer_is('}'), a2: {h: 4 /* JTPunct */, a1: {h: 3 /* Curly */, a1: 1}}}, a2: {a1: {a1: Text_Lexer_exact('null'), a2: {h: 3 /* JTNull */}}, a2: {a1: {a1: Text_Lexer_Core_x3cx7cx3e(Text_Lexer_exact('true'), Text_Lexer_exact('false')), a2: {h: 0 /* JTBoolean */}}, a2: {a1: {a1: Language_JSON_Lexer_numberLit(), a2: {h: 1 /* JTNumber */}}, a2: {a1: {a1: Language_JSON_String_permissiveStringLit(), a2: {h: 2 /* JTString */}}, a2: {h: 0}}}}}}}}}}}});
});


exports.Pacillus_Idris2LSP_GetType_process = Pacillus_Idris2LSP_GetType_process

try{__mainExpression_0()}catch(e){if(e instanceof IdrisError){console.log('ERROR: ' + e.message)}else{throw e} }
