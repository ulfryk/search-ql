/* tslint:disable:strict-boolean-expressions */
import { List, Map } from 'immutable';
import { None } from 'monet';

import { BinaryOperationExpression, DateExpression, fromMatch, FunctionExpression, NotExpression, NumberExpression, PhraseExpression, SelectorExpression, TermExpression, TextExpression } from '../ast/expressions';
import { AndOperator, EqualityOperator, GteOperator, GtOperator, IsNotOperator, IsOperator, LikeOperator, LteOperator, LtOperator, NotLikeOperator, NotOperator, OrOperator } from '../ast/operators';
import { Expression, NodeEvaluation, ValueType } from '../common/model';
import { FunctionConfig, OptionalFunctionArg, ParserConfig } from '../config';

const config = new ParserConfig();
const { AND, GT, GTE, IS, IS_NOT, LIKE, LT, LTE, NOT, NOT_LIKE, OR } = config;

const And = new AndOperator(AND[1]);
const Is = new IsOperator(IS[1]);
const IsNot = new IsNotOperator(IS_NOT[1]);
const Like = new LikeOperator(LIKE[1]);
const Not = new NotOperator(NOT[1]);
const NotLike = new NotLikeOperator(NOT_LIKE[1]);
const Gt = new GtOperator(GT[1]);
const Gte = new GteOperator(GTE[1]);
const Lt = new LtOperator(LT[1]);
const Lte = new LteOperator(LTE[1]);
const Or = new OrOperator(OR[1]);
const And0 = new AndOperator(AND[0]);
const Is0 = new IsOperator(IS[1]);
const IsNot0 = new IsNotOperator(IS_NOT[1]);
const Like0 = new LikeOperator(LIKE[0]);
const Not0 = new NotOperator(NOT[0]);
const NotLike0 = new NotLikeOperator(NOT_LIKE[0]);
const Gt0 = new GtOperator(GT[0]);
const Gte0 = new GteOperator(GTE[0]);
const Lt0 = new LtOperator(LT[0]);
const Lte0 = new LteOperator(LTE[0]);
const Or0 = new OrOperator(OR[0]);

const date = (v: string) => DateExpression.of(v);
const num = (v: string) => NumberExpression.of(v);
const phrase = (v: string) => PhraseExpression.fromTerm(fromMatch(config)(v));
const sel = (v: string, t: ValueType) => new SelectorExpression(t, v);
const txt = (v: string) => TextExpression.of(v);

const phraseFrom = (e: TermExpression) => PhraseExpression.fromTerm(e);
const txtFrom = (e: TermExpression) => TextExpression.fromTerm(e);

const and = (l: Expression, r: Expression, op: AndOperator = And) =>
  new BinaryOperationExpression(op, [l, r]);

const eq = (l: TermExpression, r: TermExpression, op: EqualityOperator) =>
  new BinaryOperationExpression(op, [
    SelectorExpression.of(l.value)(
      (r as PhraseExpression<any>).term && (r as PhraseExpression<any>).term.returnType ||
      r.returnType),
    PhraseExpression.fromTerm(r),
  ]);

const eqR = (l: TermExpression, r: TermExpression, op: EqualityOperator) =>
  new BinaryOperationExpression(op, [l, r]);

// tslint:disable:no-unnecessary-callback-wrapper
const like = (l: TermExpression, r: TermExpression, op: LikeOperator = Like) => eq(l, r, op);
const notLike = (l: TermExpression, r: TermExpression, op: NotLikeOperator = NotLike) =>
  eq(l, r, op);
const is = (l: TermExpression, r: TermExpression, op: IsOperator = Is) => eq(l, r, op);
const isNot = (l: TermExpression, r: TermExpression, op: IsNotOperator = IsNot) => eq(l, r, op);
const gt = (l: TermExpression, r: TermExpression, op: GtOperator = Gt) => eq(l, r, op);
const gte = (l: TermExpression, r: TermExpression, op: GteOperator = Gte) => eq(l, r, op);
const lt = (l: TermExpression, r: TermExpression, op: LtOperator = Lt) => eq(l, r, op);
const lte = (l: TermExpression, r: TermExpression, op: LteOperator = Lte) => eq(l, r, op);

const likeR = (l: TermExpression, r: TermExpression, op: LikeOperator = Like) => eqR(l, r, op);
const notLikeR = (l: TermExpression, r: TermExpression, op: NotLikeOperator = NotLike) =>
  eqR(l, r, op);
const isR = (l: TermExpression, r: TermExpression, op: IsOperator = Is) => eqR(l, r, op);
const isNotR = (l: TermExpression, r: TermExpression, op: IsNotOperator = IsNot) => eqR(l, r, op);
const gtR = (l: TermExpression, r: TermExpression, op: GtOperator = Gt) => eqR(l, r, op);
const gteR = (l: TermExpression, r: TermExpression, op: GteOperator = Gte) => eqR(l, r, op);
const ltR = (l: TermExpression, r: TermExpression, op: LtOperator = Lt) => eqR(l, r, op);
const lteR = (l: TermExpression, r: TermExpression, op: LteOperator = Lte) => eqR(l, r, op);
// tslint:enable:no-unnecessary-callback-wrapper

const or = (l: Expression, r: Expression, op: OrOperator = Or) =>
  new BinaryOperationExpression(op, [l, r]);

const not = (v: Expression) => new NotExpression(v);

const andNot = (l: Expression, r: Expression, op: AndOperator = And0) =>
  and(l, not(r), op);

const fn = (name: string, returnType = ValueType.Boolean) => (...args: Expression[]) =>
  FunctionExpression.fromParseResult(new FunctionConfig(
    name,
    List(args.map(({ returnType: t }, i) => OptionalFunctionArg.fromType(t, `arg${i}`))),
    None(),
    returnType,
    // tslint:disable-next-line:no-unnecessary-callback-wrapper
    () => () => NodeEvaluation.ofBoolean(Map(), null)(false),
  ), args);

export {
  config,
  date, num, phrase, phraseFrom, sel, txt, txtFrom,
  and, andNot, And, And0,
  like, Like, Like0, likeR,
  notLike, NotLike, NotLike0, notLikeR,
  is, Is, Is0, isR,
  isNot, IsNot, IsNot0, isNotR,
  gt, Gt, Gt0, gtR,
  gte, Gte, Gte0, gteR,
  lt, Lt, Lt0, ltR,
  lte, Lte, Lte0, lteR,
  not, Not, Not0,
  or, Or, Or0,
  fn,
};
