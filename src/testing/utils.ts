import { List, Map } from 'immutable';
import { None } from 'monet';

import { BinaryOperationExpression, DateExpression, FunctionExpression, NotExpression, NumberExpression, PhraseExpression, SelectorExpression, TermExpression, TextExpression } from '../ast/expressions';
import { AndOperator, EqualityOperator, IsNotOperator, IsOperator, LikeOperator, NotOperator, OrOperator } from '../ast/operators';
import { Expression, NodeEvaluation, ValueType } from '../common/model';
import { FunctionConfig, OptionalFunctionArg, ParserConfig } from '../config';

const config = new ParserConfig();
const { AND, IS, IS_NOT, LIKE, NOT, OR } = config;

const And = new AndOperator(AND[1]);
const Is = new IsOperator(IS[1]);
const IsNot = new IsNotOperator(IS_NOT[1]);
const Like = new LikeOperator(LIKE[1]);
const Not = new NotOperator(NOT[1]);
const Or = new OrOperator(OR[1]);
const And0 = new AndOperator(AND[0]);
const Is0 = new IsOperator(IS[1]);
const IsNot0 = new IsNotOperator(IS_NOT[1]);
const Like0 = new LikeOperator(LIKE[0]);
const Not0 = new NotOperator(NOT[0]);
const Or0 = new OrOperator(OR[0]);

const date = (v: string) => DateExpression.of(v);
const num = (v: string) => NumberExpression.of(v);
const phrase = (v: string) => PhraseExpression.of(v);
const sel = (v: string, t: ValueType) => new SelectorExpression(t, v);
const txt = (v: string) => TextExpression.of(v);

const txtFrom = (e: TermExpression) => TextExpression.fromTerm(e);

const and = (l: Expression, r: Expression, op: AndOperator = And) =>
  new BinaryOperationExpression(op, [l, r]);

const eq = (l: TermExpression, r: TermExpression, op: EqualityOperator) =>
  new BinaryOperationExpression(op, [
    TextExpression.of(l.value),
    PhraseExpression.of(r.value),
  ]);

// tslint:disable:no-unnecessary-callback-wrapper
const like = (l: TermExpression, r: TermExpression, op: LikeOperator = Like) => eq(l, r, op);
const is = (l: TermExpression, r: TermExpression, op: IsOperator = Is) => eq(l, r, op);
const isNot = (l: TermExpression, r: TermExpression, op: IsNotOperator = IsNot) => eq(l, r, op);
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
  date, num, phrase, sel, txt, txtFrom,
  and, andNot, And, And0,
  like, Like, Like0,
  is, Is, Is0,
  isNot, IsNot, IsNot0,
  not, Not, Not0,
  or, Or, Or0,
  fn,
};
