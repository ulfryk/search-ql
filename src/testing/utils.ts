import { List, Map } from 'immutable';
import { None } from 'monet';

import { BinaryOperationExpression, DateExpression, FunctionExpression, NotExpression, NumberExpression, PhraseExpression, TermExpression, TextExpression } from '../ast/expressions';
import { AndOperator, LikeOperator, NotOperator, OrOperator } from '../ast/operators';
import { Expression, NodeEvaluation, ValueType } from '../common/model';
import { FunctionConfig, OptionalFunctionArg, SyntaxConfig } from '../config';

const config = new SyntaxConfig();
const { AND, LIKE, NOT, OR } = config;

const And = new AndOperator(AND[1]);
const Like = new LikeOperator(LIKE[1]);
const Not = new NotOperator(NOT[1]);
const Or = new OrOperator(OR[1]);
const And0 = new AndOperator(AND[0]);
const Like0 = new LikeOperator(LIKE[0]);
const Not0 = new NotOperator(NOT[0]);
const Or0 = new OrOperator(OR[0]);

const date = (v: string) => DateExpression.of(v);
const num = (v: string) => NumberExpression.of(v);
const phrase = (v: string) => PhraseExpression.of(v);
const txt = (v: string) => TextExpression.of(v);

const txtFrom = (e: TermExpression) => TextExpression.fromTerm(e);

const and = (l: Expression, r: Expression, op: AndOperator = And) =>
  new BinaryOperationExpression(op, [l, r]);

const like = (l: TermExpression, r: TermExpression, op: LikeOperator = Like) =>
  new BinaryOperationExpression(op, [
    TextExpression.of(l.value),
    PhraseExpression.of(r.value),
  ]);

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
  date, num, phrase, txt, txtFrom,
  and, andNot, And, And0,
  like, Like, Like0,
  not, Not, Not0,
  or, Or, Or0,
  fn,
};
