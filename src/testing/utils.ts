import { BinaryOperationExpression, DateExpression, Expression, FunctionExpression, NotExpression, NumberExpression, SelectorExpression, TermExpression, TextExpression } from '../ast/expressions';
import { AndOperator, LikeOperator, NotOperator, OrOperator } from '../ast/operators';
import { ValueType } from '../common/model';
import { SyntaxConfig } from '../config';

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

const date = (v: string) => new DateExpression(v);
const num = (v: string) => new NumberExpression(v);
const term = (v: string) => new TermExpression(v, v);
const txt = (v: string) => new TextExpression(v);

const txtFrom = (e: TermExpression) => TextExpression.fromTerm(e);

const and = (l: Expression, r: Expression, op: AndOperator = And) =>
  new BinaryOperationExpression(op, [l, r]);

const like = (l: TermExpression, r: TermExpression, op: LikeOperator = Like) =>
  new BinaryOperationExpression(op, [
    SelectorExpression.fromTerm(l),
    TextExpression.fromTerm(r),
  ]);

const or = (l: Expression, r: Expression, op: OrOperator = Or) =>
  new BinaryOperationExpression(op, [l, r]);

const not = (v: Expression) => new NotExpression(v);

const andNot = (l: Expression, r: Expression, op: AndOperator = And0) =>
  and(l, not(r), op);

const fn = (name: string, returnType = ValueType.Boolean) => (...args: Expression[]) =>
  FunctionExpression.fromParseResult(returnType)(name, args);

export {
  config,
  date, num, term, txt, txtFrom,
  and, andNot, And, And0,
  like, Like, Like0,
  not, Not, Not0,
  or, Or, Or0,
  fn,
};
