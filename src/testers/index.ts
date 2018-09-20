import { List } from 'immutable';

import { BinaryOperationExpression, DateExpression, Expression, FunctionExpression, NotExpression, NumberExpression, PhraseExpression, SelectorExpression, TextExpression } from '../index';

import { BinaryOperationExpressionTester } from './binary-operation';
import { TesterConfig } from './config';
import { FunctionExpressionTester } from './function';
import { DateExpressionTester, NumberExpressionTester, PhraseExpressionTester, TextExpressionTester } from './term';
import { Tester } from './tester';
import { NotExpressionTester } from './unary-operation';

// tslint:disable-next-line:cyclomatic-complexity
Tester.fromAst = (config: TesterConfig) => (ast: Expression) => {
  switch (ast.constructor) {

    case DateExpression:
      return new DateExpressionTester(ast as DateExpression, config);

    case NumberExpression:
      return new NumberExpressionTester(ast as NumberExpression, config);

    case PhraseExpression:
      return new PhraseExpressionTester(ast as PhraseExpression<any>, config);

    case SelectorExpression:
    case TextExpression:
      return new TextExpressionTester(ast as TextExpression, config);

    case BinaryOperationExpression:
      return new BinaryOperationExpressionTester<any, any, any>(
        ast as BinaryOperationExpression,
        List(ast.value).map(Tester.fromAst(config)).toOrderedSet(),
        config);

    case FunctionExpression:
      return new FunctionExpressionTester(
        ast as FunctionExpression,
        ast.value.map(Tester.fromAst(config)),
        config);

    // TODO: UnaryOperationExpression
    case NotExpression:
      return new NotExpressionTester(
        ast as NotExpression,
        Tester.fromAst(config)(ast.value),
        config);

    default: throw Error(`Tester.fromAst: Wrong expression type: ${ast.constructor.name}`);
  }
};

export {
  BinaryOperationExpressionTester,
  FunctionExpressionTester,
  NotExpressionTester,
  Tester,
  TextExpressionTester,
};
