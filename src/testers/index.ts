import { BinaryOperationExpression, DateExpression, FunctionExpression, NotExpression, NumberExpression, PhraseExpression, TextExpression } from '../ast';
import { ParserConfig } from '../config';

import { Expression } from '../common/model';
import { BinaryOperationExpressionTester } from './binary-operation';
import { FunctionExpressionTester } from './function';
import { DateExpressionTester, NumberExpressionTester, PhraseExpressionTester, TextExpressionTester } from './term';
import { Tester } from './tester';
import { NotExpressionTester } from './unary-operation';

// tslint:disable-next-line:cyclomatic-complexity
Tester.fromAst = (config: ParserConfig) => (ast: Expression) => {
  switch (ast.constructor) {

    case DateExpression:
      return new DateExpressionTester(ast as DateExpression, config);

    case NumberExpression:
      return new NumberExpressionTester(ast as NumberExpression, config);

    case PhraseExpression:
      return new PhraseExpressionTester(ast as PhraseExpression, config);

    case TextExpression:
      return new TextExpressionTester(ast as TextExpression, config);

    case BinaryOperationExpression:
      return new BinaryOperationExpressionTester<any, any, any>(
        ast as BinaryOperationExpression,
        ast.value.map(Tester.fromAst(config)),
        config);

    case FunctionExpression:
      return new FunctionExpressionTester(
        ast as FunctionExpression<any>,
        ast.value.map(Tester.fromAst(config)),
        config);

    // TODO: UnaryOperationExpression
    case NotExpression:
      return new NotExpressionTester(
        ast as NotExpression,
        Tester.fromAst(config)(ast.value),
        config);

    default: throw Error('wrong expression type');
  }
};

export {
  BinaryOperationExpressionTester,
  FunctionExpressionTester,
  NotExpressionTester,
  Tester,
  TextExpressionTester,
};
