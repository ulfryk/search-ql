import { BinaryOperationExpression, DateExpression, Expression, NotExpression, NumberExpression, SelectorExpression, TextExpression } from '../ast';
import { SyntaxConfig } from '../config';

import { BinaryOperationExpressionTester } from './binary-operation-expression';
import { NotExpressionTester } from './not-expression';
import { DateExpressionTester, NumberExpressionTester, SelectorExpressionTester, TextExpressionTester } from './term';
import { Tester } from './tester';

// tslint:disable-next-line:cyclomatic-complexity
Tester.fromAst = (config: SyntaxConfig) => (ast: Expression) => {
  switch (ast.constructor) {

    case DateExpression:
      return new DateExpressionTester(ast as DateExpression, config);

    case NumberExpression:
      return new NumberExpressionTester(ast as NumberExpression, config);

    case SelectorExpression:
      return new SelectorExpressionTester(ast as SelectorExpression, config);

    case TextExpression:
      return new TextExpressionTester(ast as TextExpression, config);

    case BinaryOperationExpression:
      return new BinaryOperationExpressionTester(
        ast as BinaryOperationExpression,
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
  TextExpressionTester,
  BinaryOperationExpressionTester,
  NotExpressionTester,
  Tester,
};
