import { BinaryOperationExpression, Expression, NotExpression, TextExpression } from '../ast';
import { SyntaxConfig } from '../config';

import { BinaryOperationExpressionTester } from './binary-operation-expression';
import { NotExpressionTester } from './not-expression';
import { Tester } from './tester';
import { TextExpressionTester } from './text-expression';

// tslint:disable-next-line:cyclomatic-complexity
Tester.fromAst = (config: SyntaxConfig) => (ast: Expression) => {
  switch (ast.constructor) {

    case TextExpression:
      return new TextExpressionTester(ast, config);

    case BinaryOperationExpression:
      return new BinaryOperationExpressionTester(
        ast as any as BinaryOperationExpression,
        ast.value.map(Tester.fromAst(config)),
        config);

    case NotExpression:
      return new NotExpressionTester(
        ast as any as NotExpression,
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
