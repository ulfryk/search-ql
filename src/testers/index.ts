import { BasicExpression, Expression, JoinedExpression, LabelledExpression, NotExpression } from '../ast';
import { SyntaxConfig } from '../syntax-config';

import { BasicExpressionTester } from './basic-expression';
import { JoinedExpressionTester } from './joined-expression';
import { LabelledExpressionTester } from './labelled-expression';
import { NotExpressionTester } from './not-expression';
import { Tester } from './tester';

// tslint:disable-next-line:cyclomatic-complexity
Tester.fromAst = (config: SyntaxConfig) => (ast: Expression) => {
  switch (ast.constructor) {

    case BasicExpression:
      return new BasicExpressionTester(ast, config);

    case JoinedExpression:
      return new JoinedExpressionTester(
        ast as any as JoinedExpression,
        ast.value.map(Tester.fromAst(config)),
        config);

    case LabelledExpression:
      return new LabelledExpressionTester(
        ast as any as LabelledExpression,
        Tester.fromAst(config)(ast.value),
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
  BasicExpressionTester,
  JoinedExpressionTester,
  LabelledExpressionTester,
  NotExpressionTester,
  Tester,
};
