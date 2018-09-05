import { Map } from 'immutable';

import { DateExpression } from '../../ast';
import { NodeEvaluation } from '../../common/model';
import { SyntaxConfig } from '../../config';
import { Tester } from '../tester';

export class DateExpressionTester extends Tester<number, DateExpression, null> {

  constructor(
    public readonly ast: DateExpression,
    public readonly config: SyntaxConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>) {
    return NodeEvaluation.ofDate(values, this.ast)(this.ast.preparedValue);
  }

}
