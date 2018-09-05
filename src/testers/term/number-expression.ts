import { Map } from 'immutable';

import { NumberExpression } from '../../ast';
import { NodeEvaluation } from '../../common/model';
import { SyntaxConfig } from '../../config';
import { Tester } from '../tester';

export class NumberExpressionTester extends Tester<number, NumberExpression, null> {

  constructor(
    public readonly ast: NumberExpression,
    public readonly config: SyntaxConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>) {
    return NodeEvaluation.ofNumber(values, this.ast)(this.ast.preparedValue);
  }

}
