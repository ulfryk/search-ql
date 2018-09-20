import { Map } from 'immutable';

import { TextExpression } from '../../index';

import { TesterConfig } from '../config';
import { NodeEvaluation } from '../model';
import { Tester } from '../tester';

export class TextExpressionTester extends Tester<string, TextExpression, null> {

  constructor(
    public readonly ast: TextExpression,
    public readonly config: TesterConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>) {
    return NodeEvaluation.ofText(values, this.ast)(this.ast.value);
  }

}
